{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Map.Strict as M


--parser combinator

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, rest) <- p s
    pure (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser pf) <*> (Parser pa) = Parser $ \s -> do
    (f, s1) <- pf s
    (a, s2) <- pa s1
    pure (f a, s2)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \s -> do
    (a, s1) <- pa s
    runParser (f a) s1

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \case
  (c:cs) | pred c -> Just (c, cs)
  _               -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)

stringP :: String -> Parser String
stringP = traverse charP

ws :: Parser ()
ws = many (satisfy isSpace) *> pure ()

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

symbol :: String -> Parser String
symbol = lexeme . stringP

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

reserved :: [String]
reserved =
  [ "let", "in", "letrec"
  , "nil", "cons", "car", "cdr"
  , "ifz", "isnil", "ispair"
  ]

ident :: Parser String
ident = lexeme $ do
  c  <- satisfy isAlpha <|> charP '_'
  cs <- many (satisfy isAlphaNum <|> charP '_' <|> charP '\'')
  let name = c:cs
  if name `elem` reserved then empty else pure name

integer :: Parser Integer
integer = lexeme $ do
  sign <- optional (charP '-')
  ds <- some (satisfy isDigit)
  let n = read ds
  pure $ case sign of
    Just _  -> -n
    Nothing -> n

eof :: Parser ()
eof = Parser $ \s ->
  if all isSpace s then Just ((), "") else Nothing

parseAll :: Parser a -> String -> Either String a
parseAll p s =
  case runParser (ws *> p <* ws <* eof) s of
    Just (a, _) -> Right a
    Nothing     -> Left "Parse error"


--AST

type Name = String

data Expr
  = EVar Name
  | ENum Integer
  | ELam Name Expr
  | EApp Expr Expr
  | ELet Name Expr Expr
  | ELetRec Name Expr Expr
  | ENil
  | ECons Expr Expr
  | ECar Expr
  | ECdr Expr
  | EIfZ Expr Expr Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EEq  Expr Expr
  | EIsNil Expr
  | EIsPair Expr
  deriving (Show, Eq)


--expression parser
--application is explicit as an s-expression: (f a b)  ==> (((f a) b))

expr :: Parser Expr
expr = lam <|> letrec <|> let_ <|> atom
  where
    lam = lexeme $ do
      _ <- charP '\\'
      ws
      x <- ident
      ws
      _ <- stringP "->"
      ws
      body <- expr
      pure (ELam x body)

    let_ = lexeme $ do
      _ <- stringP "let"
      ws
      x <- ident
      ws
      _ <- charP '='
      ws
      e1 <- expr
      ws
      _ <- stringP "in"
      ws
      e2 <- expr
      pure (ELet x e1 e2)

    letrec = lexeme $ do
      _ <- stringP "letrec"
      ws
      x <- ident
      ws
      _ <- charP '='
      ws
      e1 <- expr
      ws
      _ <- stringP "in"
      ws
      e2 <- expr
      pure (ELetRec x e1 e2)

atom :: Parser Expr
atom =
      (ENum <$> integer)
  <|> (ENil <$ symbol "nil")
  <|> (EVar <$> ident)
  <|> parens insideParens

insideParens :: Parser Expr
insideParens =
      consP
  <|> carP
  <|> cdrP
  <|> ifzP
  <|> addP
  <|> subP
  <|> mulP
  <|> eqP
  <|> isnilP
  <|> ispairP
  <|> appList
  where
    consP   = do _ <- symbol "cons";   a <- expr; b <- expr; pure (ECons a b)
    carP    = do _ <- symbol "car";    p <- expr; pure (ECar p)
    cdrP    = do _ <- symbol "cdr";    p <- expr; pure (ECdr p)
    ifzP    = do _ <- symbol "ifz";    c <- expr; t <- expr; e <- expr; pure (EIfZ c t e)
    addP    = do _ <- symbol "+";      a <- expr; b <- expr; pure (EAdd a b)
    subP    = do _ <- symbol "-";      a <- expr; b <- expr; pure (ESub a b)
    mulP    = do _ <- symbol "*";      a <- expr; b <- expr; pure (EMul a b)
    eqP     = do _ <- symbol "==";     a <- expr; b <- expr; pure (EEq a b)
    isnilP  = do _ <- symbol "isnil";  x <- expr; pure (EIsNil x)
    ispairP = do _ <- symbol "ispair"; x <- expr; pure (EIsPair x)

    --general application: (f a b c) -> (((f a) b) c)
    appList = do
      es <- some expr
      pure (foldl1 EApp es)


--lazy eval (call-by-need) with thunk update

type Addr = Int
type Env  = M.Map Name Addr

data Value
  = VNum Integer
  | VNil
  | VPair Addr Addr
  | VClosure Name Expr Env
  deriving (Show)

data Cell
  = Thunk Expr Env
  | Val Value
  deriving (Show)

data St = St
  { heap   :: M.Map Addr Cell
  , nextA  :: Addr
  , forces :: Int
  } deriving (Show)

type EvalM a = StateT St (Either String) a

fresh :: EvalM Addr
fresh = do
  st <- get
  let a = nextA st
  put st { nextA = a + 1 }
  pure a

heapRead :: Addr -> EvalM Cell
heapRead a = do
  st <- get
  case M.lookup a (heap st) of
    Just c  -> pure c
    Nothing -> lift (Left ("Invalid address: " ++ show a))

heapWrite :: Addr -> Cell -> EvalM ()
heapWrite a c = do
  st <- get
  put st { heap = M.insert a c (heap st) }

allocVal :: Value -> EvalM Addr
allocVal v = do
  a <- fresh
  heapWrite a (Val v)
  pure a

allocThunk :: Expr -> Env -> EvalM Addr
allocThunk e env = do
  a <- fresh
  heapWrite a (Thunk e env)
  pure a

--call-by-need force:eval thunk once,update to Val
force :: Addr -> EvalM Value
force a = heapRead a >>= \case
  Val v -> pure v
  Thunk e env -> do
    modify' (\st -> st { forces = forces st + 1 })
    vAddr <- eval env e
    v <- force vAddr
    heapWrite a (Val v)   --memoize
    pure v

--eval to an address (do not force unless needed)
eval :: Env -> Expr -> EvalM Addr
eval env = \case
  EVar x ->
    case M.lookup x env of
      Just a  -> pure a
      Nothing -> lift (Left ("Unbound variable: " ++ x))

  ENum n -> allocVal (VNum n)
  ENil   -> allocVal VNil
  ELam x body -> allocVal (VClosure x body env)

  EApp f a -> do
    fA <- eval env f
    force fA >>= \case
      VClosure x body cloEnv -> do
        argA <- allocThunk a env
        let env' = M.insert x argA cloEnv
        eval env' body
      other -> lift (Left ("Tried to apply non-function: " ++ show other))

  ELet x e1 e2 -> do
    a1 <- allocThunk e1 env
    eval (M.insert x a1 env) e2

  ELetRec x e1 e2 -> do
    a <- fresh
    let env' = M.insert x a env
    heapWrite a (Thunk e1 env') --self-reference
    eval env' e2

  ECons h t -> do
    ha <- allocThunk h env
    ta <- allocThunk t env
    allocVal (VPair ha ta)

  ECar p -> do
    pa <- eval env p
    force pa >>= \case
      VPair ha _ -> pure ha
      other      -> lift (Left ("car on non-pair: " ++ show other))

  ECdr p -> do
    pa <- eval env p
    force pa >>= \case
      VPair _ ta -> pure ta
      other      -> lift (Left ("cdr on non-pair: " ++ show other))

  EIsNil x -> do
    xa <- eval env x
    force xa >>= \case
      VNil -> allocVal (VNum 0)  --true
      _    -> allocVal (VNum 1)  --false

  EIsPair x -> do
    xa <- eval env x
    force xa >>= \case
      VPair _ _ -> allocVal (VNum 0) --true
      _         -> allocVal (VNum 1) --false



  EIfZ c t e -> do
    ca <- eval env c
    force ca >>= \case
      VNum 0 -> eval env t
      VNum _ -> eval env e
      other  -> lift (Left ("ifz condition not a number: " ++ show other))

  EAdd a b -> binNum env (+) a b
  ESub a b -> binNum env (-) a b
  EMul a b -> binNum env (*) a b

  EEq a b -> do
    na <- eval env a >>= force
    nb <- eval env b >>= force
    case (na, nb) of
      (VNum x, VNum y) -> allocVal (VNum (if x == y then 1 else 0))
      _ -> lift (Left "== expects two numbers")

binNum :: Env -> (Integer -> Integer -> Integer) -> Expr -> Expr -> EvalM Addr
binNum env op a b = do
  va <- eval env a >>= force
  vb <- eval env b >>= force
  case (va, vb) of
    (VNum x, VNum y) -> allocVal (VNum (op x y))
    _ -> lift (Left "Arithmetic expects numbers")


--pretty-printing results(numbers + lists)

showAny :: St -> Addr -> String
showAny st a =
  case M.lookup a (heap st) of
    Just (Val v)      -> showValue st v
    Just (Thunk _ _)  -> "<thunk>"
    Nothing           -> "<bad-addr>"

showValue :: St -> Value -> String
showValue st = \case
  VNum n -> show n
  VNil   -> "nil"
  VClosure{} -> "<closure>"
  VPair ha ta ->
    let (isListLike, items, _) = collectList st ha ta
    in if isListLike
       then "[" ++ unwords items ++ "]"
       else "(" ++ showAny st ha ++ " . " ++ showAny st ta ++ ")"

collectList :: St -> Addr -> Addr -> (Bool, [String], String)
collectList st ha ta =
  let headStr = showAny st ha
      go acc addr =
        case M.lookup addr (heap st) of
          Just (Val VNil) -> (True, reverse acc, "")
          Just (Val (VPair h t)) ->
            let s = showAny st h
            in go (s:acc) t
          Just (Val _) -> (False, reverse acc, showAny st addr)
          Just (Thunk _ _) -> (False, reverse acc, "<thunk>")
          Nothing -> (False, reverse acc, "<bad-addr>")
  in case M.lookup ta (heap st) of
       Just (Val VNil) -> (True, [headStr], "")
       _ ->
         let (ok, rest, tailStr) = go [] ta
         in if ok
            then (True, headStr : rest, "")
            else (False, [headStr], tailStr)

forceListSpine :: Int -> Addr -> EvalM ()
forceListSpine 0 _ = pure ()
forceListSpine n a = do
  v <- force a
  case v of
    VPair ha ta -> do
      _ <- force ha               --force head value for printing
      forceListSpine (n - 1) ta   --force tail spine
    _ -> pure ()




--runner


runProgram :: String -> Either String (Value, Int, St)
runProgram src = do
  ast <- parseAll expr src
  let st0 = St { heap = M.empty, nextA = 0, forces = 0 }
  (addr, st1) <- runStateT (eval M.empty ast) st0
  (v, st2)    <- runStateT (force addr) st1
--force some list spine so printing works (cap 50)
  (_, st3)    <- runStateT (forceListSpine 50 addr) st2
  pure (v, forces st3, st3)



--example programs


--factorial (6! = 720)
progFactorial :: String
progFactorial =
  "letrec fact = \\n -> (ifz n 1 (* n (fact (- n 1)))) in (fact 6)"

--laziness demo: strict would diverge, lazy returns 1
progLazyTerminates :: String
progLazyTerminates =
  "((\\x -> 1) (letrec loop = loop in loop))"

--call-by-need memoization demo:
--without thunk updates this is exponential, with updates it is linear.
--f 20 = 2^20 = 1048576
progNeedLinear :: String
progNeedLinear =
  "letrec f = \\n -> (ifz n 1 (let x = (f (- n 1)) in (+ x x))) in (f 20)"

--infinite list of ones; take first 8(shows laziness on lists)
progTakeOnes :: String
progTakeOnes = unlines
  [ "letrec ones = (cons 1 ones) in"
  , "letrec take = \\n -> \\xs ->"
  , "  (ifz n nil (cons (car xs) (take (- n 1) (cdr xs))))"
  , "in"
  , "(take 8 ones)"
  ]

progMergeSort :: String
progMergeSort = unlines
  [ "letrec lt = \\a -> \\b ->"
  , "  (ifz a (ifz b 1 0) (ifz b 1 (lt (- a 1) (- b 1))))"
  , "in"
  , "letrec split = \\xs ->"
  , "  (ifz (isnil xs)"
  , "      (cons nil nil)"
  , "      (ifz (isnil (cdr xs))"
  , "          (cons (cons (car xs) nil) nil)"
  , "          (let p = (split (cdr (cdr xs))) in"
  , "             (cons (cons (car xs) (car p))"
  , "                   (cons (car (cdr xs)) (cdr p))))))"
  , "in"
  , "letrec merge = \\xs -> \\ys ->"
  , "  (ifz (isnil xs)"
  , "      ys"
  , "      (ifz (isnil ys)"
  , "          xs"
  , "          (ifz (lt (car xs) (car ys))"
  , "              (cons (car xs) (merge (cdr xs) ys))"
  , "              (cons (car ys) (merge xs (cdr ys))))))"
  , "in"
  , "letrec msort = \\xs ->"
  , "  (ifz (isnil xs)"
  , "      nil"
  , "      (ifz (isnil (cdr xs))"
  , "          xs"
  , "          (let p = (split xs) in"
  , "             (merge (msort (car p)) (msort (cdr p))))))" 
  , "in"
  , "(msort (cons 5 (cons 1 (cons 4 (cons 2 (cons 3 nil))))))"
  ]

--extra 1) Sum of a list: sum [1..5] = 15
progSumList :: String
progSumList = unlines
  [ "letrec sum = \\xs ->"
  , "  (ifz (isnil xs)"
  , "      0"
  , "      (+ (car xs) (sum (cdr xs))))"
  , "in"
  , "(sum (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))))"
  ]

--extra 2) Reverse a list using append: reverse [1 2 3 4] = [4 3 2 1]
progReverseList :: String
progReverseList = unlines
  [ "letrec append = \\xs -> \\ys ->"
  , "  (ifz (isnil xs)"
  , "      ys"
  , "      (cons (car xs) (append (cdr xs) ys)))"
  , "in"
  , "letrec rev = \\xs ->"
  , "  (ifz (isnil xs)"
  , "      nil"
  , "      (append (rev (cdr xs)) (cons (car xs) nil)))"
  , "in"
  , "(rev (cons 1 (cons 2 (cons 3 (cons 4 nil)))))"
  ]



examples :: [(String, String)]
examples =
  [ ("factorial", progFactorial)
  , ("mergeSort", progMergeSort)
  , ("sum-list", progSumList)
  , ("reverse-list", progReverseList)
  , ("lazy-terminates", progLazyTerminates)
  , ("call-by-need-linear", progNeedLinear)
  , ("infinite-ones-take", progTakeOnes)
  ]


--main

main :: IO ()
main = do
  putStrLn "Tiny Lazy FP Language (call-by-need) demo\n"
  mapM_ runOne examples
  where
    runOne (name, src) = do
      putStrLn ("== " ++ name ++ " ==")
      case runProgram src of
        Left err -> do
          putStrLn ("Error: " ++ err)
          putStrLn ("Source was:\n" ++ src ++ "\n")
        Right (v, fc, st) -> do
          putStrLn ("Result: " ++ showValue st v)
          putStrLn ("Force-count (thunk evaluations): " ++ show fc)
          putStrLn ""
