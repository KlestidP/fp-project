# tinyfp

a tiny untyped functional language in one haskell file. it has lambdas, numbers, and pairs (cons cells), and the evaluator is lazy (call-by-need).

## build + run (windows powershell)
- compile:
  - `ghc -O2 Main.hs -o tinyfp`
- run:
  - `.\tinyfp.exe`

it prints the results of a few example programs included in `Main.hs`.

## language syntax (quick)

### basics
- numbers: `123`, `-5`
- names: `x`, `foo`, `_tmp`
- lambda: `\x -> expr`
- let: `let x = e1 in e2`
- letrec (recursive): `letrec f = e1 in e2`

### application (important)
application is **only** written as an s-expression:
- `(f a b)` means `((f a) b)`
- so write `(fact 6)` not `fact 6`

### pairs / lists
- `nil` is empty list
- `(cons a b)` makes a pair
- `(car p)` gets first
- `(cdr p)` gets second

lists are just nested `cons` ending in `nil`.

### builtins
- arithmetic: `(+ a b)`, `(- a b)`, `(* a b)`
- equality (numbers): `(== a b)` returns `1` if equal else `0`
- branching: `(ifz c t e)` picks `t` when `c` is `0`, else `e`
- predicates:
  - `(isnil x)` returns `0` if `x` is `nil`, else `1`
  - `(ispair x)` returns `0` if `x` is a pair, else `1`

## laziness / semantics
the evaluator is call-by-need:
- function arguments and `let` bindings are stored as thunks
- thunks are only evaluated when needed (`force`)
- once a thunk is forced, it is updated in the heap to a value (so it runs once)

the program prints a `Force-count` number which is basically “how many thunks were actually evaluated”.

note: for nicer output, printing a list forces up to 50 cons cells of the list spine.

## included examples
these run automatically in `main`:

- `factorial` -> computes `6! = 720`
- `mergeSort` -> sorts a small list
- `sum-list` -> sums `[1..5]`
- `reverse-list` -> reverses a list using `append`
- `lazy-terminates` -> would not terminate with strict evaluation, but returns `1` here
- `call-by-need-linear` -> would be exponential without thunk updating, but runs fine here
- `infinite-ones-take` -> builds an infinite list of ones and takes the first 8
