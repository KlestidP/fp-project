# tinyfp

## build + run
- compile:
  - `ghc -O2 Main.hs -o tinyfp`
- run:
  - `.\tinyfp.exe`

## syntax
- numbers: `123`, `-5`
- names: `x`, `foo`, `_tmp`
- lambda: `\x -> expr`
- let: `let x = e1 in e2`
- letrec: `letrec f = e1 in e2`
- application (only s-exp): `(f a b)` means `((f a) b)`
- pairs/lists: `nil`, `(cons a b)`, `(car p)`, `(cdr p)`
- math: `(+ a b)`, `(- a b)`, `(* a b)`, `(== a b)`
- branch: `(ifz c t e)` picks `t` when `c` is `0`
- preds: `(isnil x)`, `(ispair x)` return `0` for true, `1` for false

## laziness
call-by-need: args are stored as thunks, forced only when needed, and memoized (forced once).
