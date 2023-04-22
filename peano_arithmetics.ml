type nat = Zero | Succ of nat

let rec int_to_nat a = match a with
  |0 -> Zero
  |a -> Succ (int_to_nat (a - 1));;

let rec nat_to_int a = match a with
  |Zero -> 0
  |Succ a -> 1 + nat_to_int a;;

let rec add a b = match a, b with
  |Zero, _ -> b
  |Succ a, b -> Succ (add a b);;


let rec mul a b = match a, b with
  |Zero, _ -> Zero
  |Succ a, b -> add b (mul a b)

let rec pow n m =
  match m with
  |_, Zero -> Succ Zero (*1*)
  |_, Succ m' -> mul n (pow n m')

let rec leq n m =
  match n, m with
  | Zero, _ -> true
  | _, Zero -> false
  | Succ n', Succ m' -> leq n' m'