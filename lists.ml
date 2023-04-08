(*List length*)
let rec len a = match a with
  |[] -> 0
  |h::tail -> 1 + len tail;;

(*Concatenate 2 lists*)
let rec conc l1 l2 = match l1 with 
  |[] -> l2
  |h::tail -> h :: conc tail l2;;

(*Power*)
let rec power a b = match b with
  |0 -> 1
  |_ -> a * (power a (b - 1));;

(*List to Int*)
let rec list_to_int l1 = match l1 with
|[] -> 0
|[a] -> a
|h::tail -> h * (power 10 (len(l1) - 1)) + list_to_int tail;;

(*Int to List*)
let rec int_to_list a = if a mod 10 = a then [a] 
else int_to_list(a / 10) @ [a mod 10];;

(*Reversing the list*)
let rec list_reverse l1 = match l1 with
  |[] -> l1
  |h::tail -> list_reverse (tail) @ [h];;

(*nth element of the list*)
let rec nth_element l1 a = match l1 with
  |[] -> 0
  |h::tail -> if a = 0 then h else nth_element tail (a - 1);; 

(*Fibonacci*)
let rec fib a = match a with 
  |0 -> []
  |1 -> [1]
  |2 -> [1; 1]
  |_ -> fib (a-1) @ [nth_element (fib (a-1)) (a-2) + nth_element (fib (a-2)) (a-3)];; 
  (*We also could have reversed the list and then we could have taken first and second elements from it insted of taking nth element*)
