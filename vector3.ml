(* Define a suitable data type for your point.*)
type vector3 = (int * int * int);;

(*Define three points p1, p2 and p3 with different values.*)
let p1 = (1, 2, 3);;
let p2 = (2, 3, 4);;
let p3 = (3, 4, 5);;

(*Implement a function vector3_to_string : vector3 -> string to convert a vector into a human-readable representation.*)
let vector3_to_string v = match v with
  |(v1, v2, v3) -> ("(" ^ Int.to_string v1 ^ ", " ^ Int.to_string v2 ^ ", " ^ Int.to_string v3 ^ ")");;

(*Write a function vector3_add : vector3 -> vector3 -> vector3 to add two vectors.*)
let vector3_add (v1, v2, v3) (u1, u2, u3) = (v1 + u1, v2 + u2, v3 + u3);;

(*Write a function vector3_max : vector3 -> vector3 -> vector3 that returns the larger vector.*)
let vector3_max (v1, v2, v3) (u1, u2, u3) = if sqrt(float (v1 * v1 + v2 * v2 + v3 * v3)) > sqrt(float (u1 * u1 + u2 * u2 + u3 * u3)) 
then (v1, v2, v3) else (u1, u2, u3);;

(*Compute the result of adding p1 to the larger of p2 and p3 and print the result as a string.*)(*todo*)
let final = vector3_to_string(vector3_add p1 (vector3_max p2 p3));;