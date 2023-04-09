open Float

let rec pow a n = match n with 0 -> 1 | 1 -> a | n -> let b = pow a (n / 2) in b * b * (if n mod 2 = 0 then 1 else a);;
let root_mean x y = sqrt( ((x *. x) +. (y *. y)) /. 2. )