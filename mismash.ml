(*Implement a function interleave3 : 'a list -> 'a list -> 'a list -> 'a list that interleaves three lists. 
So for input lists [0;1;2], [10;11;12] and [20;21;22] the function must return the list [0;10;20;1;11;21;2;12;22]. 
If a list is out of elements (e.g. for inputs of different lengths), the function continues interleaving the remaining lists, 
such that for inputs ['a';'b'], ['A';'B';'C';'D'] and ['!'] the output ['a';'A';'!';'b';'B';'C';'D'] is produced.*)

let rec interleave2 l1 l2 = match l1, l2 with
  |[], l2 -> l2
  |l1, [] -> l1
  |h1::tail1, h2::tail2 -> h1::h2::interleave2 tail1 tail2;;

let rec interleave3 l1 l2 l3 = match l1, l2, l3 with
  |[], l2, l3 -> interleave2 l2 l3
  |l1, [], l3 -> interleave2 l1 l3
  |l1, l2, [] -> interleave2 l1 l2
  |h1::tail1, h2::tail2, h3::tail3 -> h1::h2::h3::interleave3 tail1 tail2 tail3;;