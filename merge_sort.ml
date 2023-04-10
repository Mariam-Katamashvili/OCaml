(*Write a function to sort a collection of integers using the merge sort.*)
let rec merge l1 l2 = match l1, l2 with
  |[], l2 -> l2
  |l1, [] -> l1
  |h1::tail1, h2::tail2 -> if (h1 > h2) then h2::(merge tail2 (h1::tail1)) else h1::(merge tail1 (h2::tail2));;
             
let rec list_in_3_parts lst = match List.rev lst with
  |[] -> ([], [], [])
  |[x] -> ([x], [], [])
  |last::rev_middle -> let middle =  List.rev rev_middle in
    match middle with
      |head::tail -> ([head], tail,[ last])
      |[] -> ([], [], []);;
               
let rec split lst = match lst with
  |[] -> [], []
  |[x] -> [x], []
  |[x; y] -> [x], [y]
  |h::tail -> let (head, middle, last) = list_in_3_parts lst in
    ([h] @ fst (split middle), snd (split middle) @ last);; 

let rec mergesort lst= match lst,split lst with
  |[],_->[]
  |[x],_->[x]
  |lst,(lst1,lst2)-> merge (mergesort lst1) (mergesort lst2);;
  