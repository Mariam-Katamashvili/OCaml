<<<<<<< HEAD
(*Write a function to sort a collection of integers using the merge sort.*)

(*First, we are writing a function to sort and merge two lists into one.*)
let rec merge l1 l2 = match l1, l2 with
  |[], l2 -> l2
  |l1, [] -> l1
  |h1::tail1, h2::tail2 -> if (h1 > h2) then h2::(merge tail2 (h1::tail1)) else h1::(merge tail1 (h2::tail2));;
           
(*Then we need no write a function which will split list into 2 new lists, but for that i am using additional function
which splits list into head, middle and tail*)
let rec list_in_3_parts lst = match List.rev lst with
  |[] -> ([], [], [])
  |[x] -> ([x], [], [])
  |last::rev_middle -> let middle =  List.rev rev_middle in
    match middle with
      |head::tail -> ([head], tail,[ last])
      |[] -> ([], [], []);;
               
(*Here we are just splitting list*)
(*We take one list and split it into two. We recursively take first element and add it in the first list and also we take last element and add it in the second list. Then we call recursion for middle part, that's why we needed to split list into head middle and last *)
let rec split lst = match lst with
  |[] -> [], []
  |[x] -> [x], []
  |[x; y] -> [x], [y]
  |h::tail -> let (head, middle, last) = list_in_3_parts lst in
    ([h] @ fst (split middle), snd (split middle) @ last);; 

(*split, merge, sort - mergesort*)
let rec mergesort lst= match lst,split lst with
  |[],_->[] (*This line defines the first case of the match expression. If the input list is empty (i.e., lst is []) and the split result is any pair of lists (represented by the underscore _), then the function returns an empty list ([]).*)
  |[x],_->[x] (*This line defines the second case of the match expression. If the input list contains only one element (i.e., lst is [x]) and the split result is any pair of lists, then the function returns the same list ([x]). *)
  |lst,(lst1,lst2)-> merge (mergesort lst1) (mergesort lst2);;
  (*If the input list contains more than one element (i.e., lst is any list with length greater than 1) and the split result is a pair of two sublists lst1 and lst2,
   then the function recursively calls mergesort on lst1 and lst2, and then merges the results using the merge function. 
  The merge function is not definedin this code snippet, but we can assume that it takes two sorted lists and merges them into a single sorted list.*)
=======
(*Write a function to sort a collection of integers using the merge sort.*)

(*First, we are writing a function to sort and merge two lists into one.*)
let rec merge l1 l2 = match l1, l2 with
  |[], l2 -> l2
  |l1, [] -> l1
  |h1::tail1, h2::tail2 -> if (h1 > h2) then h2::(merge tail2 (h1::tail1)) else h1::(merge tail1 (h2::tail2));;
           
(*Then we need no write a function which will split list into 2 new lists, but for that i am using additional function
which splits list into head, middle and tail*)
let rec list_in_3_parts lst = match List.rev lst with
  |[] -> ([], [], [])
  |[x] -> ([x], [], [])
  |last::rev_middle -> let middle =  List.rev rev_middle in
    match middle with
      |head::tail -> ([head], tail,[ last])
      |[] -> ([], [], []);;
               
(*Here we are just splitting list*)
(*We take one list and split it into two. We recursively take first element and add it in the first list and also we take last element and add it in the second list. Then we call recursion for middle part, that's why we needed to split list into head middle and last *)
let rec split lst = match lst with
  |[] -> [], []
  |[x] -> [x], []
  |[x; y] -> [x], [y]
  |h::tail -> let (head, middle, last) = list_in_3_parts lst in
    ([h] @ fst (split middle), snd (split middle) @ last);; 

(*split, merge, sort - mergesort*)
let rec mergesort lst= match lst,split lst with
  |[],_->[] (*This line defines the first case of the match expression. If the input list is empty (i.e., lst is []) and the split result is any pair of lists (represented by the underscore _), then the function returns an empty list ([]).*)
  |[x],_->[x] (*This line defines the second case of the match expression. If the input list contains only one element (i.e., lst is [x]) and the split result is any pair of lists, then the function returns the same list ([x]). *)
  |lst,(lst1,lst2)-> merge (mergesort lst1) (mergesort lst2);;
  (*If the input list contains more than one element (i.e., lst is any list with length greater than 1) and the split result is a pair of two sublists lst1 and lst2,
   then the function recursively calls mergesort on lst1 and lst2, and then merges the results using the merge function. 
  The merge function is not definedin this code snippet, but we can assume that it takes two sorted lists and merges them into a single sorted list.*)
>>>>>>> 6188b496aac41afe034c0764e8c227c0e5369676
