(*1*)
let rec member c t l = match l with
  |[] -> false
  |h::tail -> if c t h = 0 then true else member c t tail;;

let equal_second_components (_, x) (_, y) = compare x y;; 

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2);;

(*2*)
let rec counter x lst = match lst with
  |[] -> 0
  |h::tail -> if x = h then 1 + counter x tail else counter x tail;;

let rec max_elem lst =  match lst with
  |[] -> 0
  |[(_, x)] -> x
  |(_, a)::tail -> 
    let max_in_tail = max_elem tail in 
    if a > max_in_tail then a else max_in_tail;;

let rec remove x lst = match lst with
  |[] -> []
  |h::tail -> if x = h then remove x tail else h:: remove x tail;;

let compare_second_elem (_, a) (_, b) = compare b a;;
let sort = List.sort compare_second_elem;;

let rec count_occurrences list1 = match list1 with
  |[] -> []
  |[x] -> [(x, 1)]
  |h::tail -> 
    let filtered_tail = remove h tail in
    sort ((h, counter h list1) :: count_occurrences filtered_tail);;

(*3*)    
let rec drop_last lst = match List.rev lst with
  |[] -> []
  |[x] -> []
  |h::tail -> List.rev tail;;

(*4*)
let rec drop_last_opt lst = match List.rev lst with
  |[] -> None
  |[x] -> Some []
  |h::tail -> Some (List.rev tail);;

(*5*)
let rec zip_with f lst1 lst2 = match lst1, lst2 with 
  |[], [] -> []
  |[], lst2 -> []
  |lst1, [] -> []
  |h1::tail1, h2::tail2 -> f h1 h2 :: zip_with f tail1 tail2;;

(*6*)
let rec unzip lst = match lst with
  |[] -> ([], [])
  |[(a, b)] -> ([a], [b])
  |(a, b)::tail -> 
    let (atail, btail) = unzip tail in 
    (a::atail, b::btail);;

(*7*)
(* evaluation steps of unzip [('a',1);('b',2)]

  The function unzip takes a list of pairs and returns a pair of two lists:
  the first list contains all first elements of each pair from the input list, 
  and the second list contains all second elements of each pair from the input list.

  unzip [('a',1);('b',2)] matches the third case (a, b)::tail, where a is 'a', b is 1, and tail is [('b', 2)].
  let (atail, btail) = unzip tail in ... evaluates unzip tail, where tail is [('b', 2)].
  unzip tail matches the second case [(a, b)], where a is 'b' and b is 2.
  ([a], [b]) evaluates to (['b'], [2]).
  Back to step 2, let (atail, btail) = (['b'], [2]) in ... binds atail to ['b'] and btail to [2].
  (a::atail, b::btail) evaluates to ('a'::['b'], 1::[2]) -> (['a', 'b'], [1, 2]).
*)


(*8*)
type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha;;

let compare_points (_, _, _, _, _, _, _, p1) (_, _, _, _, _, _, _, p2)  = compare p2 p1;;
let sort_list_by_points = List.sort compare_points;;

let compare_goal_difference (_, _, _, _, _, gf1, ga1, _) (_, _, _, _, _, gf2, ga2, _) = compare (gf2 - ga2) (gf1 - ga1);;
let sort_list_by_goal_difference = List.sort compare_goal_difference;;

let compare_goals_for (_, _, _, _, _, gf1, _, _) (_, _, _, _, _, gf2, _, _) = compare gf2 gf1;;
let sort_list_by_goals_for = List.sort compare_goals_for;;

(*random is like a comparison function, it takes two elements from the list and returns random integer that is either -1 0 or 1
  Random.int 3 generates integers from 0 to 2, so we subtract 1 and we have -1 0 1. *)
let sort_list_randomly lst = 
  let random _ _ = Random.int 3 - 1 in
  List.sort random lst;;

let sort_list lst = sort_list_by_points (sort_list_by_goal_difference (sort_list_by_goals_for (sort_list_randomly lst)));;

let first_list lst =
  let rec update_list lst t scored conceded = match lst with
    |[] -> [(t, 1, (if scored > conceded then 1 else 0), (if scored = conceded then 1 else 0), (if scored < conceded then 1 else 0), scored, conceded, (if scored > conceded then 3 else if scored = conceded then 1 else 0))]
    |(team, g, w, d, l, gf, ga, p)::tail when team = t ->
      let new_record =
        (team, g + 1, 
         w + (if scored > conceded then 1 else 0), 
         d + (if scored = conceded then 1 else 0), 
         l + (if scored < conceded then 1 else 0), 
         gf + scored, ga + conceded, 
         p + (if scored > conceded then 3 else if scored = conceded then 1 else 0)) in
      new_record::tail
    |h::tail -> h::update_list tail t scored conceded in
  let team_records = ref [] in
  List.iter (fun (team1, scorers1, team2, scorers2) ->
    let score1, score2 = (List.length scorers1), (List.length scorers2) in
    team_records := update_list (update_list !team_records team1 score1 score2) team2 score2 score1
  ) lst;
  !team_records
  |>sort_list;;

let compare_goals_and_names (p1, _, g1) (p2, _, g2) = match compare g2 g1 with
  |0 -> compare p1 p2
  |c -> c;;
let sort_by_goals_and_names = List.sort compare_goals_and_names;;

let rec goal_counter player lst = match lst with
  | [] -> 0
  | (_, [], _, []) :: tail -> goal_counter player tail
  | (team1, h1 :: tail1, team2, []) :: tail ->
      if h1 = player then 1 + goal_counter player ((team1, tail1, team2, []) :: tail)
      else goal_counter player ((team1, tail1, team2, []) :: tail)
  | (team1, [], team2, h2 :: tail2) :: tail ->
      if h2 = player then 1 + goal_counter player ((team1, [], team2, tail2) :: tail)
      else goal_counter player ((team1, [], team2, tail2) :: tail)
  | (team1, h1 :: tail1, team2, h2 :: tail2) :: tail ->
      if h1 = player || h2 = player
      then 1 + goal_counter player ((team1, tail1, team2, tail2) :: tail)
      else goal_counter player ((team1, tail1, team2, tail2) :: tail)

let remove_duplicates lst =
  let sorted = List.sort (fun (p1, _, _) (p2, _, _) -> compare p1 p2) lst in
    let rec remove_duplicates_sorted lst = match lst with
      |[] -> []
      |[x] -> [x]
      |(player1, team1, goals1)::(player2, team2, goals2)::tail ->
        if player1 = player2 then remove_duplicates_sorted ((player1, team1, goals1)::tail)
        else (player1, team1, goals1)::remove_duplicates_sorted ((player2, team2, goals2)::tail) in
          remove_duplicates_sorted sorted;;

let rec second_list lst = match lst with
<<<<<<< HEAD
  | [] -> []
  | (team1, [], team2, []) :: tail -> second_list tail
  | (team1, h1 :: tail1, team2, []) :: tail ->
      (h1, team1, goal_counter h1 lst) :: (second_list ((team1, tail1, team2, []) :: tail))
  | (team1, [], team2, h2 :: tail2) :: tail ->
      (h2, team2, goal_counter h2 lst) :: (second_list ((team1, [], team2, tail2) :: tail))
  | (team1, h1 :: tail1, team2, h2 :: tail2) :: tail ->
      (h1, team1, goal_counter h1 lst) ::
      (h2, team2, goal_counter h2 lst) ::
      (second_list ((team1, tail1, team2, tail2) :: tail))
  |> remove_duplicates
  |> sort_by_goals_and_names

let table_and_scorers lst = (first_list lst, second_list lst);;
=======
  |[] -> []
  | (team1, [], team2, []) :: tail -> second_list tail
  | (team1, h1 :: tail1, team2, h2 :: tail2) :: tail ->
      (h1, team1, goal_counter h1) ::
      (h2, team2, goal_counter h2) ::
      (second_list [(team1, tail1, team2, tail2)]) @
      (second_list tail)
  | (team1, h1 :: tail1, team2, []) :: tail ->
      (h1, team1, goal_counter h1) ::
      (second_list [(team1, tail1, team2, [])]) @
      (second_list tail)
  | (team1, [], team2, h2 :: tail2) :: tail ->
      (h2, team2, goal_counter h2) ::
      (second_list [(team1, [], team2, tail2)]) @
      (second_list tail)
  |> sort_by_player_name_alphabetically;;



(* let table_and_scorers = (first_list, second_list);; *)
>>>>>>> 718feb3e850a21f4dded9dbc4e8a4eba46caafcb
