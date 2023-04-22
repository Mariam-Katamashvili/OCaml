let compare_goals_and_names (p1, _, g1) (p2, _, g2) =
  match compare g2 g1 with
  | 0 -> compare p1 p2
  | c -> c;;

let sort_by_goals_and_names = List.sort compare_goals_and_names;;

let rec goal_counter player lst acc = match lst with
  | [] -> acc
  | (team1, l1, team2, l2)::tail ->
    let goals1 = List.length (List.filter ((=) player) l1) in
    let goals2 = List.length (List.filter ((=) player) l2) in
    goal_counter player tail (acc + goals1 + goals2);;

let rec second_list_aux acc lst = match lst with
  | [] -> acc
  | (team1, l1, team2, l2)::tail ->
    let players_goals team goals = List.map (fun player -> (player, team, goal_counter player lst 0)) goals in
    let team1_goals = players_goals team1 l1 in
    let team2_goals = players_goals team2 l2 in
    let new_acc = List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) acc (team1_goals @ team2_goals) in
    second_list_aux new_acc tail

let second_list lst = 
  second_list_aux [] lst |> sort_by_goals_and_names;;
