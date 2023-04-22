(*Define a data type for a student. For every student the first name, last name, identification number(unique), current semester 
as well as the grades received in different courses have to be stored. A course is simply represented by a number.*)
type student = {fname:string; lname:string; id:int; semester:int; grades:(int *float) list};;

(*Define database as a list of students.*)

let st1 = {fname = "a"; lname = "aa"; id = 1; semester = 1; grades = [(10, 3.7); (11, 4.0)]};;
let st2 = {fname = "b"; lname = "bb"; id = 2; semester = 1; grades = [(5, 2.7); (11, 3.0)]};;
let st3 = {fname = "c"; lname = "cc"; id = 3; semester = 2; grades = [(10, 1.7); (11, 2.0)]};;
let st4 = {fname = "d"; lname = "cc"; id = 4; semester = 3; grades = [(10, 1.7); (11, 2.0)]};;

let database = [st1; st2; st3; st4];;

(*Write a function insert : student -> database -> database that inserts a student into the database.*)
let insert st db = st :: db;;

(*Write a function find_by_id : int -> db -> student list that returns a list with all students with the
given id (either a single student(because ids are unique) or an empty list, if no such student exists).*)
let rec find_by_id id db = match db with
  |[] -> []
  |h::tail -> if h.id = id then [h] else find_by_id id tail;;

(*Implement a function find_by_last_name : string -> database -> student list to find all students with a given last name.*)
let rec find_by_last_name lname db = match db with
  |[] -> []
  |h::tail -> if h.lname = lname then h::find_by_last_name lname tail else find_by_last_name lname tail;;

(*Implement a function remove_by_id : int -> database -> database removes the student with the given id from the database.*)
let rec remove_by_id id db = match db with
  |[] -> []
  |h::tail -> if h.id = id then tail else h :: remove_by_id id tail;;

(*Implement a function count_in_semester : int -> database -> int counts the number of students in the given semester.*)
let rec count_in_semester semester db = match db with 
  |[] -> 0
  |h::tail -> if h.semester = semester then 1 + count_in_semester semester tail else count_in_semester semester tail;;

(*Implement a function student_avg_grade : int -> database -> float computes the average grade of the student with the given id. 
If no student with the given id exists or the student does not have any grades, the function shall return 0.0.*)

(*We create an additional function grade_counter which counts the sum of grades in the list of tuples. If the list is empty,
meaning student does not have any grades, the function returns 0.0.*)

let rec grade_counter lst = match lst with
  |[] -> 0.0
  |(_, a)::tail -> a +. grade_counter tail;; 

let rec student_avg_grade id db = match find_by_id id db with
  |[] -> 0.0
  |h::tail -> if h.grades = [] then 0.0 else (grade_counter h.grades) /. (Int.to_float (List.length h.grades));;


(*Implement a function course_avg_grade : int -> database -> float computes the average grade achieved in the given course. 
If no grades in the given course exist, the function shall return 0.0.*)


(*how many xtudents are taking this course*)
let rec courseCounter course_num db = match db with
  |[] -> 0.0
  |(x,y)::tail -> if course_num = x then 1.0 +. courseCounter course_num tail else courseCounter course_num tail;; 

let rec get_grade course_num lst = match lst with
  |[] -> 0.0
  |(x, y)::tail -> if x = course_num then y else get_grade course_num tail;;

let rec course_grade_sum course_num db = match db with
|[] -> 0.0
|h::tail -> get_grade course_num h.grades +. course_grade_sum course_num tail;;

let course_avg_grade course_num db = (course_grade_sum course_num db) /. (courseCounter course_num db);;

