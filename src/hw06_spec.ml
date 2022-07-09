open Hw06
open OUnit2

let rec pr_nlist_list pe ppf = function
  | [] -> ()
  | [xs] -> pr_nlist pe ppf xs
  | xs :: xss ->
      Format.fprintf ppf "%a;@ %a" (pr_nlist pe) xs (pr_nlist_list pe) xss 
  
and pr_nlist pe ppf = function
  | Atom x -> Format.fprintf ppf "NLOne@ @[%a@]" pe x
  | NList xss -> Format.fprintf ppf "NLList@[<2>@ [@[%a@]]@]" (pr_nlist_list pe) xss

let string_of_int_nlist xss =
  pr_nlist (fun ppf -> Format.fprintf ppf "%d") Format.str_formatter xss;
  Format.flush_str_formatter ()

let rec pr_tree ppf = function
  | Leaf -> Format.fprintf ppf "Leaf"
  | Node (x, left, right) ->
      Format.fprintf ppf "@[<2>Node@ (%d,@ %a,@ %a)@]" x pr_tree left pr_tree right

let string_of_tree t =
  pr_tree Format.str_formatter t;
  Format.flush_str_formatter ()

        
let list_to_string ets = 
  let rec lts rep = function
    | [] -> rep ^ "]"
    | x :: xs -> lts (rep ^ "; " ^ ets x) xs
  in
  function
  | [] -> "[]"
  | x :: xs -> lts (Printf.sprintf "[%s" (ets x)) xs

let pair_to_string pts1 pts2 (x, y) =
  Printf.sprintf "(%s, %s)" (pts1 x) (pts2 y)
    
let plist_to_string =
  list_to_string (fun (x, s) -> Printf.sprintf "(%d, \"%s\")" x s)

let slist_to_string =
  list_to_string (fun s -> "\"" ^ s ^ "\"")

let ilist_to_string =
  list_to_string (Printf.sprintf "%d")

(** Problem 1 tests *)

let unzip_tests =
  [[(1, "a"); (2, "b"); (3, "c")], ([1; 2; 3], ["a"; "b"; "c"]);
   [], ([], []);
   [(1, "a")], ([1], ["a"])]

let unzip_suite =
  let pr_result = pair_to_string ilist_to_string slist_to_string in
  List.map
    (fun (arg, res) ->
      let name = " unzip " ^ (plist_to_string arg) in
      name >:: fun tc -> assert_equal ~printer:pr_result res (unzip arg))
    unzip_tests

let fold_right_tests1 =
  [([1; 2; 3], (fun x xs -> x :: xs), "(fun x xs -> x :: xs)", []);
   ([], (fun x xs -> x :: xs), "(fun x xs -> x :: xs)", []);
   ([1; 2; 3], (fun x xs -> x :: xs), "(fun x xs -> x :: xs)", [4; 5])]
      
let fold_right_tests2 =
  [([1; 2; 3], (+), "(+)", 0);
   ([], (+), "(+)", 3);
   ([3; 5; 7], (+), "(+)", 4)]

let fold_right_suite =
  let mk_list pr_z =
    List.map
      (fun (xs, op, op_str, z) ->
        let name = "fold_right " ^ op_str ^ " " ^ (ilist_to_string xs) ^ " " ^ (pr_z z) in
        name >:: fun tc -> assert_equal ~printer:pr_z (List.fold_right op xs z) (fold_right op xs z))
  in
  mk_list ilist_to_string fold_right_tests1 @
  mk_list string_of_int fold_right_tests2

let in_relation_tests =
  [((<), "(<)", [1; 2; 3; 5; 6], true);
   ((<), "(<)", [1], true);
   ((<), "(<)", [], true);
   ((<), "(<)", [1; 3; 2], false);
   ((=), "(=)", [1; 1; 1], true);
   ((=), "(=)", [1; 1; 2], false);
   ((fun x y -> y = x + 1), "(fun x y -> y = x + 1)", [1; 2], true);
   ((fun x y -> y = x + 1), "(fun x y -> y = x + 1)", [1; 2; 4], false)]

let in_relation_suite =
  List.map
    (fun (op, op_str, xs, res) ->
      let name = "in_relation " ^ op_str ^ " " ^ (ilist_to_string xs) in
      name >:: fun tc -> assert_equal ~printer:string_of_bool res (in_relation op xs))
    in_relation_tests
    
(** Problem 2 tests *)

let emp = Leaf
let mk_elem x = Node (x, emp, emp)
let mk_tree x l r = Node (x, l, r)
    
let t1 = emp
let t2 = mk_elem 1
let t3 = mk_tree 1 (mk_elem 3) (mk_elem 4)
let t4 = mk_tree 3 (mk_elem 1) (mk_elem 4)
let t5 = mk_tree 7 (mk_tree 3 (mk_elem 1) (mk_tree 5 (mk_elem 4) (mk_elem 6))) (mk_tree 9 (mk_elem 8) (mk_elem 10))
let t6 = mk_tree 7 (mk_tree 3 (mk_elem 1) (mk_tree 5 (mk_elem 4) (mk_elem 8))) (mk_tree 9 (mk_elem 8) (mk_elem 10))
let t7 = mk_tree 7 (mk_tree 3 (mk_elem 1) (mk_tree 5 (mk_elem 4) (mk_elem 6))) (mk_tree 9 (mk_elem 6) (mk_elem 10))
let t8 = mk_tree 1 (mk_elem 1) (mk_elem 4)
let t9 = mk_tree 4 (mk_elem 1) (mk_elem 4)
    
let is_sorted_tests =
  [(t1, true); (t2, true); (t3, false); (t4, true); (t5, true); (t6, false); (t7, false); (t8, false); (t9, false)]

let is_sorted_suite =
  List.map
    (fun (t, res) ->
      let name = "is_sorted (" ^ string_of_tree t ^ ")" in
      name >:: fun tc -> assert_equal ~printer:string_of_bool res (is_sorted t))
    is_sorted_tests
    
let list_of_tree_tests =
  [(t1, []); (t2, [1]); (t4, [1; 3; 4]); (t3, [3; 1; 4]);
   (t5, [1; 3; 4; 5; 6; 7; 8; 9; 10]);
   (t6, [1; 3; 4; 5; 8; 7; 8; 9; 10]); (t7, [1; 3; 4; 5; 6; 7; 6; 9; 10])]

let list_of_tree_suite =
  List.map
    (fun (t, res) ->
      let name = "list_of_tree (" ^ string_of_tree t ^ ")" in
      name >:: fun tc -> assert_equal ~printer:ilist_to_string res (list_of_tree t))
    list_of_tree_tests

    
(** Complete test suite *)

let suite =   
  "Tests" >::: unzip_suite @ fold_right_suite @ in_relation_suite @ is_sorted_suite @ list_of_tree_suite
    
let () = run_test_tt_main suite
