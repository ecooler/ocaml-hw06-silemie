(** Problem 1 *)

(* unzip: ('a * 'b) list -> 'a list * 'b list *)

let unzip xys = failwith "Not yet implemented"



(* fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let fold_right op xs z = failwith "Not yet implemented"

(* in_relation: ('a -> bool) -> 'a list -> bool *)
let in_relation p xs = failwith "Not yet implemented"

    
(** Problem 2 *)

(** An ADT for nested lists *)
type 'a nlist =
  | NList of ('a nlist) list
  | Atom of 'a

(* flatten: 'a nlist -> 'a list *)
let flatten xss = failwith "Not yet implemented"

(** An ADT for binary search trees *)
type tree =
  | Leaf
  | Node of int * tree * tree

(* fold: ('a -> int -> 'a) -> 'a -> tree -> 'a *) 
let rec fold op z t = failwith "Not yet implemented"
        
(* list_of_tree: tree -> int list *)
let list_of_tree t = failwith "Not yet implemented"

(* is_sorted: tree -> bool *)
let is_sorted t = failwith "Not yet implemented"
  
