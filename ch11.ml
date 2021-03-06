(* chpt 11 notes *)
(* making sets *)

(* this section we construct various data structures for storing sets *)
(* for each set representation, we give five functions:
 *     - set_of_list = builds a set from list
 *     - list_of_set = builds a list from set
 *     - insert = puts given element into a set
 *     - size = gives cardinality of set
 *     - member = tests if an element is in a set *)

(* easy representation of set as a list *)
(* this time I'll use type annotations *)

let member = List.mem;;

