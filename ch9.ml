(* chpt 9 notes *)
(* searching for things *)

(* this section is all about substring/sublist search *)
(* we want a function search_list that sees if a pattern p is in a list l
 * for example: [1;2] is in [2;1;2;2] but not in [2;1;1]
 *
 * a list is a sublist if there are lists x, y so that l = x @ p @ y *)

let rec take l n =
  match l with
    [] -> []
  | x::xs -> if n = 0 then l
             else x::(take l (n-1))
;;

let rec search_list p l =
  List.length p <= List.length l
  &&
    (take l (List.length p) = p || search_list p (List.tl l))
;;

(* pretty inefficient. we can memoize *)
