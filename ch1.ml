(* chpt 1 notes *)
(* all about folds *)

let rec fold_left f a l =
  match l with
    [] -> a
  | h::tl -> fold_left f (f a h) tl
;;

let rec fold_right f l a =
  match l with
    [] -> a
  | h::tl -> f h (fold_right f tl a)
;;

(* these are the standard folds in functional programming *)
let all l = fold_left (&&) true l;;

let any l = fold_left (||) false l;;

(* List.mem e a checks whether e is an element of a *)
(* we can use this to create "setify" for a list *)
let setify l =
  fold_left (fun a e -> if List.mem e a then a else e::a) [] l
;;

let map f l =
  fold_right (fun e a -> (f e)::a) l []
;;

(* but fold is a general paradigm for most functional data structures *)
(* take trees for instance *)
type 'a tree =
  Lf
| Br of 'a * 'a tree * 'a tree
;;

let rec fold_tree f e t =
  match t with
    Lf -> e
  | Br(x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)
;;

let tree_size t =
  fold_tree (fun x l r -> 1 + l + r) 0 t
;;

let tree_sum t =
  fold_tree (fun x l r -> x + l + r) 0 t
;;

(* we can also do tree traversals pretty easily in this case *)
let tree_preorder t = fold_tree (fun x l r -> [x] @ l @ r) [] t;;
let tree_inorder t = fold_tree (fun x l r -> l @ [x] @ r) [] t;;
let tree_postorder t = fold_tree (fun x l r -> l @ r @ [x]) [] t;;

(* chpt 1 exercises *)
(* 1: function which, given a list of expenses, removes them from a budget *)
let rmv_expenses exps budget =
  fold_left (fun bd ex -> bd - ex) budget exps;;

(* 2: calculate length of a list using a fold *)
let length l =
  fold_left (fun a _ -> 1 + a) 0 l;;

(* 3: use fold to find last element of list *)
let find_last l =
  fold_left (fun a h -> h) 0 l;;

(* 4: reverse a list using folds *)
let reverse l =
  fold_left (fun a h -> h::a) [] l;;

(* 5: write List.mem using folds *)
let mem e a =
  fold_left (fun b h -> (a = h) || b) false e;;
