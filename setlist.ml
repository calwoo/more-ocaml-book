module type SetType =
  sig
    type 'a t;;
    val set_of_list : 'a list -> 'a t;;
    val list_of_set : 'a t -> 'a list;;
    val insert : 'a -> 'a t -> 'a t;;
    val size : 'a t -> int;;
    val member : 'a -> 'a t -> bool;;
  end
;;

module SetList : sig include SetType end =
  struct
    type 'a t = 'a list;;
    let member = List.mem;;
    let insert x l =
      if member x l then l else x :: l
    ;;

    let rec set_of_list l =
      match l with
        [] -> []
      | x::xs -> insert x (set_of_list xs)
    ;;

    let list_of_set s = s;;

    let size = List.length;;
  end
;;

