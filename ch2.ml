(* chpt 2 notes *)
(* being lazy *)
type 'a list = Nil | Cons of 'a * 'a list;;

(* these lists are finite-- could we create infinitely long ones? *)
(* we should create elements only when we need them-- ie a lazy list *)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist);;

(* for example: here is the sequence n, n+1, n+2,... *)
let rec lseq n =
  Cons(n, fun () -> lseq (n+1));;

(* we then build functions to extract the head and tail of a lazy list *)
let lhd (Cons(n, _)) = n;;
let ltl (Cons(_, tf)) = tf ();;

let rec ltake (Cons (h, tf)) n =
  match n with
    0 -> []
  | _ -> h :: ltake (tf ()) (n - 1)
;;

let rec ldrop (Cons (h, tf) as ll) n =
  match n with
    0 -> ll
  | _ -> ldrop (tf ()) (n - 1)
;;

(* more functions have lazy list analogues *)
let rec lmap f (Cons (h, tf)) =
  Cons (f h, fun () -> lmap f (tf ()))
;;

let rec lfilter p (Cons (h, tf)) =
  if p h then
    Cons (h, fun () -> lfilter p (tf ()))
  else
    lfilter p (tf ())
;;

(* lets use these functions to get the cubes divisible by 5 *)
let cubes : int lazylist =
  lfilter (fun x -> x mod 5 = 0)
    (lmap (fun x -> x * x * x) (lseq 1))
;;

(* even cooler, here's a list of the primes! *)
let rec mkprimes (Cons (h,tf)) =
  Cons (h, fun () ->
           mkprimes (lfilter (fun x -> x mod h <> 0) (tf ())))
;;

let primes = mkprimes (lseq 2);;

(* chpt 2 exercises *)
(* 1: write a lazy list 1, 2, 4, 8, 16, ... *)
let squares_list = lmap (fun x -> x * x) (lseq 1);;
let rec squares_list_v2 n = Cons (n*n, fun () -> squares_list_v2 (n+1));;

(* 2: return nth element of lazy list *)
let get l n = lhd (ldrop l n);;

(* 3: infinitely repeat a list *)
let rec repeater_inner l tmp =
  match l with
    [] -> repeater_inner tmp tmp
  | h::tl -> Cons (h, fun () -> repeater_inner tl tmp)
;;

let repeater l = repeater_inner l l;;

(* 4: fibonacci numbers *)
let fibonacci =
  let rec fibonacci_inner a b =
    Cons (b, fun () -> fibonacci_inner b (a+b)) in
  fibonacci_inner 1 0
;;

(* 5: unzip a stream alternatively *)
let unleave l =
  let rec unleave_odd (Cons (h, tf)) =
    Cons (h, fun () -> unleave_odd (ltl (tf ()))) in
  (unleave_odd l, unleave_odd (ltl l))
;;

