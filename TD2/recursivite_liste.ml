let rec after0 l = 
  match l with 
  |[] -> []
  |a::l when a <> 0 -> after0 l
  |a::l -> l;;

(* after0 [1;0;2;0;3];;
after0 [1;2];;
after0 [0];; *)

let rec before0 l =
  match l with 
  |[] -> []
  |0::_ -> []
  |a::l when a <> 0 -> [a] @ before0 l;;

(* before0 [1;0;2;0;3];; *)

let rec remove0 l = 
  match l with 
  |[] -> []
  |0::l -> remove0 l
  |a::l -> a :: remove0 l;;

(* remove0 [1;0;2;0;3];; *)

let rec add0 l = 
  match l with 
  |[] -> []
  |a::l -> a :: 0 :: add0 l;;

(* add0 [1;2;3]; *)

let rec opposite l =
  match l with 
  |[] -> []
  |a::l -> (-a) :: opposite l;;

(* opposite [1;2;3];; *)

let rec mem a l =
  match l with 
  |[] -> 0
  |v::l when v = a -> 1
  |v::l -> mem a l;;

(* mem 2 [1;2;3];; *)

let rec mem_triee a l = 
  match l with
  |[] -> 0
  |v::l when v > a -> 0
  |v::l when v = a -> 1
  |v::l -> mem_triee a l;;

(* mem_triee 0 [1;2;3];; *)

let rec nth i l = 
  match l with 
  |[] -> failwith "cet index n'exsite pas"
  |v::l when i = 1 -> v
  |v::l -> nth (i-1) l;;

(* nth 1 [1;2;3];;
nth 45 [1;2;3];; *)

let rec append l1 l2 = 
  match l1 with 
  |[] -> l2
  |v::reste -> v:: (append reste l2) ;;

(* append [1;3] [2;6];; *)