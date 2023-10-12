let rec flatten l =
  match l with
  |a::l -> a @ (flatten l)
  |[] -> [];;
  


(* flatten [[1]; [2;3]; [4;5;6]];; *)

let rec mem a l =
  match l with 
  |[] -> false
  |v::l when v = a -> true
  |v::l -> mem a l;;

let rec clean l =
  match l with
  |[] -> []
  |a::l when (mem a l) = true -> clean l
  |a::l -> a::(clean l);;

  clean [1;1;2;1;1;2;2;3;4];;