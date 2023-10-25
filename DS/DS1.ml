let volume_cylindre pi r h = pi*.r*.r*.h;;

(* volume_cylindre 3.14 2. 4.;; *)

let rec count a liste = 
  match liste with
  |[] -> 0
  |t::q when t = a -> 1+(count a q)
  |t::q -> count a q;;

(* count 0 [0;10;20;0;30;0;40];; *)

let rec sum liste =
  match liste with
  |[] -> 0
  |t::q -> t+(sum q);;

(* sum [1;2;3;4];; *)

let rec nth liste i =
  match liste with 
  |[] -> failwith "cet index n'esxiste pas"
  |t::q when i = 1 -> t
  |t::q -> nth q (i-1);;

(* nth [10;20;30;40;50] 3;; *)

let rec intersection l1 l2 =
  match (l1,l2) with 
  |([],_) -> []
  |(_,[]) -> []
  |(t1::q1, t2::q2) when t1 = t2 -> t1::(intersection q1 q2)
  |(t1::q1, t2::q2) when t1 > t2 -> intersection l1 q2
  |(t1::q1, t2::q2) -> intersection q1 l2;;

(* intersection [2;3;4;5;6] [1;2;4;5;7];; *)

let rec breakcoverage liste = 
  match liste with
  |[] -> failwith "Il n'y a pas d'élément dans la liste"
  |t::q when sum q > t -> breakcoverage q
  |t::q -> t;;

(* breakcoverage [1;2;3;20;9;8];;
breakcoverage [10;1;0;40;50];; *)
  
let rec dominante l1 l2 = 
  match (l1, l2) with 
  |(_,[]) -> true
  |([],_) -> false
  |(t1::q1, t2::q2) when t1 >= t2 -> dominante q1 q2
  |(t1::q1, t2::q2) -> false;;


(* dominante [10;20;30;40] [1;2;3;4];;
dominante [10;20;30;40;50;60] [1;2;3;4];;
dominante [10;20;30;40] [1;2;3;4;5;6];;
dominante [10;20;30;40] [1;2;300;4];; *)

let rec occurence a liste =
  match liste with 
  |[] -> 0
  |t::q when t = a -> 1+(occurence a q)
  |t::q -> occurence a q;;

let rec mem a l =
  match l with 
  |[] -> false
  |(t,v)::q when t = a -> true
  |t::q -> mem a q;;

let rec sumcouple l =
  match l with 
  |[] -> []
  |t::q -> (t, 1 + (occurence t q))::(sumcouple q);;

let rec setify l =
  match l with 
  |[] -> []
  |(t,v)::q when mem t q -> setify q
  |t::q -> t::(setify q);;

let sumup l = setify (sumcouple l);;

sumup [10;20;10;20;20;30;40;40;40;50;40];;