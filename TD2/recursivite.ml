let rec fact_if n = if n=1 then 1 else n*fact_if (n-1);;

let rec fact_match n =
  match n with
  |1-> 1
  |n -> n * fact_match (n-1);;

(* fact_if 5;;
fact_match 5;; *)

let rec serie n =
  match n with 
  |0-> 1.
  |n -> (1./. (float_of_int(n *2))) +. serie (n-1);;

(* serie 0;;
serie 1;;
serie 2;; *)

let rec fibonacci n =
  match n with
  |0 -> 0
  |1 -> 1
  |n -> fibonacci (n-1) + fibonacci(n-2);;

(* fibonacci 6;; *)

let rec combinaison n p =
  match p with
  |0 -> 1
  |p when p = n -> 1
  |p -> combinaison (n-1) (p-1) + combinaison (n-1) p;;

combinaison 3 2;;

let rec ackerman m n =
  match m with 
  |0 ->  n+1
  |m when n=0 -> ackerman (m-1) 1
  |m -> ackerman (m-1) (ackerman m (n-1));;

ackerman 0 5;;
ackerman 2 0;;
ackerman 2 3;;