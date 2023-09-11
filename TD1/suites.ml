(* 1 *)
let rec u n=
  match n with
  |0 -> 1
  |n -> (u (n-1)) + 1;;

u 5;;

(* 2 *)
let rec v n=
  match n with
  |0 -> 2
  |1 -> 3
  |n -> (v (n-1)) + (v (n-2));;

v  6;;

(* 3 *)
let rec w n=
  match n with
  |0 -> 2
  |1 -> 3
  |n -> (w (n-1)) * (w (n-2));;

w 4;;

(* 4 *)
let rec t n=
  match n with
  |0 -> 2
  |n -> (t (n-1)) + 4;;

t 1;;

(* 5 *)
let rec r n=
  match n with
  |0 -> 0
  |1 -> 1
  |n -> (r (n-1)) + (r (n-2));;

r 3;;
