let rec exp x n = if (n=0) then 1 else x*(exp x (n-1));;

let rec exp2 x n =
  match n with
  |0 -> 1
  |n -> x * (exp2 x (n-1));;

exp2 2 2;;