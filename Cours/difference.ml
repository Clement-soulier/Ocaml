let rec difference l1 l2 = 
  match (l1, l2) with
  |([], l2) -> l2
  |(l1, []) -> l1
  |(a1::r1, a2::r2) when  a1 = a2 -> difference r1 r2
  |(a1::r1, a2::r2) when a1 < a2 -> a1::(difference r1 l2)
  |(a1::r1, a2::r2) -> (difference l1 r2);;

difference [1;3;5;6] [1;2;4;6];;