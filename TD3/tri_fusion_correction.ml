let rec union l1 l2 = 
  match (l1,l2) with
  |([],l2) -> l2
  |(l1,[]) -> l1
  |(a1::r1, a2::r2) when a1 < a2 -> a1::(union r1 (a2::r2))
  |(a1::r1, a2::r2) when a1 > a2 -> a2::(union (a1::r1) r2)
  |(a1::r1, _::r2) -> a1::(union r1 r2);;


let rec intersection l1 l2 =
  match (l1, l2) with
  |([],l2) -> []
  |(l1,[]) -> []
  |(a1::r1, a2::r2) when a1 < a2 -> intersection r1 l2
  |(a1::r1, a2::r2) when a1 > a2 -> intersection l1 r2
  |(a1::r1, _::r2) -> a1::(intersection r1 r2);;

let rec split l =
  match l with
  |[] -> ([], [])
  |[a] -> ([a], [])
  |a1::a2::r -> let (l1, l2) = split r in (a1::l1, a2::l2) ;;

let rec sortset l = 
  match l with
  |[] -> []
  |[a] -> [a] (* Attention à ne pas oublier ce cas pour ne pas avoir de récursion infinie*)
  |l -> let (l1, l2) = (split l) in union (sortset l1) (sortset l2);;