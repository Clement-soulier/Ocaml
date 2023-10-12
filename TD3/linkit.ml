let rec linkit x l =
  match l with
  |t::q -> (x, t)::(linkit x q)
  |[] -> [];;

linkit 2 [2;4;5;6];;

let composition c1 c2 = 
  match (c1,c2) with
  |((x1, y1), (x2, y2)) when y1 = x2 -> (x1, y2)
  |(c1, c2) -> failwith "impossible de composer ces relations";;

composition (1, 2) (2, 4);;