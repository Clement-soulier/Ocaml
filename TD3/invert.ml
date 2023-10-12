let rec swap liste = 
  match liste with 
  |[] -> []
  |(x, y)::q -> (y, x)::(swap q);;

let rec aux_split l1 l2 liste = 
  match liste with
  |t1::t2::q -> aux_split (l1@[t1]) (l2@[t2]) q
  |t::q -> (l1@[t],l2)
  |[] -> (l1,l2);;
  
let split l = aux_split [] [] l;;
  
let sort l = 
  match l with 
  |a::b::q when a < b -> [a]@[b]
  |a::b::q -> [b]@[a]
  |a::q -> [a]
  |[] -> [];;
  
  let rec union_triee r1 r2 =
    match (r1,r2) with
    |([], r2) -> r2
    |(r1, []) -> r1
    |(t1::l1, t2::l2) when t1 = t2 -> t1::union_triee l1 l2
    |((x1,y1)::l1,(x2,y2)::l2) when x1 > x2 -> (x2,y2)::union_triee r1 l2
    |((x1,y1)::l1,(x2,y2)::l2) when x1 < x2 -> (x1,y1)::union_triee l1 r2
    |((x1,y1)::l1,(x2,y2)::l2) when y1 > y2 -> (x2,y2)::union_triee r1 l2
    |((x1,y1)::l1,(x2,y2)::l2) -> (x1,y1)::union_triee l1 r2;;
  
let rec check couple = 
  match couple with
  |(t11::t12::t13::q1, t21::t22::t23::q2) -> union_triee (check (split (t11::t12::t13::q1))) (check (split(t21::t22::t23::q2)))
  |(t11::t12::t13::q1,t21::t22::q2) -> union_triee (check(split (t11::t12::t13::q1)))  (sort ([t21]@[t22]))
  |(t11::t12::q1, t21::t22::q2) -> union_triee (sort ([t11]@[t12])) (sort ([t21]@[t22]))
	|(t11::t12::q1, t21::q2) -> union_triee (sort ([t11]@[t12])) [t21]
  |(l1,l2) -> union_triee l1 l2;;

  
let invert r = check (split (swap r));;

invert [(1,2);(2,3);(2,4);(3,1);(4,4)];;