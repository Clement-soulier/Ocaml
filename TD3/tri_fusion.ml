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

  let rec union_triee l1 l2 = 
    match (l1,l2) with
    |([],l2) -> l2
    |(l1,[]) -> l1
    |(a1::r1, a2::r2) when a1 < a2 -> a1::(union_triee r1 (a2::r2))
    |(a1::r1, a2::r2) when a1 > a2 -> a2::(union_triee (a1::r1) r2)
    |(a1::r1, _::r2) -> a1::(union_triee r1 r2);;

    let rec check couple = 
      match couple with
      |(t11::t12::t13::q1, t21::t22::t23::q2) -> union_triee (check (split (t11::t12::t13::q1))) (check (split(t21::t22::t23::q2)))
      |(t11::t12::t13::q1,t21::t22::q2) -> union_triee (check(split (t11::t12::t13::q1)))  (sort ([t21]@[t22]))
      |(t11::t12::q1, t21::t22::q2) -> union_triee (sort ([t11]@[t12])) (sort ([t21]@[t22]))
      |(t11::t12::q1, t21::q2) -> union_triee (sort ([t11]@[t12])) [t21]
      |(l1,l2) -> union_triee l1 l2;;

let rec mem_list x l =
  match l with
  |[] -> false
  |t::q when t = x -> true
  |t::q -> mem_list x q;;

let rec setify l = 
  match l with
  |[] -> []
  |t::q when mem_list t q -> setify q
  |t::q -> t::setify q;;

let sortset liste = 
  check (split (setify liste));;

(* sortset [1; 3; 4; 5; 2; 6];; *)

sortset [1;5;3;4;6];;