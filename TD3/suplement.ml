(* recherche les images indirectes de x *)
let rec recherche_indirecte x l =
  match l with 
  |(x1, y1)::q when x1 = x -> y1::(recherche_indirecte y1 q)
  |(x1, y1)::q -> recherche_indirecte x q
  |[] -> [];;
 
(* Créer une relation: x est l'image et l sont les antécédents *)
let rec linkit x l =
  match l with
  |t::q -> (x, t)::(linkit x q)
  |[] -> [];;

let rec fermeture l = 
  match l with 
  |(x1, y1)::q ->  [(x1, y1)]@(linkit x1 (recherche_indirecte y1 q))@(fermeture q)
  |[] -> [];;

(* fermeture [(1,2);(2,3);(3,4)];; *)
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

let sortset liste = check (split (setify liste));;
  
let invert r = check (split (swap r));;

let rec liste_element l =
  match l with
  |(x, y)::q -> x::y::(liste_element q)
  |[] -> [];;

let rec nb_image x l =
  match l with 
  |(x1,y1)::q when x = x1 -> 1 + (nb_image x q)
  |(x1, y1)::q -> nb_image x q
  |[] -> 0;;


(* nb_image 2  [(1,2);(2,3);(2,4);(3,1);(4,4)];; *)


let rec formation_couple element inverted =
  match element with
  |x::q -> (x, nb_image x inverted)::formation_couple q inverted
  |[] -> [];;

let tri_typologique l = formation_couple (sortset (liste_element l)) (invert l);;

tri_typologique [(1,2);(1,6);(2,3);(2,5);(3,4);(6,2)];;