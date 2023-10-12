let rec mem x r = 
  match r with
  |[] -> false
  |a::l when a = x -> true
  |a::l -> mem x l;;

(* mem (3,4) [(1,2);(2,3);(2,4);(3,1);(4,4)];; *)

let rec img a r = 
  match r with 
  |[] -> []
  |(x,y)::l when a = x -> y :: img a l
  |(x,y)::l -> img a l;;

(* img 2 [(1,2);(2,3);(2,4);(3,1);(4,4)];; *)

let rec union r1 r2 =
  match r1 with
  |[] -> r2
  |t::q when mem t r2 -> union q r2
  |t::q -> t::union q r2;;
  
(* union [(1,2);(2,3);(2,4);(3,1);(4,4)] [(1,2);(2,4);(4,5);(5,1);];; *)

let rec union_triee r1 r2 =
  match (r1,r2) with
  |([], r2) -> r2
  |(r1, []) -> r1
  |(t1::l1, t2::l2) when t1 = t2 -> t1::union_triee l1 l2
  |((x1,y1)::l1,(x2,y2)::l2) when x1 > x2 -> (x2,y2)::union_triee r1 l2
  |((x1,y1)::l1,(x2,y2)::l2) when x1 < x2 -> (x1,y1)::union_triee l1 r2
  |((x1,y1)::l1,(x2,y2)::l2) when y1 > y2 -> (x2,y2)::union_triee r1 l2
  |((x1,y1)::l1,(x2,y2)::l2) -> (x1,y1)::union_triee l1 r2;;

(* union_triee [(1,2);(2,4);(3,1);(4,4)] [(1,2);(2,3);(2,4);(4,5);(5,1);];; *)

let rec inter r1 r2 = 
  match r1 with 
  |[] -> []
  |t::q when mem t r2 -> t::inter q r2
  |t::q -> inter q r2;;

(* inter [(1,2);(2,3);(2,4);(3,1);(4,4)] [(1,2);(2,4);(4,4);(5,1);];; *)

let rec inter_triee r1 r2 = 
  match (r1,r2) with
  |([],r2) -> []
  |(r1,[]) -> []
  |(t1::q1,t2::q2) when t1 = t2 -> t1::inter_triee q1 q2
  |((x1,y1)::q1,(x2,y2)::q2) when x1 > x2 -> inter_triee r1 q2
  |((x1,y1)::q1,(x2,y2)::q2) when x1 < x2 -> inter_triee q1 r2
  |((x1,y1)::q1,(x2,y2)::q2) when y1 < y2 -> inter_triee q1 r2
  |((x1,y1)::q1,(x2,y2)::q2) -> inter_triee r1 q2;;

(* inter_triee [(1,2);(2,4);(3,1);(4,4)] [(1,2);(2,3);(2,4);(4,4);(5,1)];; *)