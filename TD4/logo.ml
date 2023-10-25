type logo_t = Avancer of int | Reculer of int | Gauche | Droite;;

type direction_t = Nord | Sud | Est | Ouest;;

let direction logo directionInitial =
  match (logo, directionInitial) with
  | (Avancer n, _) -> directionInitial
  | (Reculer n, _) -> directionInitial
  | (Droite, Nord) -> Est
  | (Droite, Est) -> Sud
  | (Droite, Sud) -> Ouest
  | (Droite, Ouest) -> Nord
  | (Gauche, Nord) -> Ouest
  | (Gauche, Ouest) -> Sud
  | (Gauche, Sud) -> Est
  | (Gauche, Est) -> Nord;;

let rec diriger programme directionInitial =
  match programme with
  | [] -> []
  | t::q -> let directionSuivante = direction t directionInitial in directionSuivante::(diriger q directionSuivante);;

let rec simplifie programme = 
  match programme with
  | [] -> []
  | (Avancer n)::(Avancer m)::q -> simplifie (Avancer (n + m)::q)
  | Droite::Gauche::q | Gauche::Droite::q -> simplifie q
  | Droite::Droite::Droite::q -> simplifie (Gauche::q)
  | Gauche::Gauche::Gauche::q -> simplifie (Droite::q)
  | (Avancer n) :: (Reculer m)::q when n = m -> simplifie q
  | (Reculer n)::(Avancer m)::q when n = m -> simplifie q
  | (Avancer n)::(Reculer m)::q when n > m -> simplifie (Avancer (n - m)::q)
  | (Avancer n)::(Reculer m)::q -> simplifie (Reculer (m - n)::q)
  | (Reculer n)::(Avancer m)::q when n > m -> simplifie (Reculer (n - m)::q)
  | (Reculer n)::(Avancer m)::q -> simplifie (Avancer (m - n)::q)
  | t::q -> t::simplifie q;;


let basic_move coordonnees directionCourante commande = 
  match (coordonnees, directionCourante, commande) with
  |(_, _, Gauche) -> coordonnees
  |(_, _, Droite) -> coordonnees
  |((x,y), Nord, Avancer n) -> (x, y + n)
  |((x,y), Nord, Reculer n) -> (x, y - n)
  |((x,y), Sud, Avancer n) -> (x, y - n)
  |((x,y), Sud, Reculer n) -> (x, y + n)
  |((x,y), Est, Avancer n) -> (x + n, y)
  |((x,y), Est, Reculer n) -> (x - n, y)
  |((x,y), Ouest, Avancer n) -> (x - n, y)
  |((x,y), Ouest, Reculer n) -> (x + n, y);;


let rec move coordonneesInitiales directionInitiale programme =
  match programme with
  |[] -> []
  |t::q when (t = Droite || t = Gauche) -> move coordonneesInitiales (direction t directionInitiale) q
  |t::q -> let nouvelleCoordonnees = (basic_move coordonneesInitiales directionInitiale t) in nouvelleCoordonnees::(move nouvelleCoordonnees (direction t directionInitiale) q);;