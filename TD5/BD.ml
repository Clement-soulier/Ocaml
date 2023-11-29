type mois_m = January | February | March | April | May | June | July | August | September | October | November | December;;

type date_t = {jour : int; mois : mois_m; annee : int};;

let correct_date date = 
  match date with
  | {annee; _} when annee < 1900 || annee > 2008 -> false
  | {jour; mois = (January | March | May | July | August | October | December)} when (jour < 1) || (jour > 32) -> false
  | {jour; mois = February} when jour <> 29 -> false
  | _ -> true;;

(* correct_date {jour=30; mois=December; annee=18};; *)

type person_p = {nom : string; prenom: string; dateNaissance: date_t};;

let rec ajout bd enregistrement = 
  match bd with
  |[] when enregistrement.nom <> "" -> [enregistrement]
  |[] -> []
  |t::q when (t.nom > enregistrement.nom) && (enregistrement <> {nom=""; prenom=""; dateNaissance={jour=0;mois=July;annee=800}}) -> enregistrement::t::(ajout q {nom=""; prenom=""; dateNaissance={jour=0;mois=July;annee=800}})
  |t::q when (t.nom < enregistrement.nom) && (enregistrement <> {nom=""; prenom=""; dateNaissance={jour=0;mois=July;annee=800}}) -> t::(ajout q enregistrement)
  |t::q -> t::(ajout q enregistrement);;

let rec supression bd enregistrement = 
  match bd with
  |[] -> []
  |t::q when t = enregistrement -> supression q enregistrement
  |t::q -> t::(supression q enregistrement);;

let rec recherche bd nom = 
  match bd with
  |[] -> failwith "cette enregistrement n'existe pas"
  |t::q when t.nom = nom -> t
  |t::q -> recherche q nom;;

let bd = [{nom="Pétrolier"; prenom="Gérard"; dateNaissance={jour=12; mois=March; annee=1976}};
          {nom="Dodo"; prenom = "Karotte"; dateNaissance={jour=3; mois=January; annee=1956}};
          {nom="Jojo"; prenom="Jotaro"; dateNaissance={jour=9;mois=November;annee=2000}}];;

let rec filter p l = 
  match l with
  |[] -> []
  |t::q when (p t) -> t::(filter p q)
  |t::q -> filter p q;;

let question1 enregistrement = enregistrement.dateNaissance.jour = 12 && enregistrement.dateNaissance.mois = March;;
let question2 enregistrement = String.get enregistrement.nom 0 = 'D';;

filter question1 bd;;
filter question2 bd;;


let rec select question format l = 
  match l with
  |[] -> []
  |t::q when (question t) -> (format t)::(select question format q)
  |t::q -> select question format l;;

  
