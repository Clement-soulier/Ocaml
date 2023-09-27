let boolToInt boolean = if boolean then 1 else 0;;
(* boolToInt true ;;
boolToInt false ;; *)

let intToBool integer = if integer <> 0 then true else false;;
(* intToBool 1;;
intToBool 2;;
intToBool 0;; *)

let int2roman n = 
  match n with
  |1 -> "I"
  |2 -> "II"
  |3 -> "III"
  |4 -> "IV"
  |5 -> "V"
  |6 -> "VI"
  |7 -> "VII"
  |8 -> "VIII"
  |9 -> "IX"
  |_ -> failwith "Ce n'est pas un chiffre ou n'existe pas en romain";;

int2roman 5;;
int2roman 0;;
