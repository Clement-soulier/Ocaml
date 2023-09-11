let pi =  3.14;;
let volumeSphere r = (4./.3.)*.pi*.r*.r*.r;;
volumeSphere 5.;;

let surface_disque r = pi*.r*.r;;
surface_disque 4.;;

let surface_rectangle longueur largeur = longueur*largeur;;
surface_rectangle 4 5;;

let surface_triangle base hauteur = (base*hauteur)/2;;
surface_triangle 6 2;;