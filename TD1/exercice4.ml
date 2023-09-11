let triangle l1 l2 l3 = if ((l1+l2 >= l3 && l2+l3 >= l1) && l3+l1>=l2) then true else false;;
triangle 4 6 7;;

let triangle_rectangle l1 l2 l3 = if (((l1*l1)+(l2*l2) = l3*l3) || ((l2*l2)+(l3*l3) = (l1*l1)) || ((l1*l1)+(l3*l3) = (l2*l2))) then true else false;;
triangle_rectangle 3 4 5;;