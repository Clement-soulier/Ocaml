let maximum a b = if( a > b ) then a else b;;

let max3 a b c = if ( a > b ) then (maximum a c) else (maximum b c);;
max3 78 9 89;;