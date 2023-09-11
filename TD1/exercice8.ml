let infini = 1.0 /. 0.0;;

let discr a b c = if (((b *. b) -. (4. *. a *. c)) < 0.) then infini else (b *. b -. (4. *. a *. c));;

let sol1 a b c = (-.b +. sqrt (discr a b c)) /. (2. *. a);;

let sol2 a b c = (-.b -. sqrt (discr a b c)) /. (2. *. a);;

discr (-1.) (-2.) 3.0;;
sol1 (-1.) (-2.) 3.0;;
sol2 (-1.) (-2.) 3.0;;