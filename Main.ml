

let x = "Hello world ";;


type monome = { coeff : int; puiss : int };;

let polynome : monome list = [{ coeff = 1; puiss = 4 }; { coeff = 4; puiss = 3 }; { coeff = 2; puiss = 2 }];;

(* Fonction pour afficher un monome *)
let print_point p =
  Printf.printf "{ Coefficient = %d; puissance = %d }\n" p.coeff p.puiss;;


(* Fonction pour afficher une liste de monome  qui répresente le polynome  *)
let print_monomes monomes =
  List.iter print_point monomes


(* Appel de la fonction *)
let () = print_monomes polynome;;
