
(* Affichage *)
print_string "Hello world";;
print_int (5+5);;
print_char 's';;
print_float 2.2;;
print_endline "Speed";; (*Affiche la chaine de caractère et fait un saut à la ligne *)

(* Expressions *)
5 mod 2;;
5=5;;
2>3;; (* Retourne un Boolean *)


(* Lire une valeur au clavier *)
let plus2 = read_int()+2;;


(* Variables *)
let x = 105;;
let y = 102;;

(* Conditional statement *)

if ( x>y) then print_int x 
else print_int y;;

(* la condition doit avoir un type unique *)
(*Si on avait if(2>1) then 2;; cela engendra une erreur car si la conditoin est vrai -> int mais si la condition es fausse quelle valeur la condition retournerait-t-elle ? *)
(* En OCaml, chaque expression doit avoir un type bien défini.  *)
 if (2>1) then 2 else 1;;

let a= read_int()
let b=read_int();;

if (a>b ) then print_string "a est plus grand que b"  else if (a<b) then print_string " a est plus petit que b " else if (a==b) then print_string "a est égal à b" 
 



let calculer(a,b) =
 if (a>b ) then print_string "a est plus grand que b"  else if (a<b) then print_string " a est plus petit que b " else if (a==b) then print_string "a est égal à b";;
 
calculer(5,3);;



(*Fonction qui prend un unit en argument et retorune un unit *)
let afficher_message () =
  print_endline "Hello, world!";;