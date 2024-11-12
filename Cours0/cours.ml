
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

(*Lire et afficher une chaine de caractere *)
let chaine=read_line();;
print_endline chaine;;


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

(* Fonction *)
  let calculator (input_one : int) (input_two : int) : int =
    input_one*input_two;;
    
    print_int (calculator 5 5 );;




    
let calculator (input_one : int) (input_two : int) : int =
  (*Gérer le Scoop , toujours lorsque on utilise une variable dans une fonction on la mets dans in *)
  let x= input_one*input_two in 
      x;;
print_int (calculator 5 5 );;




(* Ce le match corssepond à la Switch case ( conditional statement ) dans C,php....  *)
(* Deux type de Match peuvent etre : *)
(* Premier match : un match exhaustive qui traite tout les cas possible  de la variable que on veut faire le matching *)
(* Deuxième match : un match non exhaustive qui ne traite pas tout les cas possible de la variable que on veut faire le matching *)

(* Exemple de match exhaustive *)
let is_it_Two (x : int ) : string =
  match  x with
  | 2 -> "There's two ! "
  | _ -> "This is not two !!!!";; (*Le underscore signifie tout le reste de cas possible que x peut prendre 0,1,3,4,5,6,...........*)

  print_string (is_it_Two 5);;
  

  (*Exemple de match non exhaustive *)

  let is_it_Three (x:int) : string =
      match x with
      | 0 -> "false"
      | 3 -> "True";;

      (*Cela va engendrer un erreur car 5 est un cas pas traité é *)
      print_string ( is_it_Three 5 );;


      
let y : int list=[1;2;3;4;5];;

let is_list_empty ( y: int list ) : string =
    match y with
    | [] -> "le tableau est vide"
    | h::t-> "le tableau n'est pas vide ";;

     (* print_string (is_list_empty y);; *)


    let rec taille_list ( aa : int list) : int =
        match aa with
       | [] -> 0
       | h::t -> 1 + ( taille_list t ) ;;

(*       print_int (taille_list y);; *)


let xxx=258;;
let rec sum (aa: int list) : int =
     match aa with
    | [] -> 0
    | h::t -> h+ sum(t);;

print_int (sum y );;



