
(* 1.1 Polynôme sous forme linéaire :  *)

(* Question 1.1 :  *)

(* Ici j'ai décidé de définir une structure de donnée qui
 répresente un polynome en Liste et cette liste 
 est un ensemble d'enregsitrement qui est déjà défini 
*)

(* Le type monome répresente le polynome *)
type monome = { coeff : int; puiss : int };;

let polynome : monome list = [{ coeff = 0; puiss = 4 }; { coeff = 4; puiss = 2 }; { coeff = 2; puiss = 2 }; { coeff = 8; puiss = 221 }   ];;


(* Question 1.2   *)

(* Transformer n'importe quel polynome en polynome canonique  *)

(*La complexité pour filtre est linéaire O(n) *)
(*La complexité pour le trie est n*log(n) , la primitive List.sort utilise une Tri par fusion et la complexité de ce Trie est n*log(n) dans tout les cas *)

(*La complexité pour fusionner est O(n) car 
La fonction "fusionner" ne fait que parcourir la liste trie et 
pour chaque element on a un appel récursif donc pour n élements on aura O(n) appel récursif 
et Chaque appel à fusionner effectue une comparaison de puissance qui est 𝑂(1)
et crée éventuellement un nouveau monome (aussi 𝑂(1)  en additionnant les coefficients. Donc la complexité est de O(n) *)

(* Donc la compléxité de la fonction polynome_canonique est de O(nlog(n)) *)
let polynome_canonique (p : monome list) : monome list =  
     let polynome_filtred = List.filter (fun monome -> monome.coeff <> 0)p in 
     let polynome_trie = List.sort (fun m1 m2 -> compare m1.puiss m2.puiss) polynome_filtred in
     let rec fusionner polynome_trie  =  
     match  polynome_trie with
       | [] -> [] 
       | [monome] -> [monome]
       | monome1 :: monome2 :: rest ->
        if monome1.puiss == monome2.puiss then
          fusionner ({ coeff = monome1.coeff + monome2.coeff; puiss = monome1.puiss } :: rest)
        else
          monome1 :: fusionner (monome2 :: rest)
  in
  fusionner polynome_trie
;;


(* Question 1.3 *)

(* Additionner deux polynome *)
(*La complexité sera de n+m*(log(n+m))  *)
let poly_add ( p1 : monome list ) ( p2 : monome list) : monome list = 

 polynome_canonique( p1 @ p2 )
;;


(* Question 1.4 *)

(*La complexité est (n*m)*(log(n*m)) car la produit contient deux boucle imbriqué donc n*m ensuite le résultat est mis dans polynome_canonique   *)
let poly_prod (p1 : monome list ) ( p2 : monome list ) : monome list = 
 
  let  produit  = 
  List.flatten (
      List.map(fun (m1) ->   
      List.map(fun (m2) -> 
        { coeff = m1.coeff * m2.coeff; puiss = m1.puiss + m2.puiss }  
          ) p2
          ) p1
          )
  in 
    polynome_canonique ( produit  )  
;;




(* Test de la fonction poly_add *)
let polynome1 = [   { coeff = 5; puiss = 1 };  { coeff = 3; puiss = 2 };  { coeff = 5; puiss = 1 }; { coeff = 5; puiss = 1 }; { coeff = 5; puiss = 1 }; { coeff = 5; puiss = 1 }; { coeff = 4; puiss = 5 }] ;;
let polynome2 = [{ coeff = 7; puiss = 5 }; { coeff = 4; puiss = 2 }; { coeff = 1; puiss = 0 }; { coeff = 5; puiss = 1 } ; { coeff = 5; puiss = 1 }; { coeff = 5; puiss = 1 }; { coeff = 5; puiss = 1 }] ;;

Printf.printf "La fusion de deux polynome : \n";;
let result = poly_add polynome1 polynome2;;
List.iter (fun m -> Printf.printf "Coeff: %d, Puiss: %d\n" m.coeff m.puiss) result;;


Printf.printf "Le produit de deux polynome : \n";;
let result1 = poly_prod polynome1 polynome2;;
List.iter (fun m -> Printf.printf "Coeff: %d, Puiss: %d\n" m.coeff m.puiss) result1;;

