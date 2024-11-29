
open Main2;;

(*question 1.8*)
let rec supp_indice_ele l n =
  match l with
  | [] -> failwith "Index out of bounds"
  | x :: xs -> if n = 0 then (xs, x) else 
                  let (xs', y) = supp_indice_ele xs (n - 1) in
                  (x :: xs', y);;      (*fonction pour retourner la liste apres la suppression de l'element et l'element supprimer*)
  
  let extraction_alea l1 l2 =
    if l1 = [] then (l1, l2)  (* Cas où la liste est vide *)
    else
      let random_ele = Random.int (List.length l1) in
      let (nouveau_l1, element_supprimer) = supp_indice_ele l1 random_ele in
      (nouveau_l1, element_supprimer :: l2);;

(*question 1.9*)
let  gen_permutation n =
let rec generation n =
  match n with
  | 0 -> []
  | n -> generation (n - 1) @ [n] (*la generation d'une liste ordonne de 1 a n dans l'ordre croissant*)
in
let p = [] and l = generation n in
let rec permu (l1, l2) =
  match l1 with
  | [] -> ([], l2)
  | _ -> 
    let (nouveau_l1, elem) = extraction_alea l1 l2 in (*dayi a chaque fois on prend un element aleatoire g la premiere liste on le mit dans notre liste vide*)
    permu (nouveau_l1, (elem))
in
permu (l, p);;

(*question 1.10*)
type 'a arbre = 
| Vide
| Noeud of 'a * 'a arbre * 'a arbre;; (*definir la structure d'un Arbre binaire*)

let rec inserer valeur arbre =(*pour inserer un seul element dans un arbre*)
match arbre with
  | Vide -> Noeud(valeur, Vide, Vide)
  | Noeud(x, g, d) ->
  if valeur < x then Noeud(x, inserer valeur g, d)
  else Noeud(x, g, inserer valeur d)
        
let rec abr liste arbre =(*fonction principale qui construit un arbre a partir d'une liste*)
 match liste with
   | [] -> arbre
   | x::xs -> abr xs (inserer x arbre);;

(*question 1.11*)
let rec taille arbre =
  match arbre with
  | Vide -> 0
  | Noeud(x,g,d) -> 1 + taille g + taille d
;; (*j'aurai besoin de cette fonction pour savoir a ce que un sous arbre est une feuille ou non*) 

let rec etiquetage arbre =
  match arbre with
  | Vide -> Vide
  | Noeud(x, Vide, Vide) ->
      if Random.bool() then
        Noeud("x", Vide, Vide)
      else
        Noeud(Random.int 100 |> string_of_int, Vide, Vide) 
  | Noeud(x, g, d) ->
      if taille g = 1 && taille d = 1 then
        if x mod 2 = 0 then
          Noeud("^", Noeud("x", Vide, Vide), Noeud(Random.int 100 |> string_of_int, Vide, Vide))
        else
          Noeud("*", Noeud((Random.int 401 - 200)|> string_of_int, Vide, Vide), Noeud("x", Vide, Vide))
      else
        let probability = Random.int 4 in (* 0, 1, 2, 3 *)
        if(probability<3) then
          Noeud("+",etiquetage g,etiquetage d)
        else
        Noeud("*", etiquetage g, etiquetage d)
      ;;


  (*on aura besoin de cette structure car cette derniere  vérifie la grammaire stipulée dans la section 1.2*)
  let gauche_arbre arbre =
    match arbre with
    | Vide -> ("", Vide, Vide)
    | Noeud(_, g, _) -> (
        match g with
        | Vide -> ("", Vide, Vide)
        | Noeud(xg, gg, gd) -> (xg, gg, gd)
      );;
  

      let droite_arbre arbre =
        match arbre with
        | Vide -> ("", Vide, Vide)
        | Noeud(_, _, d) -> (
            match d with
            | Vide -> ("", Vide, Vide)
            | Noeud(xd, dg, dd) -> (xd, dg, dd)
          );;
      

(*ces fonctions me permet de savoir la valeur des sous-arbre pour savoir comment proceder s'il s'agit d'un + successive .... ect*)

let rec gen_arb (arbre )  =
    match arbre with
    | Vide -> Empty
    | Noeud(x, g, d) ->
        let (element_g, gg, gd) = gauche_arbre arbre
        and (element_d, dg, dd) = droite_arbre arbre in
        match (x, element_g, element_d) with
        | ("+","+", "+") -> Node("+", [gen_arb gg] @ [gen_arb gd] @ [gen_arb dg] @ [gen_arb dd])
        | ("+","+", _) -> Node("+", [gen_arb gg] @ [gen_arb gd] @ [gen_arb d])
        | ("+",_,"+") -> Node("+", [gen_arb g] @ [gen_arb dg] @ [gen_arb dd])
        | ("+","x",_)-> Node("+",[Node("^", [Node("x", []); Node("1", [])  ])] @ [gen_arb d])
        | ("+",_,"x")-> Node("+",[gen_arb g] @ [Node("^", [Node("x", []); Node("1", [])  ])])
        | ("+", _, _) -> Node("+", [gen_arb g] @ [gen_arb d])
        | ("*", "*", "*") -> Node("*", [gen_arb gg] @ [gen_arb gd] @ [gen_arb dg] @ [gen_arb dd])
        | ("*", "*", _) -> Node("*", [gen_arb gg] @ [gen_arb gd] @ [gen_arb d])
        | ("*", _, "*") -> Node("*", [gen_arb g] @ [gen_arb dg] @ [gen_arb dd])
        | ("*","x",_)-> Node("*",[Node("^", [Node("x", []); Node("1", [])  ])] @ [gen_arb d])
        | ("*",_,"x")-> Node("*",[gen_arb g] @ [Node("^", [Node("x", []); Node("1", [])  ])])
        | ("*", _, _) -> Node("*", [gen_arb g] @ [gen_arb d])
        | ("^", _, _) -> Node("^", [gen_arb g] @ [gen_arb d])
        | _ -> Node(x, []);;

          
        