open Sys
(* Code pris pour l'utiliser dans l'experimentation*)

type monome = { coeff : int; puiss : int };;

let polynome : monome list = [{ coeff = 0; puiss = 4 }; { coeff = 4; puiss = 2 }; { coeff = 2; puiss = 2 }; { coeff = 8; puiss = 221 }   ];;

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


let poly_add ( p1 : monome list ) ( p2 : monome list) : monome list = 
 polynome_canonique( p1 @ p2 )
;;

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

type 'a arbre =
| Vide
| Noeud of 'a * 'a arbre * 'a arbre;; 

type 'a btree =
  | Empty
  | Node of 'a * 'a btree list ;;
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

let rec arb2monomes tree =
    match tree with
    | Empty -> []
    | Node (value, children) -> (
        match value with
        | "+" -> List.flatten (List.map arb2monomes children)
        
        | "*" -> 
            let terms = List.map arb2monomes children in
            let rec multiplier lst1 lst2 =
              match lst1, lst2 with
              | [], _ | _, [] -> []
              | m1::rest1, m2::rest2 -> 
                  { coeff = m1.coeff * m2.coeff; puiss = m1.puiss + m2.puiss } 
                  :: (multiplier rest1 (m2::rest2) @ multiplier rest1 rest2)
            in List.fold_left (fun acc monomes -> multiplier acc monomes) [{coeff=1; puiss=0}] terms
        | "^" -> 
            let base, exp = List.hd children, List.hd (List.tl children) in
            (match base, arb2monomes exp with
             | Node("x", []), [{coeff=n; puiss=0}] -> [{ coeff = 1; puiss = n }]
             | _ -> [])
        | _ -> 
            try [{ coeff = int_of_string value; puiss = 0 }]
            with Failure _ -> [{ coeff = 1; puiss = 1 }]
      )


let arb2poly tree =
  polynome_canonique (arb2monomes tree)

let rec supp_indice_ele l n =
  match l with
  | [] -> failwith "Index out of bounds"
  | x :: xs -> if n = 0 then (xs, x) else 
                  let (xs', y) = supp_indice_ele xs (n - 1) in
                  (x :: xs', y);;      
  
  let extraction_alea l1 l2 =
    if l1 = [] then (l1, l2)  
    else
      let random_ele = Random.int (List.length l1) in
      let (nouveau_l1, element_supprimer) = supp_indice_ele l1 random_ele in
      (nouveau_l1, element_supprimer :: l2);;

let  gen_permutation n =
let rec generation n =
  match n with
  | 0 -> []
  | n -> generation (n - 1) @ [n] 
in
let p = [] and l = generation n in
let rec permu (l1, l2) =
  match l1 with
  | [] -> ([], l2)
  | _ -> 
    let (nouveau_l1, elem) = extraction_alea l1 l2 in 
    permu (nouveau_l1, (elem))
in
permu (l, p);;



let rec inserer valeur arbre =
match arbre with
  | Vide -> Noeud(valeur, Vide, Vide)
  | Noeud(x, g, d) ->
  if valeur < x then Noeud(x, inserer valeur g, d)
  else Noeud(x, g, inserer valeur d)
        
let rec abr liste arbre =
 match liste with
   | [] -> arbre
   | x::xs -> abr xs (inserer x arbre);;

let rec taille arbre =
  match arbre with
  | Vide -> 0
  | Noeud(x,g,d) -> 1 + taille g + taille d
;; 

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
        let probability = Random.int 4 in 
        if(probability<3) then
          Noeud("+",etiquetage g,etiquetage d)
        else
        Noeud("*", etiquetage g, etiquetage d)
      ;;

let rec gen_arb arbre =
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
            

(***********************************************************************************************************)
(*Question 2.13*)

(* Fonction pour générer un ABR de taille 20 avec des valeurs aléatoires *)
let gen_abr_taille_20 () =
  let rec gen_abr_rec taille acc =
    if taille = 0 then acc
    else
      let valeur = Random.int 100 in (* Génère un nombre aléatoire entre 0 et 100 *)
      gen_abr_rec (taille - 1) (inserer valeur acc)
  in
  gen_abr_rec 20 Vide

(* Fonction pour générer n ABR de taille 20, les transformer avec etiquetage et gen_arb *)
let gen_n_abr n =
  let rec gen_n_abr_rec n acc =
    if n = 0 then acc
    else
      let arbre = gen_abr_taille_20 () in
      let arbre_etiquete = etiquetage arbre in
      let arbre_transforme = gen_arb arbre_etiquete in
      gen_n_abr_rec (n - 1) (arbre_transforme :: acc)
  in
  gen_n_abr_rec n []

(* Fonction pour tester plusieurs valeurs de n : n=[100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] *)
let test_n_list n_list =
  List.iter (fun n ->
    let _ = gen_n_abr n in (* Appeler la fonction et ignorer la valeur de retour *)
    Printf.printf "Finished generating and transforming %d trees\n" n;
  ) n_list

(* Exemple d'utilisation avec la liste des tailles de n *)
let n_list = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000]

let () = test_n_list n_list

(**********************************************************************)

(*Question 2.14*)
(*Strategie 1 : Conversion des arbres en polynômes puis addition globale *)
let somme_arbres_strategie1 arbres =
  let polynomes= 
    List.map (fun arbre -> arb2poly arbre
    ) arbres
  in
  List.fold_left poly_add [] polynomes
;; 

(*Strategie 2*: Addition au fur et à mesure *)
(* le principe est : On convertit chaque arbre en polynôme avec arb2poly et on l'ajoute immédiatement au polynôme cumulatif.*)
let somme_arbres_strategie2 arbres =
  List.fold_left
    (fun cumul arbre ->
      let poly = arb2poly arbre in
      poly_add cumul poly
    )
    [] (* Polynôme initial vide *)
    arbres
;;


(*Strategie 3*)

(* Fusionner tous les arbres avant d'effectuer l'addition *)
(* le principe est : Tous les arbres sont d'abord fusionnés en un seul polynôme (liste). La canonisation est appliquée ensuite pour simplifier le polynôme résultant.*)
let somme_arbres_strategie3 arbres =
  (* Fusionner les arbres en un seul arbre en utilisant une addition directe *)
  List.fold_left (fun acc arbre -> 
    arb2poly arbre @ acc  (* Ajouter chaque arbre sous forme de polynôme *)
  ) [] arbres


(*Mesuring time*)
let mesure_temps_somme_strategie somme_arbres_strategie n =
  let arbres = gen_n_abr n in
  let start_time = Sys.time () in
  let _ = somme_arbres_strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
;;


(*teste*)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  let strategies = [
    ("Stratégie somme 1", somme_arbres_strategie1);
    ("Stratégie somme 2", somme_arbres_strategie2);
    ("Stratégie somme 3", somme_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      List.iter
        (fun n ->
          let execution_time = mesure_temps_somme_strategie strategie n in
          Printf.printf "n = %d, temps = %.6f secondes\n" n execution_time;
        )
        ns
    )
    strategies
;;
  
  
(*Question 2.15*)
(*Stratégie 1*)
let produit_arbres_strategie1 arbres =
  let polynomes = List.map arb2poly arbres in
  List.fold_left poly_prod [{coeff=1; puiss=0}] polynomes
;;

(*Strategie 2*)

let produit_arbres_strategie2 arbres =
  List.fold_left
    (fun cumul arbre ->
      let poly = arb2poly arbre in
      poly_prod cumul poly
    )
    [{coeff=1; puiss=0}] (* Polynôme initial unité *)
    arbres
;;

(*Strategie 3*)

let produit_arbres_strategie3 arbres =
  let termes_fusionnes =
    List.fold_left (fun acc arbre -> arb2poly arbre @ acc) [] arbres
  in
  polynome_canonique termes_fusionnes
;;

(*Mesuring time*)

let mesure_temps_produit_strategie produit_arbres_strategie n =
  let arbres = gen_n_abr n in
  let start_time = Sys.time () in
  let _ = produit_arbres_strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
;;

(*teste*)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  let strategies = [
    ("Stratégie produit 1", produit_arbres_strategie1);
    ("Stratégie produit 2", produit_arbres_strategie2);
    ("Stratégie produit 3", produit_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      List.iter
        (fun n ->
          let execution_time = mesure_temps_produit_strategie strategie n in
          Printf.printf "n = %d, temps = %.6f secondes\n" n execution_time;
        )
        ns
    )
    strategies
;;


(*Question 2.16*)

(* Génération de 15 ABR de tailles spécifiques 1,1,2,4,8,...2^13 *)
let tailles = 1 :: List.init 14 (fun i -> 1 lsl i);;

let generateurs_arbre tailles =
  List.map (fun taille ->
    let permutation = List.map (fun x -> Random.int 1000) (gen_permutation taille |> snd) in
    abr permutation Vide
  ) tailles;;

let arbres_etiq = List.map etiquetage (generateurs_arbre tailles);;
let arbres_transf = List.map gen_arb arbres_etiq;;

(*Question 2.17*)

let mesure_temps_somme15_strategie somme_arbres_strategie n =
  let arbres = arbres_transf in
  let start_time = Sys.time () in
  let _ = somme_arbres_strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
;;


(*teste*)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  let strategies = [
    ("Stratégie somme15 1", somme_arbres_strategie1);
    ("Stratégie somme15 2", somme_arbres_strategie2);
    ("Stratégie somme15 3", somme_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      List.iter
        (fun n ->
          let execution_time = mesure_temps_somme15_strategie strategie n in
          Printf.printf "n = %d, temps = %.6f secondes\n" n execution_time;
        )
        ns
    )
    strategies
;;

(*Question 2.18*)
let mesure_temps_produit15_strategie produit_arbres_strategie n =
  let arbres = arbres_transf in
  let start_time = Sys.time () in
  let _ = produit_arbres_strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
;;


(*teste*)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  let strategies = [
    ("Stratégie produit15 1", produit_arbres_strategie1);
    ("Stratégie produit15 2", produit_arbres_strategie2);
    ("Stratégie produit15 3", produit_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      List.iter
        (fun n ->
          let execution_time = mesure_temps_produit15_strategie strategie n in
          Printf.printf "n = %d, temps = %.6f secondes\n" n execution_time;
        )
        ns
    )
    strategies
;;