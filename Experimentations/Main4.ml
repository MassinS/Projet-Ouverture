open Main1;;
open Main2;;
open Main3;;

(****************************************cle*******************************************************************)
(*Question 2.13*)

(* Fonction pour générer n ABR de taille 20, les transformer avec etiquetage et gen_arb *)
let gen_n_abr n  =
  let rec gen_n_abr_rec n acc =
    if n = 0 then acc
    else
      let liste_permutee = snd (gen_permutation 20) in (* ça génere une liste de taile 20*)
      let arbre = abr liste_permutee Vide in (* le transforme en arbre binaire*)
      let arbre_etiquete  = etiquetage arbre in (* l'étiquetage de l'arbre*)
      let arbre_transforme = gen_arb arbre_etiquete in (* transformation pour réspecter la grammaire*)
      gen_n_abr_rec (n - 1) (arbre_transforme :: acc) (* géneration recursive des n abr de taille 20*)
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
let somme_arbres_strategie1 arbres   =
  let polynomes = List.map arb2poly arbres in (* une liste polynomes où chaque élément est le polynôme correspondant à un arbre de la liste arbres*)
  List.fold_left poly_add [] polynomes (* on applique poly_add sur chaque element de la liste des polynôme*)
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

(* le principe est : Tous les arbres sont d'abord fusionnés en un seul polynôme (liste). La canonisation est appliquée ensuite pour simplifier le polynôme résultant.*)
let somme_arbres_strategie3 arbres  =
  (* Fusionner les arbres en un seul arbre en utilisant une addition directe *)
  let termes_fusionnes   =
    List.fold_left (fun acc arbre -> arb2poly arbre @ acc  (* Ajouter chaque arbre sous forme de polynôme *)
  ) [] arbres
  in
  polynome_canonique termes_fusionnes
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
  let rec produit_intermediaire arbres cumul =
    match arbres with
    | [] -> cumul  (* Quand il n'y a plus d'arbres à traiter, on renvoie le résultat final *)
    | arbre :: reste ->
        let poly = arb2poly arbre in
        let produit_partiel = poly_prod cumul poly in  (* On multiplie le polynôme cumulatif avec le polynôme de l'arbre courant *)
        produit_intermediaire reste produit_partiel  (* On continue avec les arbres restants *)
  in
  produit_intermediaire arbres [{coeff=1; puiss=0}]  (* Le polynôme initial est l'unité *)


(*Mesuring time*)
let mesure_temps strategie arbres =
  let start_time = Sys.time () in
  let _ = strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
;;

let mesure_temps_n strategie n =
  let arbres = gen_n_abr n in
  mesure_temps strategie arbres
;;

(*-------------------------------------------------------------------------------------------------------------- *)
(*Question 2.16*)

(* Génération de 15 ABR de tailles spécifiques 1,1,2,4,8,...2^13 *)
let tailles = 1 :: List.init 14 (fun i -> 1 lsl i);;

let generateurs_arbre tailles =
  List.map (fun taille ->
    let permutation = (gen_permutation taille |> snd) in
    abr permutation Vide
  ) tailles;;

  
let arbres_etiq = List.map etiquetage (generateurs_arbre tailles);;
let arbres_transf = List.map gen_arb arbres_etiq;;

(*Question 2.17*)

let mesure_temps_15 strategie =
  mesure_temps strategie arbres_transf
;;

(*teste*)
let () =
  Random.self_init ();
  let strategies = [
    ("Somme15 Stratégie 1", somme_arbres_strategie1);
    ("Somme15 Stratégie 2", somme_arbres_strategie2);
    ("Somme15 Stratégie 3", somme_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
          let execution_time = mesure_temps_15 strategie in
          Printf.printf "temps = %.6f secondes\n" execution_time;
    )
    strategies
;;


(*Question 2.18*)

(*teste*)
let () =
  Random.self_init ();
  let strategies = [
    ("Produit15 Stratégie 1", produit_arbres_strategie1);
    ("Produit15 Stratégie 2", produit_arbres_strategie2);
    ("Produit15 Stratégie 3", produit_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
          let execution_time = mesure_temps_15 strategie in
          Printf.printf "temps = %.6f secondes\n" execution_time;
    )
    strategies
;;


 Printf.printf "==========================================================================\n";;