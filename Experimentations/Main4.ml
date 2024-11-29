open Sys;;
open Main1;;
open Main2;;
open Main3;;

(**********************************************************************)
(*Question 2.13*)

(* Fonction pour générer un ABR de taille 20 avec des valeurs aléatoires *)
let gen_abr_taille_20 ()  =
  let rec gen_abr_rec taille acc =
    if taille = 0 then acc
    else
      let valeur = Random.int 100 in
      gen_abr_rec (taille - 1) (inserer valeur acc)
  in
  gen_abr_rec 20 Vide

(* Fonction pour générer n ABR de taille 20, les transformer avec etiquetage et gen_arb *)
let gen_n_abr n  =
  let rec gen_n_abr_rec n acc =
    if n = 0 then acc
    else
      let arbre = gen_abr_taille_20 () in
      let arbre_etiquete  = etiquetage arbre in
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
  let polynomes = List.map arb2poly arbres in
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

(*Strategie 3: Fusionner tous les arbres avant d'effectuer l'addition*)
(* le principe est : Tous les arbres sont d'abord fusionnés en un seul polynôme (liste). La canonisation est appliquée ensuite pour simplifier le polynôme résultant.*)
let somme_arbres_strategie3 arbres  =
  (* Fusionner les arbres en un seul arbre en utilisant une addition directe *)
  List.fold_left (fun acc arbre -> 
    arb2poly arbre @ acc  (* Ajouter chaque arbre sous forme de polynôme *)
  ) [] arbres


(*Mesuring time*)
let mesure_temps_n strategie n =
  let arbres = gen_n_abr n in
  let start_time = Sys.time () in
  let _ = strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
;;


(*teste*)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  let strategies  = [
    ("SommeN Stratégie 1", somme_arbres_strategie1);
    ("SommeN Stratégie 2", somme_arbres_strategie2);
    ("SommeN Stratégie 3", somme_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      List.iter
        (fun n ->
          let execution_time = mesure_temps_n strategie n in
          Printf.printf "n = %d, temps = %.6f secondes\n" n execution_time;
        )
        ns
    )
    strategies
;;
  
(**********************************************************************)
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
  let termes_fusionnes = List.fold_left (fun acc arbre -> arb2poly arbre @ acc) [] arbres
  in
  polynome_canonique termes_fusionnes
;;

(*teste*)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  let strategies = [
    ("ProduitN Stratégie 1", produit_arbres_strategie1);
    ("ProduitN Stratégie 2", produit_arbres_strategie2);
    ("ProduitN Stratégie 3", produit_arbres_strategie3)
  ] in
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      List.iter
        (fun n ->
          let execution_time = mesure_temps_n strategie n in
          Printf.printf "n = %d, temps = %.6f secondes\n" n execution_time;
        )
        ns
    )
    strategies
;;

(**********************************************************************)
(*Question 2.16*)

(* Génération de 15 ABR de tailles spécifiques 1,1,2,4,8,...2^13 *)
let tailles = 1 :: List.init 14 (fun i -> 1 lsl i);;

let generateurs_arbre tailles =
  List.map (fun taille ->
    let permutation = List.map (fun _ -> Random.int 1000) (gen_permutation taille |> snd) in
    abr permutation Vide
  ) tailles;;

let arbres_etiq = List.map etiquetage (generateurs_arbre tailles);;
let arbres_transf = List.map gen_arb arbres_etiq;;

(**********************************************************************)
(*Question 2.17*)

let mesure_temps_15 strategie =
  let arbres = arbres_transf in
  let start_time = Sys.time () in
  let _ = strategie arbres in
  let end_time = Sys.time () in
  end_time -. start_time
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

(**********************************************************************)
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