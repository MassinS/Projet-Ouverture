open Main1;;
open Main2;;
open Main3;;

(****************************************cle*******************************************************************)
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
let somme_arbres_strategie1 arbres   =
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


(*Strategie 3*)

(* Fusionner tous les arbres avant d'effectuer l'addition *)
(* le principe est : Tous les arbres sont d'abord fusionnés en un seul polynôme (liste). La canonisation est appliquée ensuite pour simplifier le polynôme résultant.*)
let somme_arbres_strategie3 arbres  =
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


(* Fonction pour générer les données *)
let genere_data somme_arbres_strategie ns =
  List.map
    (fun n ->
      let execution_time = mesure_temps_somme_strategie somme_arbres_strategie n in
      (n, execution_time)
    )
    ns
;;

(* Générer les données pour chaque stratégie *)
let () =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in

  (* Stratégies et noms *)
  let strategies  = [
    ("Stratégie somme 1", somme_arbres_strategie1);
    ("Stratégie somme 2", somme_arbres_strategie2);
    ("Stratégie somme 3", somme_arbres_strategie3)
  ] in

  (* Générer et afficher les données pour chaque stratégie *)
  List.iter
    (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      let data = genere_data strategie ns in
      List.iter
        (fun (n, time) ->
          Printf.printf "n = %d, temps = %.6f secondes\n" n time
        )
        data;

      (* Affichage ou stockage des données dans une variable *)
      if nom = "Stratégie somme 1" then
        let data1 = data in
        Printf.printf "Data 1 : %s\n" (String.concat ", " (List.map (fun (n, t) -> Printf.sprintf "(%d, %.6f)" n t) data1))
      else if nom = "Stratégie somme 2" then
        let data2 = data in
        Printf.printf "Data 2 : %s\n" (String.concat ", " (List.map (fun (n, t) -> Printf.sprintf "(%d, %.6f)" n t) data2))
      else if nom = "Stratégie somme 3" then
        let data3 = data in
        Printf.printf "Data 3 : %s\n" (String.concat ", " (List.map (fun (n, t) -> Printf.sprintf "(%d, %.6f)" n t) data3))
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
  let termes_fusionnes   =
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
    let permutation = List.map (fun _ -> Random.int 1000) (gen_permutation taille |> snd) in
    abr permutation Vide
  ) tailles;;

  
let arbres_etiq = List.map etiquetage (generateurs_arbre tailles);;
let arbres_transf = List.map gen_arb arbres_etiq;;

(*Question 2.17*)

let mesure_temps_somme15_strategie somme_arbres_strategie _ =
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
let mesure_temps_produit15_strategie produit_arbres_strategie _ =
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

