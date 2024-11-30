open Graphics
open Main4

(* Dimensions du graphique *)
let width = 1300
let height = 800
let margin = 80 (* marge autour du graphique *)

let genere__somme_data somme_arbres_strategie ns =
  List.map
    (fun n ->
      let execution_time = mesure_temps_somme_strategie somme_arbres_strategie n in
      (n, execution_time)
    )
    ns
;;






(* Initialisation de la fenêtre *)
let () = open_graph (Printf.sprintf " %dx%d" width height)

(* Tracer les axes *)
let draw_axes () =
  set_color black;
  moveto margin margin;
  lineto margin (height - margin); (* Axe Y *)
  moveto margin margin;
  lineto (width - margin) margin (* Axe X *)
;;

(* Dessiner le graphique *)
let draw_graph data color =
  (* Déterminer les limites des axes *)
  let max_n = List.fold_left (fun acc (n, _) -> max acc n) 0 data in
  let max_temps = List.fold_left (fun acc (_, t) -> max acc t) 0.0 data in

  (* Fonctions de transformation des coordonnées *)
  let scale_x n = margin + (n * (width - 2 * margin)) / max_n in
  let scale_y t = margin + int_of_float (t *. float_of_int (height - 2 * margin) /. max_temps) in

  (* Dessiner les points *)
  let rec draw_points = function
    | [] | [_] -> () (* Rien à faire si 0 ou 1 point *)
    | (x1, y1) :: ((x2, y2) :: _ as rest) ->
      let x1', y1' = scale_x x1, scale_y y1 in
      let x2', y2' = scale_x x2, scale_y y2 in
      set_color color;
      moveto x1' y1';
      lineto x2' y2';
      draw_points rest
  in
  draw_points data;

  (* Afficher les points *)
  List.iter (fun (n, t) ->
    let x, y = scale_x n, scale_y t in
    set_color red;
    fill_circle x y 3;
    set_color black;
    moveto (x + 5) (y + 5);
    draw_string (Printf.sprintf "(%d, %.3f)" n t);
  ) data
;;

(* Ajouter une légende au graphique *)
let draw_legend legend_items =
  let legend_x = 100 in
  let legend_y_start = height - margin - 30 in
  let line_height = 20 in
  List.iteri (fun i (color, text) ->
    let y = legend_y_start - (i * line_height) in
    set_color color;
    fill_rect legend_x y 15 15; (* Dessiner une case colorée *)
    set_color black;
    moveto (legend_x + 20) (y + 2); (* Positionner le texte *)
    draw_string text
  ) legend_items
;;


(* Générer les données pour chaque stratégie *)
let data1, data2, data3 =
  Random.self_init ();
  let ns = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in

  (* Stratégies et noms *)
  let strategies = [
    ("Stratégie somme 1", somme_arbres_strategie1);
    ("Stratégie somme 2", somme_arbres_strategie2);
    ("Stratégie somme 3", somme_arbres_strategie3);
  ] in

  (* Générer et collecter les données *)
  let results = List.map (fun (nom, strategie) ->
      Printf.printf "==== %s ====\n" nom;
      let data = genere__somme_data strategie ns in
      List.iter (fun (n, time) ->
        Printf.printf "n = %d, temps = %.6f secondes\n" n time
      ) data;
      data
    ) strategies
  in
  match results with
  | [d1; d2; d3] -> (d1, d2, d3)
  | _ -> failwith "Erreur lors de la génération des données"
;;

(* Programme principal *)
let () =
  draw_axes ();
  draw_graph data1 blue; (* Dessiner la stratégie 1 en bleu *)
  draw_graph data2 green; (* Dessiner la stratégie 2 en vert *)
  draw_graph data3 cyan; (* Dessiner la stratégie 3 en cyan *)

  draw_legend [
    (blue, "Strategie somme 1");
    (green, "Strategie somme 2");
    (cyan, "Strategie somme 3");
  ];

  ignore (read_key ()); (* Attendre une touche pour fermer la fenêtre *)
  close_graph ()
;;
