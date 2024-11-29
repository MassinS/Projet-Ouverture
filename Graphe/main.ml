open Graphics;;

(* Données *)
let data = [
  (100, 0.000000);
  (200, 0.000000);
  (300, 0.000000);
  (400, 0.016000);
  (500, 0.000000);
  (600, 0.015000);
  (700, 0.016000);
  (800, 0.015000);
  (900, 0.01000);
  (1000, 0.016000);
]

let data1 = [
  (100, 0.000000);
  (200, 0.000000);
  (300, 0.000000);
  (400, 0.015000);
  (500, 0.016000);
  (600, 0.015000);
  (700, 0.016000);
  (800, 0.015000);
  (900, 0.032000);
  (1000, 0.031000);
]
let data2 = [
  (100, 0.000000);
  (200, 0.000000);
  (300, 0.000000);
  (400, 0.000000);
  (500, 0.000000);
  (600, 0.000000);
  (700, 0.000000);
  (800, 0.015000);
  (900, 0.016000);
  (1000, 0.016000);
]

(* Dimensions du graphique *)
let width = 1300;;
let height = 800;;
let margin = 80;; (* marge autour du graphique *)

(* Initialisation de la fenêtre *)
let () = open_graph (Printf.sprintf " %dx%d" width height);;

(* Tracer les axes *)
let draw_axes () =
  set_color black;
  moveto margin margin;
  lineto margin (height - margin); (* Axe Y *)
  moveto margin margin;
  lineto (width - margin) margin; (* Axe X *)
;;

(* Dessiner le graphique *)
let draw_graph data =
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
      set_color blue;
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
  ) data;
;;

(* Programme principal *)
let () =
  draw_axes ();
  draw_graph data;
  draw_graph data1;
  draw_graph data2;
  
  ignore (read_key ()); (* Attendre une touche pour fermer la fenêtre *)
  close_graph ()
;;