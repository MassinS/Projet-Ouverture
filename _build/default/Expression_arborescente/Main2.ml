open Main1;;
(* Question 1.5 *)
(* Question 1.6 *)

(*La structure que j'ai choisi est que pour chaque noeud on a une valeur qui peut être  peu importe (String , int , char ....) 'a
  et un liste de sous arbre 
*)

type 'a btree =
  | Empty
  | Node of 'a * 'a btree list


(* Exemple d'arbre avec plusieurs enfants par nœud *)
let tree = 
  Node(
    "+", 
    [
      Node(
        "*", 
        [
          Node("123", []);
          Node("^", [Node("x", []); Node("1", [])  ])
        ]
      );
      Node(
        "^", 
        [
          Node("x", []);
          Node("3",[])
        ]
      );
      Node("42",[]);

    ]
  )


(* Question 1.7  *)

(*L'idee ici de prendre une arbre et la transformer en forme d'une liste d'enregistrement de monome *)
(*Ensuite on a déjà une fonction qui prend cette liste et la transforme en polynome canonique *)


(* Cette fonction transforme une arbre en une liste de monome  *)
  let rec arb2monomes tree =
    match tree with
    | Empty -> []
    | Node (value, children) -> (
        match value with
        (* List.map arb2monomes children applique arb2monomes à chaque sous-arbre pour récupérer les monomes,
           puis List.flatten aplatit la liste résultante de monomes en une seule liste *)
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


      (*La fonction arb2poly transforme l'expression arborescente et la transformant en un polynôme canonique. *)
      let arb2poly tree =
        polynome_canonique (arb2monomes tree)


  
  
      (*Test*)
      let () = let poly = arb2poly tree in
      List.iter (fun m     -> Printf.printf "{ coeff = %d; puiss = %d }\n" m.coeff m.puiss) poly




        