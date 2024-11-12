



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
