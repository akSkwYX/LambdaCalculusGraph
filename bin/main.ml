(* ---------- Tests ---------- *)

let test _ =
  let module Lis = Strategy.LeftInnermostStrategy in
  let term_str = "Lf,x.fx" in
  let term = Lis.Lt.of_string term_str in
  let () = print_endline ("Term : " ^ (Lis.Lt.to_string_tree term)) in
  let () = print_endline ("Term : " ^ (Lis.Lt.to_string term)) in ()

(* ---------- All reduction possibilities ---------- *)

let reduction_possibilities_viewer preterm =
  let module Ns = Strategy.NoStrategy in

  let exit = ref false in

  let process lambda =
    if List.mem lambda ["exit"; "q"; "quit"] then
      exit := true
    else
      let term = Ns.Lt.of_string lambda in
      let () = print_endline ("Term : \n" ^ (Ns.Lt.to_string_tree term)) in
      let reduce_possibilities = Ns.reduce_step term in
      let () = print_endline "Reduction possibilities :" in
      List.iter (fun t -> print_endline (Ns.Lt.to_string t)) reduce_possibilities
  in

  let () = print_endline "Lambda calculus reduction possibilities viewer" in
  match preterm with
  | "" ->
    while not !exit do
      let () = print_string "> " in
      let lambda = read_line () in
      process lambda
    done
  | _ -> process preterm

(* ---------- Reduction graph ---------- *)

let reduction_graph_viewer preterm =
  let module Ns = Strategy.NoStrategy in

  let exit = ref false in

  let process lambda =
    if List.mem lambda ["exit"; "q"; "quit"] then
      exit := true
    else
      let hst, g = Ns.reduce_graph lambda in
      let () = print_endline "Node map :" in
      let () = Hashtbl.iter (fun k v -> 
        print_endline (string_of_int v ^ " : " ^ (Ns.Lt.to_string (Ns.Lt.of_deBruijn k)))) hst in
      let () = print_newline () in
      let () = print_endline "Graph :" in
      let () = print_endline (Graph.Graph.to_string g) in
      let reverse_hst = Hashtbl.fold (fun k v acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (Hashtbl.length hst)) in
      let file = open_out "results/node_map.txt" in
      let () = Hashtbl.iter (fun k v -> 
        output_string file (string_of_int v ^ " : " ^ (Ns.Lt.to_string (Ns.Lt.of_deBruijn k)) ^ "\n")) hst in
      let () = close_out file in
      Graph.Graph.to_pdf g reverse_hst "reduction_graph"; ()
      (* let _ = Sys.command "xdg-open results/reduction_graph.pdf &" in () *)
  in

  let () = print_endline "Lambda calculus reduction graph viewer" in
  match preterm with
  | "" ->
    while not !exit do
      let () = print_string "> " in
      let lambda = read_line () in
      process lambda
    done
  | _ -> process preterm

(* ---------- A* ---------- *)

let astar preterm =
  let module Ns = Strategy.NoStrategy in

  let exit = ref false in

  let process lambda =
    if List.mem lambda ["exit"; "q"; "quit"] then
      exit := true
    else
      let hst, g, steps = Ns.astar lambda in
      let () = print_endline "Node map :" in
      let () = Hashtbl.iter (fun k (v, _) -> 
        print_endline (string_of_int v ^ " : " ^ (Ns.Lt.to_string (Ns.Lt.of_deBruijn k)))) hst in
      let () = print_newline () in
      let () = print_endline "Graph :" in
      let () = print_endline (Graph.Graph.to_string g) in
      let () = print_newline () in
      let () = print_endline ("Minimum step required to derivate term : " ^ (string_of_int steps)) in
      let reverse_hst = Hashtbl.fold (fun k (v, _) acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (Hashtbl.length hst)) in
      let file = open_out "results/astar_node_map.txt" in
      let () = Hashtbl.iter (fun k (v, _) -> 
        output_string file (string_of_int v ^ " : " ^ (Ns.Lt.to_string (Ns.Lt.of_deBruijn k)) ^ "\n")) hst in
      let () = close_out file in
      Graph.Graph.to_pdf g reverse_hst "astar_graph"; ()
      (* let _ = Sys.command "xdg-open results/reduction_graph.pdf &" in () *)
  in

  let () = print_endline "Lambda calculus reduction graph viewer" in
  match preterm with
  | "" ->
    while not !exit do
      let () = print_string "> " in
      let lambda = read_line () in
      process lambda
    done
  | _ -> process preterm

(* ---------- DeBruijn tests ---------- *)

(* open LambdaTerm.Left *)
(**)
(* let lambda = "Lx.(Lx.x(Lx.xx)(Ly.xy))" *)
(* let de_bruijn = deBruijn_string (of_string lambda) *)
(* let of_de_bruijn = of_deBruijn_string de_bruijn *)
(* let back_to_string = to_string of_de_bruijn *)
(**)
(* let () = print_endline de_bruijn *)
(* let () = print_endline back_to_string *)

(* ---------- Extended Lambda Term ---------- *)

let extend_lambda_term lambda =
  let open LambdaTerm.LambdaTerm in
  let term = of_string lambda in
  print_endline (to_ugly_string term)

(* ---------- Reduction ---------- *)

let reduction_with_chosen_strategy preterm strategy =
  let module Lis = Strategy.LeftInnermostStrategy in
  let module Les = Strategy.LeftExternalStrategy in

  let rec choose_strategy () =
    let () = print_endline "Select reduction strategy :\n1 : Left Innermost Strategy\n2 : Left External Strategy" in
    match read_line () with
    | "exit" | "q" | "quit" -> exit 0
    | "1" -> "Lis"
    | "2" -> "Les"
    | _ -> print_endline "Invalid strategy"; choose_strategy ()
  in
  let strategy = match strategy with | "" -> choose_strategy () | _ -> strategy in

  let exit = ref false in

  let process lambda =
    if List.mem lambda ["exit"; "q"; "quit"] then
      exit := true
    else
      let reverse_hst hst = Hashtbl.fold (fun k v acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (Hashtbl.length hst)) in
      if strategy = "Lis" then
        let (hst, g) = Lis.reduce_string_graph lambda in
        Graph.Graph.to_pdf g (reverse_hst hst) "reduction_graph_Lis";
        let _ = Sys.command "xdg-open results/reduction_graph_Lis.pdf &" in ()
      else
        let (hst, g) = Les.reduce_string_graph lambda in
        Graph.Graph.to_pdf g (reverse_hst hst) "reduction_graph_Les";
        let _ = Sys.command "xdg-open results/reduction_graph_Les.pdf &" in ()
  in

  let () = print_endline "Lambda calculus reduction with chosen strategy" in

  match preterm with
  | "" ->
    while not !exit do
      let () = print_string "> " in
      let lambda = read_line () in
      process lambda
    done
  | _ -> process preterm

(* ---------- Launch ---------- *)

let () = print_endline "Starting Lambda Calculus"
let () = print_endline "Enter 'exit', 'q' or 'quit' to leave the program"
let rec mode_choice choice term strategy =
  match choice with
  | "exit" | "q" | "quit" -> exit 0
  | "1" -> reduction_graph_viewer term
  | "2" -> astar term
  | "3" -> reduction_with_chosen_strategy term strategy
  | "4" -> reduction_possibilities_viewer term
  | "5" -> extend_lambda_term term
  | "6" -> test term
  | _ ->
    let () = print_endline "Enter desired mode :\n1 : Reduction graph viewer\n2 : A*\n3 : Reduction with chosen strategy (Left Innermost or Left External)\n4 : All reduction possibilities viewer" in
    let () = print_string "> " in
    match read_line () with
    | "exit" | "q" | "quit" -> exit 0
    | "1" -> astar ""
    | "2" -> reduction_graph_viewer ""
    | "3" -> reduction_with_chosen_strategy "" ""
    | "4" -> reduction_possibilities_viewer ""
    | _ -> print_endline "Invalid mode"; mode_choice "" "" ""

let prechoice = try Sys.argv.(1) with | Invalid_argument _ -> ""
let preterm = try Sys.argv.(2) with | Invalid_argument _ -> ""
let strategy = try Sys.argv.(3) with | Invalid_argument _ -> ""
let () = mode_choice prechoice preterm strategy
