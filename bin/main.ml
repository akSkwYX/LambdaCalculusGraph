let ( #~ ) = Fun.compose

module type Value = sig
  val v : string
end

(* ---------- Tests ---------- *)

let test _ =
  let module Lis = Strategy.LeftInnermostStrategy in
  let term_str = "Lf,x.fx" in
  let term = Lis.Lt.of_string term_str in
  let () = print_endline ("Term : " ^ (Lis.Lt.to_string_tree term)) in
  let () = print_endline ("Term : " ^ (Lis.Lt.to_string term)) in ()

(* ---------- Lambda tree viewer ---------- *)

module Lambda_tree_viewer (Term : Value) = struct
  let exec () =
    let module LambdaTerm = LambdaTerm.LambdaTerm in
    let term = LambdaTerm.of_string Term.v in
    let () = print_endline ("Term : \n" ^ (LambdaTerm.to_string_tree term)) in ()
end

(* ---------- Reduction graph ---------- *)

module Reduction_graph_viewer (Term : Value) (Strategy : Strategy.NoStrategy) = struct
  let exec () = 
    let hst, g = Strategy.reduce_graph Term.v in
    let reverse_hst = Hashtbl.fold (fun k v acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (Hashtbl.length hst)) in
    (* let file = open_out "results/node_map.txt" in *)
    (* let () = Hashtbl.iter (fun k v ->  *)
    (*   output_string file (string_of_int v ^ " : " ^ (Strategy.Lt.to_string (Strategy.Lt.of_deBruijn k)) ^ "\n")) hst in *)
    (* let () = close_out file in *)
    Graph.Graph.to_pdf g reverse_hst [] "reduction_graph"; ()
    (* let _ = Sys.command "xdg-open results/reduction_graph.pdf &" in () *)
end

(* ---------- A* ---------- *)

module Astar (Term : Value) (Strategy : Strategy.NoStrategy) (Construct_graph : Value) = struct
  let exec () =
    let hst, g, terms, steps = Strategy.astar (bool_of_string Construct_graph.v) Term.v in
    let () = print_endline ("Minimum step required to derivate term : " ^ (string_of_int steps)) in
    let () = print_endline ("Order of graph : " ^ (string_of_int (Graph.Graph.order g))) in
    let () = print_endline ("Size of graph : " ^ (string_of_int (Graph.Graph.size g))) in
    let () = print_endline "Path :" in
    let () = List.iter (print_endline #~ Strategy.Lt.to_string) terms in
    if bool_of_string Construct_graph.v then
      begin
      let reverse_hst = Hashtbl.fold (fun k (v, _, _) acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (Hashtbl.length hst)) in
      Graph.Graph.to_pdf g reverse_hst (List.map Strategy.Lt.deBruijn_index terms) "astar_graph"
      end
end

module IDAstar (Term : Value) (Strategy : Strategy.NoStrategy) = struct
  let exec () =
    let terms, steps = Strategy.idastar Term.v in
    let () = print_endline ("Minimum step required to derivate term : " ^ (string_of_int steps)) in
    let () = print_endline "Path :" in
    List.iter (print_endline #~ Strategy.Lt.to_string) terms
end

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

module Reduction_with_chosen_strategy (Term : Value) (Strategy : Strategy.Strategy) = struct
  let exec () =
    let reverse_hst hst = Hashtbl.fold (fun k v acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (Hashtbl.length hst)) in
    let hst, g, steps = Strategy.reduce_graph Term.v in
    Graph.Graph.to_pdf g (reverse_hst hst) [] ("reduction_graph_" ^ Strategy.name);
    print_endline ("Number of steps to normal form : " ^ (string_of_int steps))
    (* let _ = Sys.command "xdg-open results/reduction_graph_WLis.pdf &" in () *)
end

(* ---------- Launch ---------- *)

let () = print_endline "Starting Lambda Calculus"
let () = print_endline "Enter 'exit', 'q' or 'quit' to leave the program"
let mode_choice choice term strategy construct_graph =
  match choice with
  | "1" -> 
    (match strategy with
    | "Lo" -> 
      let module M = Reduction_graph_viewer (struct let v = term end) (Strategy.LONoStrategy) in
      M.exec ()
    | "Li" -> 
      let module M = Reduction_graph_viewer (struct let v = term end) (Strategy.LINoStrategy) in
      M.exec ()
    | "Wlo" ->
      let module M = Reduction_graph_viewer (struct let v = term end) (Strategy.WLONoStrategy) in
      M.exec ()
    | "Wli" ->
      let module M = Reduction_graph_viewer (struct let v = term end) (Strategy.WLINoStrategy) in
      M.exec ()
    | _ -> raise (Invalid_argument "Not a valid strategy"))
  | "2" -> 
    (match strategy with
    | "Lo" ->
      let module M = Astar (struct let v = term end) (Strategy.LONoStrategy) (struct let v = string_of_bool construct_graph end) in
      M.exec ()
    | "Li" ->
      let module M = Astar (struct let v = term end) (Strategy.LINoStrategy) (struct let v = string_of_bool construct_graph end) in
      M.exec ()
    | "Wlo" ->
      let module M = Astar (struct let v = term end) (Strategy.WLONoStrategy) (struct let v = string_of_bool construct_graph end) in
      M.exec ()
    | "Wli" ->
      let module M = Astar (struct let v = term end) (Strategy.WLINoStrategy) (struct let v = string_of_bool construct_graph end) in
      M.exec ()
    | _ -> raise (Invalid_argument "Not a valid strategy"))
  | "3" ->
    (match strategy with
    | "Lo" ->
      let module M = IDAstar (struct let v = term end) (Strategy.LONoStrategy) in
      M.exec ()
    | "Li" ->
      let module M = IDAstar (struct let v = term end) (Strategy.LINoStrategy) in
      M.exec ()
    | "Wlo" ->
      let module M = IDAstar (struct let v = term end) (Strategy.WLONoStrategy) in
      M.exec ()
    | "Wli" ->
      let module M = IDAstar (struct let v = term end) (Strategy.WLINoStrategy) in
      M.exec ()
    | _ -> raise (Invalid_argument "Not a valid strategy"))
  | "4" -> 
    (match strategy with
    | "Lo" -> 
      let module M = Reduction_with_chosen_strategy (struct let v = term end) (Strategy.LeftOutermostStrategy) in
      M.exec ()
    | "Li" ->
      let module M = Reduction_with_chosen_strategy (struct let v = term end) (Strategy.LeftInnermostStrategy) in
      M.exec ()
    | "Wlo" ->
      let module M = Reduction_with_chosen_strategy (struct let v = term end) (Strategy.WeakLeftOutermostStrategy) in
      M.exec ()
    | "Wli" ->
      let module M = Reduction_with_chosen_strategy (struct let v = term end) (Strategy.WeakLeftInnermostStrategy) in
      M.exec  ()
    | _ -> raise (Invalid_argument "Not a valid strategy"))
  | "5" -> 
    let module M = Lambda_tree_viewer (struct let v = term end) in
    M.exec ()
  | "6" -> extend_lambda_term term
  | "7" -> test term
  | _ -> raise (Invalid_argument "Not a valid choice")

let prechoice = try Sys.argv.(1) with | Invalid_argument _ -> ""
let preterm = try Sys.argv.(2) with | Invalid_argument _ -> ""
let strategy = try Sys.argv.(3) with | Invalid_argument _ -> ""
let construct_graph = try Sys.argv.(4) = "true" with | Invalid_argument _ -> false
let () = mode_choice prechoice preterm strategy construct_graph
