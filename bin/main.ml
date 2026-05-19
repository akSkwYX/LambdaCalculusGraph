let ( #~ ) = Fun.compose

(* ---------- Lambda tree viewer ---------- *)

let lambda_tree_viewer term =
  let module LambdaTerm = LambdaTerm.LambdaTerm in
  let term = LambdaTerm.of_string term in
  let () = print_endline ("Term : \n" ^ (LambdaTerm.to_string_tree term)) in ()

(* ---------- Reduction graph ---------- *)

let reduction_graph_viewer term (strategy : (module Strategy.NoStrategy)) =
  let module Strategy = (val strategy : Strategy.NoStrategy) in
  let module SHashtbl = Strategy.LHashtbl in
  let module Graph = Strategy.Graph in
  let hst, g = Strategy.reduce_graph term in
  let reverse_hst = SHashtbl.fold (fun k v acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (SHashtbl.length hst)) in
  Graph.to_pdf g reverse_hst [] "reduction_graph";
  Printf.printf "Ordre : %d | Taille : %d" (Graph.order g) (Graph.size g)

(* ---------- A* ---------- *)

let astar term (strategy : (module Strategy.NoStrategy)) construct_graph =
  let module Strategy = (val strategy : Strategy.NoStrategy) in
  let module SHashtbl = Strategy.LHashtbl in
  let module Graph = Strategy.Graph in
  let hst, g, terms, steps = Strategy.astar construct_graph (Strategy.Lt.of_string term) in
  let () = print_endline ("Minimum step required to derivate term : " ^ (string_of_int steps)) in
  let () = print_endline ("Order of graph : " ^ (string_of_int (Graph.order g))) in
  let () = print_endline ("Size of graph : " ^ (string_of_int (Graph.size g))) in
  let () = print_endline "Path :" in
  let () = List.iter (print_endline #~ Strategy.Lt.to_string) terms in
  if construct_graph then
    begin
    let reverse_hst = SHashtbl.fold (fun k (v, _, _) acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (SHashtbl.length hst)) in
    Graph.to_pdf g reverse_hst terms "astar_graph"
    end

(* ---------- Extended Lambda Term ---------- *)

let extend_lambda_term lambda =
  let open LambdaTerm.LambdaTerm in
  let term = of_string lambda in
  print_endline (to_ugly_string term)

(* ---------- Reduction ---------- *)

let reduction_with_chosen_strategy term (strategy : (module Strategy.Strategy)) strategy_name = 
  let module Strategy = (val strategy : Strategy.Strategy) in
  let module SHashtbl = Strategy.LHashtbl in
  let module Graph = Strategy.Graph in
  let hst, g, steps = Strategy.reduce_graph term in
  let reverse_hst = SHashtbl.fold (fun k v acc -> Hashtbl.add acc v k; acc) hst (Hashtbl.create (SHashtbl.length hst)) in
  Graph.to_pdf g reverse_hst [] ("reduction_graph_"^strategy_name);
  print_endline ("Number of steps to normal form : " ^ (string_of_int steps))

(* ---------- Main ---------- *)

let mode_choice choice term strategy construct_graph =
  let strategyMod, nostrategyMod = match strategy with
  | "Lo" -> Strategy.leftOutermostStrategy, Strategy.loNoStrategy
  | "Li" -> Strategy.leftInnermostStrategy, Strategy.liNoStrategy
  | "Wlo" -> Strategy.weakLeftOutermostStrategy, Strategy.wloNoStrategy
  | "Wli" -> Strategy.weakLeftInnermostStrategy, Strategy.wliNoStrategy
  | _ -> raise (Invalid_argument "Invalid strategy name : Lo | Li | Wlo | Wli")
  in
  match choice with
  | "1" -> reduction_graph_viewer term nostrategyMod
  | "2" -> astar term nostrategyMod construct_graph
  | "3" -> reduction_with_chosen_strategy term strategyMod strategy
  | "4" -> lambda_tree_viewer term
  | "5" -> extend_lambda_term term
  | _ -> raise (Invalid_argument "Not a valid choice")

let prechoice = try Sys.argv.(1) with | Invalid_argument _ -> ""
let strategy = try Sys.argv.(2) with | Invalid_argument _ -> ""
let construct_graph = try Sys.argv.(3) = "true" with | Invalid_argument _ -> false
let preterm = try Sys.argv.(4) with | Invalid_argument _ -> ""
let () = print_endline preterm
let () = mode_choice prechoice preterm strategy construct_graph
