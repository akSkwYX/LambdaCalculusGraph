(* ---------- Tests ---------- *)

(* let lambda = "(Lx.xx)(Lx.xx)" *)
(* let term = Strategy.LeftExternalStrategy.Lt.of_string lambda *)
(* let () = print_endline (Strategy.LeftExternalStrategy.Lt.to_string term) *)
(* let reduce_les_term = Strategy.LeftExternalStrategy.reduce_safer term *)
(* let () = print_endline (Strategy.LeftExternalStrategy.Lt.to_string reduce_les_term) *)
(* let reduced_les_term_db = Strategy.LeftExternalStrategy.Lt.deBruijn_index (reduce_les_term) *)
(* let () = List.iter (fun x -> print_int x; print_string " ") reduced_les_term_db *)

(* ---------- All reduction possibilities ---------- *)

(* module Ns = Strategy.NoStrategy *)
(**)
(* let () = print_endline "Lambda calculus reduction possibilities viewer" *)
(* let exit = ref false *)
(* let () = while not !exit do *)
(*   let () = print_string "> " in *)
(*   let lambda = read_line () in *)
(*   if List.mem lambda ["exit"; "q"; "quit"] then *)
(*     exit := true *)
(*   else *)
(*     let term = Ns.Lt.of_string lambda in *)
(*     let () = print_endline ("Term : \n" ^ (Ns.Lt.to_string_tree term)) in *)
(*     let reduce_possibilities = Ns.reduce_step term in *)
(*     let () = print_endline "Reduction possibilities :" in *)
(*     List.iter (fun t -> print_endline (Ns.Lt.to_string t)) reduce_possibilities *)
(*   done *)

(* ---------- Reduction graph ---------- *)

module Ns = Strategy.NoStrategy

let () = print_endline "Lambda calculus reduction graph viewer"
let exit = ref false
let () = while not !exit do
  let () = print_string "> " in
  let lambda = read_line () in
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
    Graph.Graph.to_pdf g reverse_hst "reduction_graph"
  done

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

(* open LambdaTerm.NoParenthesis *)
(**)
(* let succ = "Ln,f,x.f(nfx)" *)
(* let plus = "Lm,n,f,x.mf(nfx)" *)
(* let time = "Lm,n,f.m(nf)" *)
(**)
(* let succ_term = of_string succ *)
(* let plus_term = of_string plus *)
(* let time_term = of_string time *)
(**)
(* let () = print_endline (to_ugly_string succ_term) *)
(* let () = print_endline (to_ugly_string plus_term) *)
(* let () = print_endline (to_ugly_string time_term) *)

(* ---------- Reduction ---------- *)

(* let strats = ["Li"; "Le"] *)
(* let full_name_strats = ["Left Innermost"; "Left External"] *)
(* let rec string_of_strats l = *)
(*   match l with *)
(*   | [] -> "" *)
(*   | [x] -> x *)
(*   | h :: t -> h ^ " - " ^ (string_of_strats t) *)
(**)
(* let strat_to_use = try Sys.argv.(1) with  *)
(*   | _ -> (print_endline ("Usage :\ndune exec main <strategy name>\n\nStrategy : " ^ (string_of_strats strats)); exit 1) *)
(**)
(* module Lis = Strategy.LeftInnermostStrategy *)
(* module Les = Strategy.LeftExternalStrategy *)
(**)
(* let () = if not ( List.mem strat_to_use strats ) then *)
(*   (print_endline ("Usage :\ndune exec main <strategy name>\n\nStrategy : " ^ (string_of_strats strats)); exit 1) *)
(**)
(* let () = print_endline "Lambda Calculus Interpreter" *)
(* let () = print_string "Strategy : " *)
(* let _ = Option.map (Fun.compose print_endline (List.nth full_name_strats)) (List.find_index ((=) strat_to_use ) strats) *)
(* let () = print_string "> " *)
(* let lambda = read_line () *)
(**)
(* let (hst, g) = if String.equal strat_to_use "Li" then *)
(*                  Lis.reduce_string_graph lambda *)
(*                else *)
(*                  Les.reduce_string_graph lambda *)
(* let () = print_endline ("Node map :\n" ^ *)
(*          Hashtbl.fold (fun k v acc -> acc ^ k ^ " : " ^ (string_of_int v) ^ "\n") *)
(*          hst "") *)
(* let () = print_endline "Graph :" *)
(* let () = print_endline (Graph.Graph.to_string g) *)
