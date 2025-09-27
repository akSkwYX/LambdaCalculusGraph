(* DeBruijn tests *)
open LambdaTerm.Left

let lambda = "Lx.(Lx.x(Lx.xx)(Ly.xy))"
let de_bruijn = deBruijn_string (of_string lambda)
let of_de_bruijn = of_deBruijn_string de_bruijn
let back_to_string = to_string of_de_bruijn

let () = print_endline de_bruijn
let () = print_endline back_to_string


(* ---------- Extended Lambda Term ---------- *)

(* open LambdaTerm.Left *)
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
