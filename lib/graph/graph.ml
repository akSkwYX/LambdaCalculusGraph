module type Graph = sig
  type t

  val empty : t
  val is_empty : t -> bool

  val order : t -> int
  val size : t -> int
  val add_node : t -> t * int 
  val add_edge : int -> int -> t -> unit 
  val force_add_edge : int -> int -> t -> t
  val get_neighbors : int -> t -> int list
  val init_from_edges : (int * int) list -> t
  val to_string : t -> string
  val to_pdf : t -> (int, int list) Hashtbl.t -> string -> unit
end

module Graph : Graph = struct
  type t = int list array

  let empty = [||]
  let is_empty g = Array.length g = 0

  let order g = Array.length g
  let size g = Array.fold_left (fun acc neighbors -> acc + List.length neighbors) 0 g

  let add_node g = Array.append g [| [] |], Array.length g

  let add_edge v1 v2 g =
    let len = Array.length g in
    if v1 < 0 || v1 >= len || v2 < 0 || v2 >= len then
      raise (Invalid_argument "Node index out of bounds")
    else
      (* Remove this case if want to see each possible derivations *)
      if List.mem v2 g.(v1) then
        ()
      else
        g.(v1) <- v2 :: g.(v1)

  let force_add_edge v1 v2 g =
    let len = Array.length g in
    let g' = if v1 >= len || v2 >= len then
               let new_size = max (v1 + 1) (v2 + 1) in
               Array.init new_size (fun i -> if i < len then g.(i) else [])
             else g
    in
    add_edge v1 v2 g';
    g'

  let get_neighbors v g =
    if v < 0 || v >= Array.length g then
      raise (Invalid_argument "Node index out of bounds")
    else
      g.(v)

  let init_from_edges edges =
    let max_node = List.fold_left (fun acc (v1, v2) -> max acc (max v1 v2)) (-1) edges in
    let g = Array.make (max_node + 1) [] in
    List.iter (fun (v1, v2) -> add_edge v1 v2 g |> ignore) edges;
    g

  let to_string g =
    Array.fold_left (fun (acc, i) neighbors ->
       acc ^ (string_of_int i) ^ " : " ^
       (String.concat "; " (List.map string_of_int neighbors) ^ "\n"), i+1
    ) ("", 0) g |> fst

  (* To pdf : *)
  let escape_label s =
    let b = Buffer.create (String.length s + 8) in
    String.iter (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\"" 
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | _ -> Buffer.add_char b c
    ) s;
    Buffer.contents b

  let compute_layers (g : int list array) : int list array =
    let n = Array.length g in
    let q = Queue.create () in
    let level = Array.make n (-1) in
    let maxl = ref 0 in
    Queue.add 0 q; level.(0) <- 0;
    while not (Queue.is_empty q) do
      let u = Queue.take q in
      List.iter (fun v ->
        if v >= 0 && v < n && level.(v) = -1 then (
          level.(v) <- level.(u) + 1;
          if level.(v) > !maxl then maxl := level.(v);
          Queue.add v q
        )
      ) g.(u)
    done;
    let groups = Array.init (!maxl + 1) (Fun.const []) in
    for i = 0 to n - 1 do
      if level.(i) >= 0 then groups.(level.(i)) <- i :: groups.(level.(i))
      else groups.(0) <- i :: groups.(0)
    done;
    groups

  let to_pdf (g : int list array) (hst : (int, int list) Hashtbl.t) (filename : string) : unit =
    let format_node n =
      try LambdaTerm.LambdaTerm.to_string @@ LambdaTerm.LambdaTerm.of_deBruijn (Hashtbl.find hst n)
      with Not_found -> string_of_int n
    in
    let dot_path = "results/" ^ filename ^ ".dot" in
    let pdf_path = "results/" ^ filename ^ ".pdf" in
    let oc = open_out dot_path in
    Printf.fprintf oc "digraph G {\n";
    Printf.fprintf oc "  graph [splines=true, overlap=false, rankdir=LR, nodesep=0.7, ranksep=1.0];\n";
    Printf.fprintf oc "  node [shape=box, style=rounded, fontname=\"JetBrains Mono\", fontsize=10, margin=0.08];\n";
    Printf.fprintf oc "  edge [arrowsize=0.8];\n\n";

    Array.iteri (fun i _ ->
      let label = escape_label (format_node i) in
      Printf.fprintf oc "  n%d [label=\"%s\"];\n" i label
    ) g;
    Printf.fprintf oc "\n";

    Array.iteri (fun i nbrs ->
      List.iter (fun j -> Printf.fprintf oc "  n%d -> n%d;\n" i j) nbrs
    ) g;
    Printf.fprintf oc "\n";

    let layers = compute_layers g in
    Array.iter (fun nodes ->
      match nodes with
      | [] -> ()
      | _ ->
        Printf.fprintf oc "  { rank = same; ";
        List.iter (fun i -> Printf.fprintf oc "n%d; " i) nodes;
        Printf.fprintf oc "}\n"
    ) layers;
    Printf.fprintf oc "}\n";
    close_out oc;

    let cmd = Printf.sprintf "dot -Tpdf -Gcharset=UTF-8 -o %s %s" pdf_path dot_path in
    ignore (Sys.command cmd)
end
