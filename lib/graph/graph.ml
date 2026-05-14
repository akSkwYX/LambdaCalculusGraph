module type Element = sig
  type t

  val eq : t -> t -> bool

  val to_string : t -> string
end

module type Graph = sig
  module Elem : Element

  type t

  val empty : t
  val is_empty : t -> bool

  val order : t -> int
  val size : t -> int

  val add_node : t -> unit
  val add_edge : int -> int -> t -> unit
  val force_add_edge : int -> int -> t -> unit

  val to_pdf : t -> (int, Elem.t) Hashtbl.t -> Elem.t list -> string -> unit
end

module Graph (Elem : Element) : Graph with module Elem = Elem = struct
  module Elem = Elem

  (* order, size, graph *)
  type t = int ref * int ref * int list array ref

  let empty = (ref 0, ref 0, ref [||])
  let is_empty (o, _, _) = !o = 0

  let order (o, _, _) = !o
  let size (_, s, _) = !s

  let add_node (o, _, g) = 
    o := !o + 1; g := Array.append !g [| [] |]

  let add_edge u v (o, s, g) =
    if u < 0 || u >= !o || v < 0 || v >= !o then
      raise (Invalid_argument "Node index out of bounds")
    else
      if List.mem v !g.(u) then ()
      else (!g.(u) <- v :: !g.(u); s := !s + 1)

  let force_add_edge u v (o, s, g) =
    if u >= !o || v >= !o then
      (g := Array.init (max u v + 1) (fun i -> if i < !o then !g.(i) else []); o := max u v + 1);
    add_edge u v (o, s, g)

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

  let to_pdf ((_, _, g) : t) (hst : (int, Elem.t) Hashtbl.t) (path : Elem.t list) (filename : string) : unit =
    let format_node n =
      try Elem.to_string (Hashtbl.find hst n)
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
    ) !g;
    Printf.fprintf oc "\n";

    Array.iteri (fun i nbrs ->
      let i_node = Hashtbl.find hst i in
      List.iter (fun j -> 
        if List.mem i_node path && List.mem (Hashtbl.find hst j) path then Printf.fprintf oc "  n%d -> n%d [color=red style=\"bold\"];\n" i j
        else Printf.fprintf oc "  n%d -> n%d;\n" i j) nbrs
    ) !g;
    Printf.fprintf oc "\n";

    let layers = compute_layers !g in
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
