module type Analysis = sig
  type data
  type edge
  type vertex
  type g

  val empty : data
  val is_empty : data -> bool
  val is_input : vertex -> bool
  val is_output : vertex -> bool
  val hint_normalize : vertex -> bool
  val join : data -> data -> data

  (* accumulated -> potentially_new -> accumulated * actually_new *)
  val accumulate : data -> data -> data * data
  val analyze : edge -> data -> data
  val show_data : data -> string
  val show_vertex : vertex -> string
end

(** Minimal graph signature for work list algorithm *)
module type G = sig
  type t

  module V : Graph.Sig.COMPARABLE

  module E : sig
    type t

    val dst : t -> V.t
    val src : t -> V.t
  end

  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges : (V.t -> V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val succ_e : t -> V.t -> E.t list
  val pred_e : t -> V.t -> E.t list
  val succ : t -> V.t -> V.t list
  val pred : t -> V.t -> V.t list
end

module Make
    (G : G)
    (A : Analysis
           with type g = G.t
           with type edge = G.E.t
           with type vertex = G.V.t) =
struct
  module NodeSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

  module OriginalVertexMap = Map.Make (struct
    type t = G.V.t

    let compare = compare
  end)

  module OriginalVertexPairMap = Map.Make (struct
    type t = G.V.t * G.V.t

    let compare = compare
  end)

  module Topological = Graph.Topological.Make (G)

  type prepared_node = {
    initial : unit -> A.data;
    pred_edges : (int * (A.data -> A.data)) list;
    succ_nodes : int list; (* TODO remove *)
    succ_edges : int list;
  }

  let analyze_prepared (m_data_acc : A.data option Array.t)
      (m_data_new : A.data Array.t) (nodes : prepared_node Array.t)
      (nodes_by_original : int OriginalVertexMap.t) (wl : NodeSet.t) =
    let m_data_acc = Array.copy m_data_acc in
    let m_data_new = Array.copy m_data_new in
    let wl = ref wl in

    let rec loop () =
      match NodeSet.min_elt_opt !wl with
      | None -> ()
      | Some wl_node ->
          wl := NodeSet.remove wl_node !wl;

          Printf.printf "DEBUG loop wl_node %d\n" wl_node;

          let process_node node =
            Printf.printf "DEBUG process_node %d\n" node;

            let node_entry = nodes.(node) in
            let potentially_new =
              List.fold_left
                (fun potentially_new (pred_edge, f) ->
                  let edge_data = f m_data_new.(pred_edge) in
                  A.join potentially_new edge_data)
                (node_entry.initial ()) node_entry.pred_edges
            in
            Printf.printf "DEBUG potentially_new (%s)\n"
              (if A.is_empty potentially_new then "empty" else "non-empty");
            List.iter
              (fun (pred_edge, _) -> m_data_new.(pred_edge) <- A.empty)
              node_entry.pred_edges;
            let process_successors data =
              List.iter
                (fun succ_edge ->
                  m_data_new.(succ_edge) <- A.join m_data_new.(succ_edge) data)
                node_entry.succ_edges
            in
            match m_data_acc.(node) with
            | Some data_acc ->
                let data_acc', actually_new =
                  A.accumulate data_acc potentially_new
                in
                if not (A.is_empty actually_new) then (
                  m_data_acc.(node) <- Some data_acc';
                  process_successors actually_new)
            | None ->
                if not (A.is_empty potentially_new) then
                  process_successors potentially_new
          in

          let wl_node_entry = nodes.(wl_node) in
          List.iter process_node wl_node_entry.succ_nodes;
          List.iter
            (fun succ_node -> wl := NodeSet.add succ_node !wl)
            wl_node_entry.succ_nodes;

          loop ()
    in

    NodeSet.iter
      (fun node ->
        let node_entry = nodes.(node) in
        List.iter
          (fun succ_edge -> m_data_new.(succ_edge) <- node_entry.initial ())
          node_entry.succ_edges)
      !wl;
    loop ();
    fun original_node ->
      let node = OriginalVertexMap.find original_node nodes_by_original in
      Printf.printf "DEBUG A %s\n"
        (match m_data_acc.(node) with
        | Some d -> Printf.sprintf "Some %s" (A.show_data d)
        | None -> "None");
      Printf.printf "DEBUG B %s\n"
        (m_data_acc
        |> Array.map (function
             | Some d -> Printf.sprintf "Some %s" (A.show_data d)
             | None -> "None")
        |> Array.to_list |> String.concat "; ");
      Option.get m_data_acc.(node)

  let prepare g =
    let m_data_acc, m_data_new, nodes, nodes_by_original, wl, initial_ref =
      Perf.count_and_time Perf.global_counters.fixpoint_prepare (fun () ->
          let nodes_by_original, _ =
            Topological.fold
              (fun v (m, i) ->
                Printf.printf "DEBUG node %d %s\n" i (A.show_vertex v);
                (OriginalVertexMap.add v i m, i + 1))
              g
              (OriginalVertexMap.empty, 0)
          in
          let edges_by_original =
            let next_edge_index_ref = ref 0 in
            G.fold_edges
              (fun e_src e_dst m ->
                let i = !next_edge_index_ref in
                next_edge_index_ref := i + 1;
                Printf.printf "DEBUG edge %d %s %s\n" i (A.show_vertex e_src)
                  (A.show_vertex e_dst);
                OriginalVertexPairMap.add (e_src, e_dst) i m)
              g OriginalVertexPairMap.empty
          in

          let node_count = OriginalVertexMap.cardinal nodes_by_original in
          let edge_count = OriginalVertexPairMap.cardinal edges_by_original in

          let m_data_acc = Array.make node_count None in
          let m_data_new = Array.make edge_count A.empty in

          let nodes = Array.make node_count None in
          let wl = ref NodeSet.empty in
          let initial_ref = ref None in

          OriginalVertexMap.iter
            (fun original_node node ->
              if A.is_input original_node then wl := NodeSet.add node !wl;
              if A.is_output original_node || A.hint_normalize original_node
              then m_data_acc.(node) <- Some A.empty;
              nodes.(node) <-
                Some
                  {
                    initial =
                      (fun () ->
                        Printf.printf "DEBUG initial %d %s\n" node
                          (A.show_vertex original_node);
                        (Option.get !initial_ref) original_node);
                    pred_edges =
                      original_node |> G.pred_e g
                      |> List.map (fun original_edge ->
                             ( OriginalVertexPairMap.find
                                 (G.E.src original_edge, G.E.dst original_edge)
                                 edges_by_original,
                               A.analyze original_edge ));
                    succ_nodes =
                      original_node |> G.succ g
                      |> List.map (fun original_succ ->
                             OriginalVertexMap.find original_succ
                               nodes_by_original);
                    succ_edges =
                      original_node |> G.succ_e g
                      |> List.map (fun original_edge ->
                             OriginalVertexPairMap.find
                               (G.E.src original_edge, G.E.dst original_edge)
                               edges_by_original);
                  })
            nodes_by_original;

          let nodes = Array.map Option.get nodes in
          let wl = !wl in

          (m_data_acc, m_data_new, nodes, nodes_by_original, wl, initial_ref))
    in
    fun initial ->
      initial_ref := Some initial;
      analyze_prepared m_data_acc m_data_new nodes nodes_by_original wl
end
