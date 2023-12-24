(* This file has been copied from Ocamlgraph and modified. The original
   copyright notices are below. *)

(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Copyright (c) 2010 - 2012 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * All rights reserved. *)

(* maximum fixpoint point calculation with the work list algorithm;
   to implement a concrete analysis, implement a module that satisfies
   the Rules signature. Such a module in the Analysis functor gives a
   complete analysis/optimization module that works on a CFG.
*)

module type Analysis = sig
  type data
  type edge
  type vertex
  type g

  val empty : data
  val join : data -> data -> data

  (* accumulated -> potentially_new -> accumulated * actually_new *)
  val accumulate : data -> data -> data * data option
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

  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
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
  module M = Map.Make (G.V)
  module N = Set.Make (G.V)

  let analyze initial g =
    let nodes, m_data_acc, m_data_new =
      G.fold_vertex
        (fun vertex (n, m_data_acc, m_data_new) ->
          ( (if initial vertex = A.empty then n else N.add vertex n),
            (* TODO is the above "if" check necessary? *)
            M.add vertex A.empty m_data_acc,
            M.add vertex (initial vertex) m_data_new ))
        g
        (N.empty, M.empty, M.empty)
    in
    (* generate an associative map to quickly find the incoming
     * edges of a node during the anaysis store a pair of
     * a partially applied analysis function and the corresponding
     * 'partner' node *)
    let nodemap : ((A.data -> A.data) * G.V.t) list M.t =
      let add n =
        let preds = G.pred_e g n in
        List.map (fun edge -> (A.analyze edge, G.E.src edge)) preds
      in
      G.fold_vertex (fun vertex m -> M.add vertex (add vertex) m) g M.empty
    in

    let rec worklist (m_data_acc : A.data M.t) (m_data_new : A.data M.t)
        (wl : N.t) =
      (* 'meet' an arbitrary number of data-sets *)
      let meet initial xs = List.fold_left A.join initial xs in

      (* analyze one node, creating a new data-set and node-worklist
         as necessary *)
      let analyze_node analysis n m_data_acc m_data_new wl =
        match analysis m_data_acc m_data_new n with
        | m_data_acc, None -> (m_data_acc, M.add n A.empty m_data_new, wl)
        | m_data_acc, Some m_data_new -> (m_data_acc, m_data_new, N.add n wl)
      in

      (* get some node from the node-set -- this will eventually trigger
           an exception *)
      match try Some (N.choose wl) with Not_found -> None with
      | None -> m_data_acc
      | Some n ->
          (* remove the chosen node from the set *)
          let wl = N.remove n wl in

          let f, ns =
            (* analyze all INCOMING edges of all SUCCESSOR nodes of the
               node to be processed *)
            (* process one node: analyze all it's incoming edges
               and merge the resulting data;
               if the result is different to the previously stored data
               for this node, return a new tuple, else None *)
            let new_node_data (m_data_acc : A.data M.t)
                (m_data_new : A.data M.t) node =
              let edges = M.find node nodemap in
              let analysis =
                List.map (fun (f, src) -> f (M.find src m_data_new)) edges
              in
              let data_acc = M.find node m_data_acc in
              let potentially_new = meet (initial node) analysis in
              let data_acc', actually_new =
                A.accumulate data_acc potentially_new
              in
              (*
              Printf.printf
                "Analyzing node %s; data_acc = %s; potentially_new = %s; \
                 data_acc' = %s; actually_new = %s\n"
                (A.show_vertex node) (A.show_data data_acc)
                (A.show_data potentially_new)
                (A.show_data data_acc')
                (match actually_new with
                | Some d -> Printf.sprintf "Some(%s)" @@ A.show_data d
                | None -> "None");
                *)
              ( M.add node data_acc' m_data_acc,
                match actually_new with
                | Some d -> Some (M.add node d m_data_new)
                | None -> None )
            in

            (new_node_data, G.succ g n)
          in
          (* analyze all successor nodes by analyzing all of their
             predecessor edges *)
          let m_data_acc, m_data_new, wl =
            List.fold_left
              (fun (m_data_acc, m_data_new, wl) n ->
                analyze_node f n m_data_acc m_data_new wl)
              (m_data_acc, m_data_new, wl)
              ns
          in

          (* do a recursive call: the recursion will eventually end with a
           * Not_found exception when no nodes are left in the work list *)
          worklist m_data_acc m_data_new wl
    in
    let m_data_acc = worklist m_data_acc m_data_new nodes in
    fun n -> M.find n m_data_acc
end
