(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* Maps. *)

(* We require imperative maps, that is, maps that can be updated in place.
   An implementation of persistent maps, such as the one offered by ocaml's
   standard library, can easily be turned into an implementation of imperative
   maps, so this is a weak requirement. *)

module type IMPERATIVE_MAPS = sig
  type key
  type 'data t
  val create: unit -> 'data t
  val clear: 'data t -> unit
  val add: key -> 'data -> 'data t -> unit
  val find: key -> 'data t -> 'data
  val iter: (key -> 'data -> unit) -> 'data t -> unit
end

(* -------------------------------------------------------------------------- *)

(* Properties. *)

(* Properties must form a partial order, equipped with a least element, and
   must satisfy the ascending chain condition: every monotone sequence
   eventually stabilizes. *)

(* [is_maximal] determines whether a property [p] is maximal with respect to
   the partial order. Only a conservative check is required: in any event, it
   is permitted for [is_maximal p] to return [false]. If [is_maximal p]
   returns [true], then [p] must have no upper bound other than itself. In
   particular, if properties form a lattice, then [p] must be the top
   element. This feature, not described in the paper, enables a couple of
   minor optimizations. *)

module type PROPERTY = sig
  type property
  val bottom: property
  val equal: property -> property -> bool
  val is_maximal: property -> bool
end

(* -------------------------------------------------------------------------- *)

(* The dynamic dependency graph. *)

(* An edge from [node1] to [node2] means that [node1] depends on [node2], or
   (equivalently) that [node1] observes [node2]. Then, an update of the
   current property at [node2] causes a signal to be sent to [node1]. A node
   can observe itself. *)

(* This module could be placed in a separate file, but is included here in
   order to make [Fix] self-contained. *)

module Graph : sig

  (* This module provides a data structure for maintaining and modifying
     a directed graph. Each node is allowed to carry a piece of client
     data. There are functions for creating a new node, looking up a
     node's data, looking up a node's predecessors, and setting or
     clearing a node's successors (all at once). *)
  type 'data node

  (* [create data] creates a new node, with no incident edges, with
     client information [data]. Time complexity: constant. *)
  val create: 'data -> 'data node

  (* [data node] returns the client information associated with
     the node [node]. Time complexity: constant. *)
  val data: 'data node -> 'data

  (* [predecessors node] returns a list of [node]'s predecessors.
     Amortized time complexity: linear in the length of the output
     list. *)
  val predecessors: 'data node -> 'data node list

  (* [set_successors src dsts] creates an edge from the node [src] to
     each of the nodes in the list [dsts]. Duplicate elements in the
     list [dsts] are removed, so that no duplicate edges are created. It
     is assumed that [src] initially has no successors. Time complexity:
     linear in the length of the input list. *)
  val set_successors: 'data node -> 'data node list -> unit

  (* [clear_successors node] removes all of [node]'s outgoing edges.
     Time complexity: linear in the number of edges that are removed. *)
  val clear_successors: 'data node -> unit

  (* That's it. *)
end
= struct

  (* Using doubly-linked adjacency lists, one could implement [predecessors]
     in worst-case linear time with respect to the length of the output list,
     [set_successors] in worst-case linear time with respect to the length of
     the input list, and [clear_successors] in worst-case linear time with
     respect to the number of edges that are removed. We use a simpler
     implementation, based on singly-linked adjacency lists, with deferred
     removal of edges. It achieves the same complexity bounds, except
     [predecessors] only offers an amortized complexity bound. This is good
     enough for our purposes, and, in practice, is more efficient by a
     constant factor. This simplification was suggested by Arthur
     Charguéraud. *)

  type 'data node = {

    (* The client information associated with this node. *)

    data: 'data;

    (* This node's incoming and outgoing edges. *)

    mutable outgoing: 'data edge list;
    mutable incoming: 'data edge list;

    (* A transient mark, always set to [false], except when checking
       against duplicate elements in a successor list. *)

    mutable marked: bool;

  }

  and 'data edge = {

    (* This edge's nodes. Edges are symmetric: source and destination
       are not distinguished. Thus, an edge appears both in the outgoing
       edge list of its source node and in the incoming edge list of its
       destination node. This allows edges to be easily marked as
       destroyed. *)

    node1: 'data node;
    node2: 'data node;

    (* Edges that are destroyed are marked as such, but are not
       immediately removed from the adjacency lists. *)

    mutable destroyed: bool;

  }

  let create (data : 'data) : 'data node = {
    data = data;
    outgoing = [];
    incoming = [];
    marked = false;
  }

  let data (node : 'data node) : 'data =
    node.data

  (* [follow src edge] returns the node that is connected to [src]
     by [edge]. Time complexity: constant. *)

  let follow src edge =
    if edge.node1 == src then
      edge.node2
    else begin
      assert (edge.node2 == src);
      edge.node1
    end

  (* The [predecessors] function removes edges that have been marked
     destroyed. The cost of removing these has already been paid for,
     so the amortized time complexity of [predecessors] is linear in
     the length of the output list. *)

  let predecessors (node : 'data node) : 'data node list =
    let predecessors = List.filter (fun edge -> not edge.destroyed) node.incoming in
    node.incoming <- predecessors;
    List.map (follow node) predecessors

  (* [link src dst] creates a new edge from [src] to [dst], together
     with its reverse edge. Time complexity: constant. *)

  let link (src : 'data node) (dst : 'data node) : unit =
    let edge = {
      node1 = src;
      node2 = dst;
      destroyed = false;
    } in
    src.outgoing <- edge :: src.outgoing;
    dst.incoming <- edge :: dst.incoming

  let set_successors (src : 'data node) (dsts : 'data node list) : unit =
    assert (src.outgoing = []);
    let rec loop = function
      | [] ->
          ()
      | dst :: dsts ->
          if dst.marked then
            loop dsts (* skip duplicate elements *)
          else begin
            dst.marked <- true;
            link src dst;
            loop dsts;
            dst.marked <- false
          end
    in
    loop dsts

  let clear_successors (node : 'data node) : unit =
    List.iter (fun edge ->
      assert (not edge.destroyed);
      edge.destroyed <- true;
    ) node.outgoing;
    node.outgoing <- []

end

(* -------------------------------------------------------------------------- *)

(* The code is parametric in an implementation of maps over variables and in
   an implementation of properties. *)

module Make
  (M : IMPERATIVE_MAPS)
  (P : PROPERTY)
= struct

type variable =
    M.key

type property =
    P.property

type valuation =
    variable -> property

type rhs =
    valuation -> property

type equations =
    variable -> rhs

(* -------------------------------------------------------------------------- *)

(* Data. *)

(* Each node in the dependency graph carries information about a fixed
   variable [v]. *)

type node =
    data Graph.node

and data = {

  (* This is the result of the application of [rhs] to the variable [v]. It
     must be stored in order to guarantee that this application is performed
     at most once. *)
  rhs: rhs;

  (* This is the current property at [v]. It evolves monotonically with
     time. *)
  mutable property: property;

  (* That's it! *)
}

(* [property node] returns the current property at [node]. *)

let property node =
  (Graph.data node).property

(* -------------------------------------------------------------------------- *)

(* Many definitions must be made within the body of the function [lfp].
   For greater syntactic convenience, we place them in a local module. *)

let lfp (eqs : equations) : valuation =
  let module LFP = struct

(* -------------------------------------------------------------------------- *)

(* The workset. *)

(* When the algorithm is inactive, the workset is empty. *)

(* Our workset is based on a Queue, but it could just as well be based on a
   Stack. A textual replacement is possible. It could also be based on a
   priority queue, provided a sensible way of assigning priorities could
   be found. *)

module Workset : sig

  (* [insert node] inserts [node] into the workset. [node] must have no
     successors. *)
  val insert: node -> unit

  (* [repeat f] repeatedly applies [f] to a node extracted out of the
     workset, until the workset becomes empty. [f] is allowed to use
     [insert]. *)
  val repeat: (node -> unit) -> unit

  (* That's it! *)
end
= struct

  (* Initialize the workset. *)

  let workset =
    Queue.create()

  let insert node =
    Queue.push node workset

  let repeat f =
    while not (Queue.is_empty workset) do
      f (Queue.pop workset)
    done

end

(* -------------------------------------------------------------------------- *)

(* Signals. *)

(* A node in the workset has no successors. (It can have predecessors.)  In
   other words, a predecessor (an observer) of some node is never in the
   workset. Furthermore, a node never appears twice in the workset. *)

(* When a variable broadcasts a signal, all of its predecessors (observers)
   receive the signal. Any variable that receives the signal loses all of its
   successors (that is, it ceases to observe anything) and is inserted into
   the workset. This preserves the above invariant. *)

let signal subject =
  List.iter (fun observer ->
    Graph.clear_successors observer;
    Workset.insert observer
  ) (Graph.predecessors subject)
  (* At this point, [subject] has no predecessors. This plays no role in
     the correctness proof, though. *)

(* -------------------------------------------------------------------------- *)

(* Tables. *)

(* The permanent table maps variables that have reached a fixed point
   to properties. It persists forever. *)

let permanent : property M.t =
  M.create()

(* The transient table maps variables that have not yet reached a
   fixed point to nodes. (A node contains not only a property, but
   also a memoized right-hand side, and carries edges.) At the
   beginning of a run, it is empty. It fills up during a run. At the
   end of a run, it is copied into the permanent table and cleared. *)

let transient : node M.t =
  M.create()

(* [freeze()] copies the transient table into the permanent table, and
   empties the transient table. This allows all nodes to be reclaimed
   by the garbage collector. *)

let freeze () =
  M.iter (fun v node ->
    M.add v (property node) permanent
  ) transient;
  M.clear transient

(* -------------------------------------------------------------------------- *)

(* Workset processing. *)


(* [solve node] re-evaluates the right-hand side at [node]. If this leads to
   a change, then the current property is updated, and [node] emits a signal
   towards its observers. *)

(* When [solve node] is invoked, [node] has no subjects. Indeed, when [solve]
   is invoked by [node_for], [node] is newly created; when [solve] is invoked by
   [Workset.repeat], [node] has just been extracted out of the workset, and a
   node in the workset has no subjects. *)

(* [node] must not be in the workset. *)

(* In short, when [solve node] is invoked, [node] is neither awake nor asleep.
   When [solve node] finishes, [node] is either awake or asleep again. (Chances
   are, it is asleep, unless it is its own observer; then, it is awakened by the
   final call to [signal node].) *)

let rec solve (node : node) : unit =

  (* Retrieve the data record carried by this node. *)
  let data = Graph.data node in

  (* Prepare to compute an updated value at this node. This is done by
     invoking the client's right-hand side function.  *)

  (* The flag [alive] is used to prevent the client from invoking [request]
     after this interaction phase is over. In theory, this dynamic check seems
     required in order to argue that [request] behaves like a pure function.
     In practice, this check is not very useful: only a bizarre client would
     store a [request] function and invoke it after it has become stale. *)
  let alive = ref true
  and subjects = ref [] in

  (* We supply the client with [request], a function that provides access to
     the current valuation, and dynamically records dependencies. This yields
     a set of dependencies that is correct by construction. *)
  let request (v : variable) : property =
    assert !alive;
    try
      M.find v permanent
    with Not_found ->
      let subject = node_for v in
      let p = property subject in
      if not (P.is_maximal p) then
        subjects := subject :: !subjects;
      p
  in

  (* Give control to the client. *)
  let new_property = data.rhs request in

  (* From now on, prevent any invocation of this instance of [request]
     the client. *)
  alive := false;

  (* At this point, [node] has no subjects, as noted above. Thus, the
     precondition of [set_successors] is met. We can install [data.subjects]
     as the new set of subjects for this node. *)

  (* If we have gathered no subjects in the list [data.subjects], then
     this node must have stabilized. If [new_property] is maximal,
     then this node must have stabilized. *)

  (* If this node has stabilized, then it need not observe any more, so the
     call to [set_successors] is skipped. In practice, this seems to be a
     minor optimization. In the particular case where every node stabilizes at
     the very first call to [rhs], this means that no edges are ever
     built. This particular case is unlikely, as it means that we are just
     doing memoization, not a true fixed point computation. *)

  (* One could go further and note that, if this node has stabilized, then it
     could immediately be taken out of the transient table and copied into the
     permanent table. This would have the beneficial effect of allowing the
     detection of further nodes that have stabilized. Furthermore, it would
     enforce the property that no node in the transient table has a maximal
     value, hence the call to [is_maximal] above would become useless. *)

  if not (!subjects = [] || P.is_maximal new_property) then
    Graph.set_successors node !subjects;

  (* If the updated value differs from the previous value, record
     the updated value and send a signal to all observers of [node]. *)
  if not (P.equal data.property new_property) then begin
    data.property <- new_property;
    signal node
  end
  (* Note that equality of the two values does not imply that this node has
     stabilized forever. *)

(* -------------------------------------------------------------------------- *)

(* [node_for v] returns the graph node associated with the variable [v]. It is
   assumed that [v] does not appear in the permanent table. If [v] appears in
   the transient table, the associated node is returned. Otherwise, [v] is a
   newly discovered variable: a new node is created on the fly, and the
   transient table is grown. The new node can either be inserted into the
   workset (it is then awake) or handled immediately via a recursive call to
   [solve] (it is then asleep, unless it observes itself). *)

(* The recursive call to [solve node] can be replaced, if desired, by a call
   to [Workset.insert node]. Using a recursive call to [solve] permits eager
   top-down discovery of new nodes. This can save a constant factor, because
   it allows new nodes to move directly from [bottom] to a good first
   approximation, without sending any signals, since [node] has no observers
   when [solve node] is invoked. In fact, if the dependency graph is acyclic,
   the algorithm discovers nodes top-down, performs computation on the way
   back up, and runs without ever inserting a node into the workset!
   Unfortunately, this causes the stack to grow as deep as the longest path in
   the dependency graph, which can blow up the stack. *)

and node_for (v : variable) : node =
  try
    M.find v transient
  with Not_found ->
    let node = Graph.create { rhs = eqs v; property = P.bottom } in
    (* Adding this node to the transient table prior to calling [solve]
       recursively is mandatory, otherwise [solve] might loop, creating
       an infinite number of nodes for the same variable. *)
    M.add v node transient;
    solve node; (* or: Workset.insert node *)
    node

(* -------------------------------------------------------------------------- *)

(* Invocations of [get] trigger the fixed point computation. *)

(* The flag [inactive] prevents reentrant calls by the client. *)

let inactive =
  ref true

let get (v : variable) : property =
  try
    M.find v permanent
  with Not_found ->
    assert !inactive;
    inactive := false;
    let node = node_for v in
    Workset.repeat solve;
    freeze();
    inactive := true;
    property node

(* -------------------------------------------------------------------------- *)

(* Close the local module [LFP]. *)

end
in LFP.get

end
