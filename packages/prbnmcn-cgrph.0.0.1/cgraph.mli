(** Cgraph: a simple library for incremental computation *)

(** {1:introduction Introduction}

    The API exposed by Cgraph allows to construct a DAG where
    nodes hold functions. Users describe computations using this API; the library
    ensures that upon change of input, only the minimal amount of computation
    has to be performed to update the output.
    See the work by Umut Acar et al, and others for more details. *)

(** {1:example Example}

    We illustrate the library by considering the toy problem of
    evaluating an arithmetic expression. We consider a very simple data type,
    with just addition and negation. Variables are represented by strings
    and we'll use a string map to represent the environment in which
    the expression is evaluated.

    {[
        module String_map = Map.Make (String)

        type expr = Var of string | Add of expr * expr | Neg of expr
    ]}

    In the rest, we'll consider the following expression,
    corresponding to [(a + b) - (c + d)].
    {[
        let expr = Add (Add (Var "a", Var "b"), Neg (Add (Var "c", Var "d")))
    ]}

    In order to trace the evaluation of the naive vs incrementalized
    evaluators, we instrument addition and negation with printfs.
    {[
        let ( + ) x y =
          Format.printf "%d + %d@." x y ;
          x + y

        let ( - ) x =
          Format.printf "- %d@." x ;
          -x
    ]}

    The [Non_incremental] module defines a straightforward evaluator
    for our small language and evaluates it, changing the value
    of the variable ["a"] the second time.
    {[
        let env l = String_map.of_seq (List.to_seq l)

        module Non_incremental = struct
          let () = Format.printf "Non incremental computation@."

          let rec eval (env : int String_map.t) expr =
            match expr with
            | Var s -> String_map.find s env
            | Add (l, r) -> eval env l + eval env r
            | Neg e -> -eval env e

          let () = Format.printf "First evaluation@."

          let () =
            assert (eval (env [("a", 1); ("b", 2); ("c", 3); ("d", 4)]) expr = -4)

          let () = Format.printf "Second evaluation@."

          let () =
            assert (eval (env [("a", 3); ("b", 2); ("c", 3); ("d", 4)]) expr = -2)
        end
    ]}

    Evaluating this piece of code prints the following:
    {v
    Non incremental computation
    First evaluation
    3 + 4
    1 + 2
    3 + -7
    Second evaluation
    3 + 4
    3 + 2
    5 + -7
    v}

    Let's reiterate the experiment, using the library.
    {[
        module Incremental = struct
          let () = Format.printf "Incremental computation@."

          open Cgraph

          let rec eval (env : int t String_map.t) expr =
            match expr with
            | Var s -> String_map.find s env
            | Add (l, r) -> map2 (eval env l) (eval env r) ( + )
            | Neg e -> map (eval env e) ( ~- )

          let (a, b, c, d) = (Var.create 1, Var.create 2, Var.create 3, Var.create 4)

          let graph =
            eval (env [("a", var a); ("b", var b); ("c", var c); ("d", var d)]) expr

          let () = Format.printf "First evaluation@."

          let () = assert (get graph = -4)

          let () = Var.set a 3

          let () = Format.printf "Second evaluation@."

          let () = assert (get graph = -2)
        end
    ]}

    Here, instead of evaluating the expession, [Incremental.eval]
    builds a graph with inputs the variables [a,b,c,d] and output
    the node [graph]. We force the evaluation of a node
    by calling [Cgraph.get] on that node. This triggers the recursive
    evaluation of all {b and only} the nodes which are not up to date.

    Evaluating this piece of code prints the following:
    {v
    Incremental computation
    First evaluation
    1 + 2
    3 + 4
    3 + -7
    Second evaluation
    3 + 2
    5 + -7
    v}

    Notice how the node that didn't need to be recomputed wasn't!
*)

(** {1:api API documentation} *)

(** [Var] is the module type of variables, which are the first kind
      of input nodes available to users. *)
module Var : sig
  (** ['a t] is the type of a variable holding a value of type ['a] *)
  type 'a t

  (** [create x] creates a variable with initial value [x] *)
  val create : 'a -> 'a t

  (** [peek v] returns the value currently held in the variable. *)
  val peek : 'a t -> 'a

  (** [set v x] updates the variable [v] to hold the value [x].
        This automatically invalidates all nodes that depend directly
        and indirectly on [v]. *)
  val set : 'a t -> 'a -> unit
end

(** [Gen] is the module type of generators, which are
      the second kind of input nodes available to users. These
      correspond to streams of values. *)
module Gen : sig
  (** ['a t] is the type of a generator of values of type ['a]. *)
  type 'a t

  (** [create f] creates a generator using [f] as generating function. *)
  val create : (unit -> 'a) -> 'a t

  (** [touch g] invalidates all nodes that depend directly and indirectly
        on [g] and generates a new value to replace the old one. *)
  val touch : 'a t -> unit
end

(** The type of nodes. *)
type !'a t

(** Inject a variable as a node. *)
val var : 'a Var.t -> 'a t

(** Inject a generator as a node. *)
val gen : 'a Gen.t -> 'a t

(** [get n] computes the currenet value associated to [n]. This might
      recursively trigger the recomputation of all currently invalidated node
      on which [n] depends. *)
val get : 'a t -> 'a

(** [return x] is a node that holds the constant value [x]. Can never be invalidated. *)
val return : 'a -> 'a t

(** [map n f] is a node whose value is equal to [f] applied to the value of [n]. *)
val map : 'a t -> ('a -> 'b) -> 'b t

(** [map2 n1 n2 f] is a node whose value is equal to [f] applied to the values
      of [n1] and [n2]. *)
val map2 : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t

(** See [map2]. *)
val map3 : 'a t -> 'b t -> 'c t -> ('a -> 'b -> 'c -> 'd) -> 'd t

(** [bind m f] allows to construct graphs dynamically. Use only if you really need
      it, as this induces the extra overhead of garbage collecting nodes. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [if c t f] constructs a node whose value is equal to that of [t] if
      [c] has value [true], or that of [f] in the other case. *)
val if_ : bool t -> 'a t -> 'a t -> 'a t

(** Attach an arbitrary callback to a node, to be called when the value in the
      node is updated. *)
val on_update : 'a t -> ('a -> unit) -> unit

(** Infix operators, for convenience. *)
module Infix : sig
  (** Alias to [bind] *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Alias to [bind] *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Alias to [map] *)
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
end

(** Existentially packed nodes. Not useful to end users. *)
type ex

val ex : 'a t -> ex

(** Functions useful for debugging. *)
module Internal : sig
  type mode = Backward_only | Full

  type edge = Dependency of ex | Dynamic of ex

  val uid : ex -> int

  val upstream : ex -> ex list

  val used_by : ex -> ex list

  val valid : ex -> bool

  module Node_table : Hashtbl.S with type key = ex

  val compute_graph : mode -> ex -> edge list Node_table.t

  (** Export a graph to [graphviz] format. *)
  val to_dot : ?name:string -> mode:mode -> ex -> out_channel -> unit
end
