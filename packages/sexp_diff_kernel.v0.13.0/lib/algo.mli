open Core_kernel

(** This module can be used for computing the diff of two sexps.
    It can identify insertion, deletion, and modification anywhere in the sexp.
    If you want to display the diff in a human-readable format, take a look at
    [Sexp_diff_display].

    This library intended for comparing serialized representations of the same OCaml type.
    It doesn't handle the case where a sub-sexp moves to a different depth in the
    updated sexp. Note that this case never occurs when comparing sexp representations
    of the same OCaml type.

    A common approach for diffing sexps is to serialize the sexps as strings and
    then diff the strings. While this is a reasonable approach, it means the diff
    algorithm is unable to use the tree structure to produce a better diff.
    Nodes which are very far apart in the tree can be close together in the serialized
    representation of the tree.

    This library computes the diff on the tree directly, without serializing the tree.

    The complexity is O(nm), where n is the number of nodes in the original sexp
    and m is the number of nodes in the updated sexp.

    Actually it's a bit better than that: if we denote by n_i the number of nodes
    in the first sexp at depth i, and m_i the number of nodes in the second sexp
    at depth i, the complexity is O(sum_i (n_i * m_i)).

    If the original and updated sexps are 10KB or less, this library will run in less
    than a second on them. Otherwise, this library may or may not be fast enough,
    and you should probably test its performance before using it.

    It's very similar the algorithm that's described in

    {v
      Stanley M. Selkow (1977)
      The Tree-to-tree Editing Problem
      Information Processing Letters, 6(6)
      https://pdfs.semanticscholar.org/2e2e/47f3748368797c9d51b08e938dfb930b97c3.pdf
    v}

    The differences are that we have a 'replace' operation which is distinguished
    from 'add' and 'delete', and in the paper every node can have a label, while
    with sexps only leaves (aka. atoms) can have labels.

    To explain exactly what this module does, we can define the functions
    [to_original: Diff.t -> Sexp.t] and
    [to_updated:  Diff.t -> Sexp.t].
    They are defined as follows:

    {[
      let extract ~project diff =
        let rec f = function
          | Delete x       -> project (Some x, None)
          | Add x          -> project (None,   Some x)
          | Replace (x, y) -> project (Some x, Some y)
          | Same x         -> Some x
          | Enclose diffs  ->
            let sexps = List.filter_map diffs ~f in
            Some (Sexp.List sexps)
        in
        Option.value_exn (f diff)
      ;;

      let to_original = extract ~project:fst
      let to_updated  = extract ~project:snd
    ]}

    Then we guarantee that:
    [to_original (diff ~original ~updated) = original] and
    [to_updated  (diff ~original ~updated) = updated]

    We also guarantee the following:
    [Diff.apply_exn              (diff ~original ~updated)  original = updated]  and
    [Diff.apply_exn (Diff.invert (diff ~original ~updated)) updated  = original]
*)

module Cache : sig
  type t

  val create : unit -> t
end

(* If you expect to diff similar sexps many times, you might want to use the same cache
   for all the diffs as this will improve performance. *)

val diff : original:Sexp.t -> updated:Sexp.t -> ?cache:Cache.t -> unit -> Diff.t
