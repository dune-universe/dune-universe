type _ t




val empty: _ t
(** An empty trie. *)




val find: Term.t -> int -> 'a t -> 'a option
(** [find term n trie]

    Find the value associated with the key [term] in [trie]. [term] is valid in
    a context of size [n]. Return [None] if no value can be found under the key
    [term].
*)




val add_new: Term.t -> int -> 'a -> 'a t -> ('a t, 'a) result
(** [add_new term n a trie]

    Add the key value pair [(term, a)] where [term] is valid in a context of
    size [n] to [trie]. Return the new trie or a value which has already been
    stored under the key [term].
*)

