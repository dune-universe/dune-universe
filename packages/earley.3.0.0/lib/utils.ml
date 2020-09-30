(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

(* Comparison function accepting to compare everything. *)
let eq_closure : type a. a -> a -> bool =
  fun f g ->
    let open Obj in
    (* repr f == repr g
       || (Marshal.to_string f [Closures] = Marshal.to_string g [Closures]) *)
    let adone = ref [] in
    let rec fn f g =
      f == g ||
        match is_int f, is_int g with
        | true, true -> f == g
        | false, true | true, false -> false
        | false, false ->
           let ft = tag f and gt = tag g in
           if ft = forward_tag then (
             fn (field f 0) g)
           else if gt = forward_tag then (
             fn f (field g 0))
           else if ft <> gt then false
           else
           if ft = string_tag || ft = double_tag || ft = double_array_tag
             then f = g
           else if ft = abstract_tag || ft = out_of_heap_tag
                   || ft = no_scan_tag || ft = custom_tag || ft = infix_tag
                 (* FIXME: we could certainly do better with infix_tag
                           i.e. mutually recursive functions *)
             then f == g
           else
             size f == size g &&
               let rec gn i =
                 if i < 0 then true
                 else fn (field f i) (field g i) && gn (i - 1)
               in
               List.exists (fun (f',g') -> f == f' && g == g') !adone ||
                (List.for_all (fun (f',g') -> f != f' && g != g') !adone &&
                 (adone := (f,g)::!adone;
                  gn (size f - 1)))

    in fn (repr f) (repr g)

(* Custom hash table module. [Hashtbl] won't  do  because  it  does  not
   accept keys that contain closures. Here a custom  comparing  function
   can be provided at the creation of the hash table. *)
module EqHashtbl :
  sig
    type ('a, 'b) t

    val create : int -> ('a, 'b) t
    val add    : ('a, 'b) t -> 'a -> 'b -> unit
    val find   : ('a, 'b) t -> 'a -> 'b
    val iter   : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  end =
  struct
    type ('a, 'b) t =
      { mutable nb_buckets : int
      ; mutable buckets    : ('a * 'b) list array
      ; mutable max_size   : int
      ; mutable size_limit : int }

    let rec log2 n = if n <= 0 then 0 else 1 + log2 (n lsr 1)

    let create : int -> ('a, 'b) t =
      fun nb_buckets ->
        let nb_buckets = max nb_buckets 8 in
        let buckets = Array.make nb_buckets [] in
        let size_limit = log2 nb_buckets + 7 in
        { nb_buckets ; buckets ; max_size = 0 ; size_limit }

    let iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit =
      fun fn h ->
        Array.iter (List.iter (fun (k,v) -> fn k v)) h.buckets

    let hash = Hashtbl.hash

    let find_bucket : ('a, 'b) t -> 'a -> int =
      fun h k -> hash k mod h.nb_buckets

    exception Size_is of int
    let rec add : ('a, 'b) t -> 'a -> 'b -> unit =
      fun h k v ->
        let i = find_bucket h k in
        let rec remove sz = function
          | []                             -> raise (Size_is sz)
          | (kv,_) :: ls when eq_closure k kv -> ls
          | e      :: ls                   -> e :: remove (sz+1) ls
        in
        try h.buckets.(i) <- (k,v) :: remove 0 h.buckets.(i)
        with Size_is(sz) ->
          h.buckets.(i) <- (k,v) :: h.buckets.(i);
          h.max_size <- max h.max_size sz;
          if h.max_size > h.size_limit then grow h

    and grow : ('a, 'b) t -> unit =
      fun h ->
        let old_tbl = h.buckets in
        h.nb_buckets <- h.nb_buckets * 2;
        h.buckets <- Array.make h.nb_buckets [];
        h.size_limit <- h.size_limit + 1;
        h.max_size <- 0;
        Array.iter (List.iter (fun (k,v) -> add h k v)) old_tbl

    let find : ('a, 'b) t -> 'a -> 'b =
      fun h k ->
        let i = find_bucket h k in
        let rec find = function
          | []         -> raise Not_found
          | (kv,v)::xs -> if eq_closure k kv then v else find xs
        in
        find h.buckets.(i)
  end

(** This modules implements a computation of a fixpoints for valus
    that depends upon other values. Cycles are handled through update of
    references. If the fixpoint is not reached, this might loop.

    This modules ressemble a little the Lazy module.
*)
module Fixpoint :
  sig
    type 'a t

    (** Standard way to construct a value of type ['a t] *)
    val from_val  : 'a -> 'a t
    val from_fun  : 'a t -> ('a -> 'b) -> 'b t
    val from_fun2 : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
    val from_funl : 'a t list -> 'b -> ('b -> 'a -> 'b) -> 'b t

    (** value obtained by reading 'b which is mutable *)
    val from_ref  : 'b -> ('b -> 'a t) -> 'a t

    (** Must be called when updating a mutable field used in [from_ref]  *)
    val update    : 'a t -> unit

    (** Reading the value *)
    val force     : 'a t -> 'a
  end =
  struct
    module rec H :
      sig
        type 'a fix =
          { mutable value  : 'a
          ; compute        : unit -> unit
          ; mutable deps   : W.t option
          ; mutable is_ref : ('a fix * (unit -> 'a fix)) option
          ; ident          : int }

        include Hashtbl.HashedType with type t = Obj.t fix
      end =
      struct
        type 'a fix =
          { mutable value  : 'a
          ; compute        : unit -> unit
          ; mutable deps   : W.t option
          ; mutable is_ref : ('a fix * (unit -> 'a fix)) option
          ; ident          : int }

        type t = Obj.t fix

        let equal a b = a.ident = b.ident

        let hash a = a.ident
      end
    and W : Weak.S with type data = H.t = Weak.Make(H)

    open H
    type 'a t = 'a fix

    let force : 'a t -> 'a = fun b -> b.value

    let new_id =
      let r = ref 0 in
      (fun () -> let x = !r in r := x + 1; x)

    let add_deps r {deps;_} =
      match deps with
      | None     -> true
      | Some tbl ->
          let r = Obj.magic r in
          if not (W.mem tbl r) then W.add tbl r;
          false

    let iter_deps fn {deps;_} =
      match deps with
      | None     -> ()
      | Some tbl -> W.iter (fun v -> fn (Obj.magic v)) tbl

    let from_val value =
      { value
      ; compute = ignore
      ; deps    = None
      ; is_ref  = None
      ; ident   = new_id () }

    let from_fun l fn =
      let rec res =
        { value   = fn l.value
        ; compute = (fun () -> res.value <- fn l.value)
        ; deps    = Some (W.create 7)
        ; is_ref  = None
        ; ident   = new_id () }
      in
      if add_deps res l then res.deps <- None;
      res

    let from_fun2 l1 l2 fn =
      let rec res =
        { value   = fn l1.value l2.value
        ; compute = (fun () -> res.value <- fn l1.value l2.value)
        ; deps    = Some (W.create 7)
        ; is_ref  = None
        ; ident   = new_id () }
      in
      let b1 = add_deps res l1 in
      let b2 = add_deps res l2 in
      if b1 && b2 then res.deps <- None;
      res

    let rec fold l a f =
      match l with
      | []   -> a
      | x::l -> fold l (f a x.value) f

    let from_funl l a fn =
      let rec res =
        { value   = fold l a fn
        ; compute = (fun () -> res.value <- fold l a fn)
        ; deps    = Some (W.create 7)
        ; is_ref  = None
        ; ident   = new_id () }
      in
      let fn acc x = add_deps res x && acc in
      if List.fold_left fn true l then res.deps <- None;
      res

    let from_ref l fn =
      let a = fn l in
      let rec res =
        { value   = a.value
        ; compute = (fun () -> res.value <- (fn l).value)
        ; deps    = Some (W.create 7)
        ; is_ref  = Some (a, fun () -> fn l)
        ; ident   = new_id () }
      in
      ignore (add_deps res a);
      res

    let update b =
      begin
        match b.is_ref with
        | None      -> invalid_arg "Fixpoint.update";
        | Some(_,f) ->
            let a' = f () in
            ignore (add_deps b a');
            b.is_ref <- Some (a', f)
      end;
      let rec fn x =
        let old = x.value in x.compute ();
        if old <> x.value then iter_deps fn x
      in fn b
  end
