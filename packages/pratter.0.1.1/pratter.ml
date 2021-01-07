(** This modules defines a functor whose image is a parser for terms with
    applications, binary and unary operators. These terms are specified in the
    argument of the functor.

    The algorithm implemented is an extension of the Pratt parser. The Sunting
    Yard algorithm could also be used.
    @see <https://dev.to/jrop/pratt-parsing>
    @see <https://effbot.org/zone/simple-top-down-parsing.htm> *)

(** Associativity of an operator. *)
type associativity =
  | Left
      (** If [+] is a left associative operator, [x + y + z] is parsed [(x +
          y) + z]. *)
  | Right
      (** If [+] is a right associative operator, [x + y + z] is parsed [x +
          (y + z)]. *)

(** Reprensentation of operators. *)
type operator =
  | Unary  (** Unary prefix operators. *)
  | Binary of associativity
      (** Binary infix operators with its associativity. *)

type priority = float
(** Priority of operators. If [*] has a higher priority than [+], than [x + y *
    z] is parsed [x + (y * z)]. *)

(** Types and utilities on terms that are to be Pratt parsed. *)
module type SUPPORT = sig
  type term
  (** The main type of terms, that contains symbols, applications, binary and
      unary operators. *)

  type pos
  (** The type of positions. *)

  type popt = pos option

  type ident
  (** Type of identifiers of symbols. *)

  val get_ident : term -> (ident * popt) option
  (** [get_ident t] returns the identifier and (optional) position of term [t],
      if [t] is an identifier. *)

  val make_appl : term -> term -> term
  (** [make_appl t u] returns the application of [t] to [u], sometimes noted
      [@(t, u)], or just [t u]. *)

  val make_bin_appl :
    term -> popt -> ident * associativity * priority -> term -> term
  (** [make_bin_appl t p op u] returns the application of binary operator [op]
      in position [p] to terms [t] and [u], so the term [t op u]. *)

  val make_una_appl : popt -> ident * priority -> term -> term
  (** [make_una_appl p op t] returns the application of unary operator [op] in
      position [p] to term [t], so the term [op t]. *)
end

module Make : functor (Sup : SUPPORT) -> sig
  val add_unary : Sup.ident -> priority -> unit
  (** [add_unary id pr] adds unary operator identified by [id] with priority
      [pr] to the table of operators. *)

  val add_binary : Sup.ident -> priority -> associativity -> unit
  (** [add_binary id pr assoc] adds binary operator identified by [id] with
      priority [pr] to the table of operators, with associativity [assoc]. *)

  val flush : unit -> unit
  (** [flush ()] empties the table of operators. *)

  val expression : ?rbp:priority -> Sup.term Stream.t -> Sup.term
  (** [expression rbp s] parses stream of tokens [s] with right binding power
      [rbp] (which is 0 by default). It transforms a sequence of applications to
      a structured application tree containing infix and prefix operators. For
      instance, assuming that [+] is declared infix, it transforms [3 + 5 + 2],
      represented as [@(@(@(@(3,+),5),+),2)] (where [@] is the application) into
      [(@(+(@(+,3,5)),2)]. *)
end =
functor
  (Sup : SUPPORT)
  ->
  struct
    (** Table containing all registered binary and unary operators that may
        appear in terms parsed by {!val:Pratt.expression}. *)
    let operators : (Sup.ident, operator * priority) Hashtbl.t =
      Hashtbl.create 17

    let add_unary s p = Hashtbl.add operators s (Unary, p)
    let add_binary s p a = Hashtbl.add operators s (Binary a, p)
    let flush () = Hashtbl.reset operators

    (** [lbp t] returns the left binding power of term [t] (which is 0 if [t] is
        not an operator). *)
    let lbp : Sup.term -> priority =
     fun pt ->
      match Sup.get_ident pt with
      | Some (s, _) -> (
          match Hashtbl.find_opt operators s with
          | Some (Binary _, bp) | Some (Unary, bp) -> bp
          | None -> assert false )
      | _ -> (* [t] must be an operator *) assert false

    (* NOTE: among the four functions operating on streams, only [expression]
       consumes elements from it. *)

    (** [is_binop t] returns [true] iff term [t] is a binary operator. *)
    let is_binop : Sup.term -> bool =
     fun t ->
      match Sup.get_ident t with
      | Some (s, _) -> (
          match Hashtbl.find_opt operators s with
          | Some (Binary _, _) -> true
          | _ -> false )
      | _ -> false

    (** [nud t] is the production of term [t] with {b no} left context. If [t]
        is not an operator, [nud] is the identity. Otherwise, the output is a
        production rule. *)
    let rec nud : Sup.term Stream.t -> Sup.term -> Sup.term =
     fun strm t ->
      match Sup.get_ident t with
      | Some (s, p) -> (
          match Hashtbl.find_opt operators s with
          | Some (Unary, rbp) ->
              Sup.make_una_appl p (s, rbp) (expression ~rbp strm)
          | _ -> t )
      | _ -> t

    (** [led left t] is the production of term [t] with left context
        [left]. *)
    and led : Sup.term Stream.t -> Sup.term -> Sup.term -> Sup.term =
     fun strm left t ->
      match Sup.get_ident t with
      | Some (s, p) -> (
          match Hashtbl.find_opt operators s with
          | Some (Binary assoc, bp) ->
              let rbp =
                if assoc = Right then bp *. (1. -. epsilon_float) else bp
              in
              Sup.make_bin_appl left p (s, assoc, bp) (expression ~rbp strm)
          | _ -> assert false (* [t] must be an operator. *) )
      | _ -> (* [t] must be an operator *) assert false

    and expression : ?rbp:priority -> Sup.term Stream.t -> Sup.term =
     fun ?(rbp = 0.) strm ->
      (* [aux left] inspects the stream and may consume one of its elements, or
         return [left] unchanged. *)
      let rec aux left =
        match Stream.peek strm with
        | None -> left
        | Some pt when is_binop pt ->
            (* If [pt] has a higher left binding power than the binding power of
               the previous operator in the stream. *)
            if lbp pt > rbp then
              (* Performed before to execute side effect on stream. *)
              let next = Stream.next strm in
              aux (led strm left next)
            else left
        | Some _ ->
            (* argument of an application *)
            let next = Stream.next strm in
            let right = nud strm next in
            aux (Sup.make_appl left right)
      in

      let next = Stream.next strm in
      let left = nud strm next in
      aux left
  end
