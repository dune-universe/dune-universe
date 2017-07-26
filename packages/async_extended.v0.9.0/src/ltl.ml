open Core
open Async

(* The algorithm idea is as follows. Define

     sol(phi, E, xs) = { E' <= E | xs |= phi, E' }

   where E' <= E means that E' defines all variables in var(phi) and agrees with
   E for all variables where E is defined. The key insight is that we can define
   a set of rules of the form

     (phi, E), x -> (phi', E')

   such that E' <= E for each rule and                                    ( * )

   sol(phi, E, x :: xs) = U { sol(phi', E', xs)                           ( ** )
                              | exists rule (phi, E), x -> (phi', E') }

   Now the algorithm just needs to maintain a set of constraints of the form
   (phi, E) and update them using (**) after seeing each x. This is done in
   [Constraint.step].

   An important part is the rule for conjunctions:

   (phi_1, E), x -> (phi_2, E')     (psi_1, E'), x -> (psi_2, E'')
   ----------------------------------------------------------------
   (phi_1 /\ psi_1), E -> (phi_2 /\ psi_2), E''

   Consider the formula [(x == 12) /\ (x != 12)]. The rule will evaluate this to
   false: the first conjunct will bind x to 12 in E, which will lead to failure
   when evaluating the second conjunct. It is important that E is threaded
   through each conjunct.

   Note that [(x != 12) /\ (x == 12)] wouldn't work - we can't do anything with
   [x != 12] if x is not bound already. Order matters, and so we do sorting of
   conjuncts in [normalize]. We also check statically that this results in a
   formula that doesn't fail.

   Optimization
   -------------

   Applying a rewriting rule to each contraint at each state would in most cases
   lead to quadratic behavior. However, we know that a constraint of the form

      ( Eventually (x == field1), {x -> 42} )

   will not change its value unless the incoming state has field1 = 42. This
   allows us to put such a constraint to sleep in a hashtable, with the guard
   (field1 == 42) as the key. The module [Sleeping_constraints] implements this
   optimization in a slightly more general way - it allows multiple guards, only
   one of which needs to be satisfied.

   Proof
   -----

   Here's how to prove ( ** ) for the conjunction rule. We use /\ for both
   conjunction and set intersection.

   sol(phi_1 /\ psi_1, E, x :: xs)
   = sol(phi_1, E, x :: xs) /\ sol(psi_1, E, x :: xs)
   = (U { sol(phi_2, E', xs)
       | exists rule (phi_1, E), x -> (phi_2, E') })
     /\ sol(psi_1, E, x :: xs)
   = U { sol(phi_2, E', xs) /\ sol(psi_1, E, x :: xs)
       | exists rule (phi_1, E), x -> (phi_2, E') }
-->
   = U { sol(phi_2, E', xs) /\ sol(psi_1, E', x :: xs)
       | exists rule (phi_1, E), x -> (phi_2, E') }
   = U { sol(phi_2, E', xs) /\ U { sol(phi_2, E'', xs)
                                   | exists rule (phi_2, E'), x -> (phi_2, E'') }
       | exists rule (phi_1, E), x -> (phi_2, E') }
   = U { sol(phi_2, E', xs) /\ sol(phi_2, E'', xs)
        | exists rule (phi_1, E ),   -> (phi_2, E') and
          exists rule (phi_2, E'), x -> (phi_2, E'') }
-->
   = U { sol(phi_2, E'', xs) /\ sol(phi_2, E'', xs)
        | exists rule (phi_1, E ), x -> (phi_2, E') and
          exists rule (phi_2, E'), x -> (phi_2, E'') }
   = U { sol(phi_2 /\ psi_2, E'', xs)
        | exists rule (phi_1, E ), x -> (phi_2, E') and
          exists rule (phi_2, E'), x -> (phi_2, E'') }

   The crucial steps are replacing E by E' and E' by E'' where marked by ---->
   This is justified by ( * ) which guarantees E'' <= E' <= E.
*)

let compare = `You_are_using_polymorphic_compare
let _ = compare

module type State = sig
  type t
  val to_string : t -> string
  val time : t -> Time_ns.t option
end

module Make (State: State) = struct
  module Field = struct
    type 'a t =
      { id : 'a Type_equal.Id.t
      ; get : State.t -> 'a option
      ; hashable : 'a Hashtbl.Hashable.t
      } [@@deriving fields]

    let create ~name ~get hashable =
      let id = Type_equal.Id.create ~name hashable.Hashtbl.Hashable.sexp_of_t in
      { id; get; hashable }

    let name t =
      Type_equal.Id.name t.id

    let compare t1 t2 =
      Type_equal.Id.Uid.compare
        (Type_equal.Id.uid t1.id)
        (Type_equal.Id.uid t2.id)

    let same t1 t2 =
      Type_equal.Id.same t1.id t2.id

    let sexp_of_t _sexp_of_a t =
      Sexp.Atom (Type_equal.Id.name t.id)

    let compare_values t =
      t.hashable.compare

    let time =
      { id = Type_equal.Id.create ~name:"time" Time_ns.sexp_of_t
      ; get = (fun state -> State.time state)
      ; hashable = Time_ns.hashable
      }
  end

  module Variable = struct
    include Type_equal.Id

    let to_string t =
      Type_equal.Id.name t

    let sexp_of_t _ t =
      Sexp.Atom (to_string t)

    let create ?sexp_of name =
      let sexp_of = Option.value sexp_of ~default:sexp_of_opaque in
      Type_equal.Id.create ~name sexp_of

    module Id = struct
      module T = struct
        type t = string * Uid.t [@@deriving compare]

        let sexp_of_t (name, _) =
          Sexp.Atom name

        let t_of_sexp _ =
          failwith "undefined"
      end

      include T
      include Comparable.Make (T)
    end

    let id t =
      to_string t, Type_equal.Id.uid t

    let compare_uids t1 t2 =
      Uid.compare (uid t1) (uid t2)

    let compare _ t1 t2 =
      compare_uids t1 t2
  end

  module Assignment = Univ_map

  module Expression = struct
    module T = struct
      (* Keep track of the free variables of the expression. *)
      type 'a t = (State.t -> Assignment.t -> 'a) * Variable.Id.t list

      include Applicative.Make (struct
          type nonrec 'a t = 'a t

          let return x = (fun _ _ -> x), []

          let apply (f, f_vars) (e1, e1_vars) =
            let e2 state values = (f state values) (e1 state values) in
            e2, f_vars @ e1_vars

          let map (e1, vs) ~f =
            let e2 state values = f (e1 state values) in
            e2, vs

          let map = `Custom map
        end)
    end

    include T
    module Args = Applicative.Make_args (T)

    let maybe_field fld =
      let f state _values = Field.get fld state in
      f, []

    (* Using an exception to shortcut (and hopefully speed up) evaluation of
       boolean expressions. This relies on the fact that expressions are only
       evaluated in predicates via [eval]. A more general way would be to define
       [eval : 'a t -> 'a option].  *)
    exception Missing_field

    let field fld =
      let f state _values =
        match Field.get fld state with
        | None -> raise Missing_field
        | Some value -> value
      in
      f, []

    let variable v =
      (* Validation will make sure this never fails. That is the whole purpose
         of keeping track of the variables. *)
      let f _state values = Assignment.find_exn values v in
      f, [Variable.id v]

    let variables (_e, vs) = vs

    (** precondition: [values] contains bindings for all [variables t] *)
    let eval (e, _vs) state values : bool =
      try e state values with Missing_field -> false
  end

  module Predicate = struct
    type t =
      { description : string option
      ; exp : bool Expression.t
      }

    let sexp_of_t t =
      [%sexp_of: string option] t.description

    let eval t state values =
      Expression.eval t.exp state values

    let variables t =
      Expression.variables t.exp
  end

  (* Hack to make sexp conversion work for GADTs. *)
  let _of_a _ =
    failwith "_of_a undefined in LTL.sexp_of_t"

  type t =
    | True
    | False
    (* Tag predicates so we can define a comparison function *)
    | Predicate     of int * Predicate.t
    | Not_predicate of int * Predicate.t
    | Eq     : 'a Variable.t * 'a Field.t -> t
    | Not_eq : 'a Variable.t * 'a Field.t -> t
    | And of t list
    | Or  of t list
    (* Tail satisfies t. Undefined on empty sequence. *)
    | Delay of t
    (* Sequence is non-empty and satisfies t.
       Now t = Until (False, t) *)
    | Now         of t
    (* Sequence is empty or satisfies t.
       Weak_now t = Release (True, t) *)
    | Weak_now    of t
    | Until   of t * t
    | Release of t * t
  [@@deriving sexp_of]

  (* No [with compare] for GADTs, so I auto-generated the comparison function
     for a modified type and tweaked it. We could go by constructing an
     intermediate value first, but I'm not keen on allocating a new value just
     for comparison.  *)
  let rec compare t1 t2 =
    let rec loop =
      function
      | a ->
        (function
          | b ->
            (match (a, b) with
             | ([],[]) -> 0
             | ([],_) -> (-1)
             | (_,[]) -> 1
             | (x::xs,y::ys) ->
               let n = compare x y in
               if Pervasives.(=) n 0
               then loop xs ys
               else n))
    in
    if phys_equal t1 t2 then 0
    else begin
      match t1, t2 with
      | True, True -> 0
      | True, _    -> (-1)
      | _,    True -> 1
      | False, False -> 0
      | False, _     -> (-1)
      | _,     False -> 1
      | Predicate (id1, _), Predicate (id2, _) ->
        Int.compare id1 id2
      | Predicate _, _  -> (-1)
      | _, Predicate _ -> 1
      | Not_predicate (id1, _), Not_predicate (id2, _) ->
        Int.compare id1 id2
      | Not_predicate _, _ -> (-1)
      | _, Not_predicate _ -> 1
      | Eq (v1, field1), Eq (v2, field2) ->
        let ret = Variable.compare_uids v1 v2 in
        if ret <> 0 then ret
        else Field.compare field1 field2
      | Eq _, _ -> (-1)
      | _, Eq _ -> 1
      | Not_eq (v1, field1), Not_eq (v2, field2) ->
        let ret = Variable.compare_uids v1 v2 in
        if ret <> 0 then ret
        else Field.compare field1 field2
      | Not_eq _, _ -> (-1)
      | _, Not_eq _ -> 1
      | And ts1, And ts2 ->
        loop ts1 ts2
      | And _, _ -> (-1)
      | _, And _ -> 1
      | Or ts1, Or ts2 ->
        loop ts1 ts2
      | Or _, _ -> (-1)
      | _, Or _ -> 1
      | Delay t1, Delay t2 ->
        compare t1 t2
      | Delay _, _ -> (-1)
      | _, Delay _ -> 1
      | Now t1, Now t2 ->
        compare t1 t2
      | Now _, _ -> (-1)
      | _, Now _ -> 1
      | Weak_now t1, Weak_now t2 ->
        compare t1 t2
      | Weak_now _, _ -> (-1)
      | _, Weak_now _ -> 1
      | Until (a1, b1), Until (a2, b2) ->
        let ret = compare a1 a2 in
        if ret <> 0 then ret
        else compare b1 b2
      | Until _, _ -> (-1)
      | _, Until _ -> 1
      | Release (a1, b1), Release (a2, b2) ->
        let ret = compare a1 a2 in
        if ret <> 0 then ret
        else compare b1 b2
    end

  let to_string t =
    Sexp.to_string (sexp_of_t t)

  let map_children ~f t = match t with
    | True | False
    | Predicate _  | Not_predicate _
    | Eq _ | Not_eq _ -> t
    | And ts -> And (List.map ~f ts)
    | Or ts -> Or (List.map ~f ts)
    | Until (t1, t2) -> Until (f t1, f t2)
    | Now t -> Now (f t)
    | Weak_now t -> Weak_now (f t)
    | Delay t -> Delay (f t)
    | Release (t1, t2) -> Release (f t1, f t2)

  let iter_children ~f = function
    | True | False
    | Predicate _ | Not_predicate _
    | Eq _ | Not_eq _ -> ()
    | And ts | Or ts ->
      List.iter ~f ts
    | Now t | Weak_now t
    | Delay t -> f t
    | Until (t1, t2) | Release (t1, t2) ->
      f t1; f t2

  let rec negate = function
    | True -> False
    | False -> True
    | Predicate (id, p) -> Not_predicate (id, p)
    | Not_predicate (id, p) -> Predicate (id, p)
    | Eq (x,y) -> Not_eq (x,y)
    | Not_eq (x,y) -> Eq (x,y)
    | And xs -> Or (List.map xs ~f:negate)
    | Or xs -> And (List.map xs ~f:negate)
    | Delay t -> Delay (negate t)
    | Now t -> Weak_now (negate t)
    | Weak_now t -> Now (negate t)
    | Until (x,y) -> Release (negate x, negate y)
    | Release (x,y) -> Until (negate x, negate y)

  let conj ts =
    let rec add acc = function
      | [] ->
        begin match acc with
        | [] -> True
        | [t] -> t
        | ts -> And ts
        end
      | t :: ts ->
        match t with
        | False -> False
        | True -> add acc ts
        | And ts' -> add acc (ts' @ ts)
        | t -> add (t :: acc) ts
    in
    add [] ts

  let disj ts =
    let rec add acc = function
      | [] ->
        begin match acc with
        | [] -> False
        | [t] -> t
        | ts -> Or ts
        end
      | t :: ts ->
        match t with
        | True -> True
        | False -> add acc ts
        | Or ts' -> add acc (ts' @ ts)
        | t -> add (t :: acc) ts
    in
    add [] ts

  let const b = if b then True else False
  let eq var expr = Eq (var, expr)
  let implies p q = disj [negate p; q]
  let until p q = Until (p, q)
  let release p q = Release (p, q)
  let always p = Release (False, p)
  let eventually p = Until (True, p)
  let next t = Now (Delay t)
  let now t = Now t

  let predicate_id = ref 0

  let next_predicate_id () =
    incr predicate_id;
    !predicate_id

  let predicate ?description exp =
    Predicate (next_predicate_id (), {Predicate. description; exp})

  let has fld =
    let description = sprintf "has %s" (Field.name fld) in
    predicate ~description
      Expression.(map ~f:Option.is_some (maybe_field fld))

  let field_predicate ?description fld f =
    predicate ?description Expression.(map ~f (field fld))

  let before before_time =
    predicate
      ~description:(sprintf !"before %{sexp:Time_ns.t}" before_time)
      Expression.(
        map
          (maybe_field Field.time)
          ~f:(Option.value_map ~default:false ~f:(Time_ns.(>) before_time))
      )

  let after after_time =
    predicate
      ~description:(sprintf !"after %{sexp:Time_ns.t}" after_time)
      Expression.(
        map
          (maybe_field Field.time)
          ~f:(Option.value_map ~default:false ~f:(Time_ns.(<) after_time))
      )

  let before_var ?(add = Time_ns.Span.zero) vtime =
    predicate Expression.(
      map2 (variable vtime) (field Field.time)
        ~f:(fun var_time field_time ->
          field_time <= Time_ns.add var_time add))

  let after_var ?(add = Time_ns.Span.zero) vtime =
    predicate Expression.(
      map2 (variable vtime) (field Field.time)
        ~f:(fun var_time field_time ->
          field_time >= Time_ns.add var_time add))

  let rec true_at_eof t =
    let fail_empty () =
      failwithf !"Tried to evaluate %{sexp:t} on an empty sequence." t ()
    in
    match t with
    | True -> true
    | False -> false
    | Predicate _
    | Not_predicate _
    | Eq _
    | Not_eq _ -> fail_empty ()
    | And ts ->
      List.for_all ts ~f:true_at_eof
    | Or ts  ->
      List.exists ts ~f:true_at_eof
    | Now _ -> false
    | Weak_now _ -> true
    | Delay _ -> fail_empty ()
    | Until _ -> false
    | Release _ -> true

  module O = struct
    let (&&) t1 t2 = conj [t1; t2]
    let (||) t1 t2 = disj [t1; t2]
    let not = negate
    let (==) = eq
    let (==>) = implies
  end

  module Guard : sig
    type t = Eq : 'a Field.t * 'a -> t
    include Hashable with type t := t
  end = struct
    module T = struct
      type t = Eq : 'a Field.t * 'a -> t

      module Sexp = struct
        type t = Eq of Sexp.t * Sexp.t [@@deriving sexp_of]
      end

      let sexp_of_t (Eq (field, value)) =
        Sexp.Eq (Field.sexp_of_t sexp_of_opaque field,
                 field.hashable.sexp_of_t value)
        |> Sexp.sexp_of_t

      let compare (Eq (field1, value1)) (Eq (field2, value2)) =
        match Type_equal.Id.same_witness field1.id field2.id with
        | None ->
          let uid1 = Type_equal.Id.uid field1.id in
          let uid2 = Type_equal.Id.uid field2.id in
          Type_equal.Id.Uid.compare uid1 uid2
        | Some Type_equal.T ->
          Field.compare_values field1 value1 value2

      let hash (Eq (field, value)) =
        Hashtbl.hash (Type_equal.Id.hash field.id, field.hashable.hash value)

      let t_of_sexp _ =
        failwith "Guard.t_of_sexp undefined"
    end

    include T
    include Hashable.Make (T)
  end

  module Constraint : sig
    type formula = t
    type t [@@deriving sexp_of]
    include Comparable with type t := t
    val create : formula -> t
    val step : t -> State.t -> t list
    val eof : t -> Assignment.t option
    val is_determined : t -> Assignment.t option
    (* If not None, the value of the constraint will not change until we see a
       state that satisfies at least one of the guards. *)
    val no_change_unless : t -> Guard.t list option
  end = struct
    open O

    type formula = t [@@deriving sexp_of, compare]

    module T = struct
      module Assignment = struct
        include Assignment
        let compare = Polymorphic_compare.compare
      end

      type t = formula * Assignment.t [@@deriving sexp_of, compare]

      let t_of_sexp _ =
        failwith "Constraint.t_of_sexp undefined"
    end

    include T

    let create formula =
      formula, Assignment.empty

    let is_determined = function
      | (True, values) -> Some values
      | _ -> None

    let is_false = function
      | (False, _) -> true
      | _ -> false

    let eof (formula, values) =
      if true_at_eof formula then Some values else None

    let step (formula, values) state =
      let rec step formula values =
        let open List.Monad_infix in
        match formula with
        | True  -> [True, values]
        | False -> []
        | Predicate (_, p) ->
          if Predicate.eval p state values then [True, values] else []
        | Not_predicate (_, p) ->
          if Predicate.eval p state values then [] else [True, values]
        | Eq (var, field) ->
          begin match Field.get field state with
          | None -> []
          | Some value ->
            match Assignment.find values var with
            | None ->
              [True, Assignment.set values var value]
            | Some value' ->
              if Field.compare_values field value value' = 0
              then [True, values] else []
          end
        | Not_eq (var, field) ->
          begin match Field.get field state with
          | None -> [True, values]
          | Some value ->
            match Assignment.find values var with
            | None ->
              (* This will never get triggered by a formula that satisfies
                 [Normalize.check_bound]. *)
              failwithf "BUG: Tried to check formula %s before %s was unified"
                (to_string formula)
                (Variable.to_string var) ()
            | Some value' ->
              if Field.compare_values field value value' = 0
              then [] else [True, values]
          end
        | And ts ->
          let rec add acc values = function
            | [] -> [conj (List.rev acc), values]
            | t :: ts ->
              step t values
              >>= fun (t, values) ->
              add (t :: acc) values ts
          in
          add [] values ts
        | Or ts ->
          List.concat_map ts ~f:(fun t -> step t values)
        (* Now and Weak_now only differ at EOF *)
        | Now         p -> step p values
        | Weak_now    p -> step p values
        | Delay       p -> [p, values]
        | Until (p, q) as until ->
          step (q || p && Delay until) values
        | Release (p, q) as release ->
          step (q && (p || Delay release)) values
      in
      List.filter (step formula values) ~f:(Fn.non is_false)
      (* We could also do deduplication on the more global level, in query, but
         I feel this is the right balance.

         Why do we need deduplication at all? Consider
         [always (t1 || t2)]
         where both t1 and t2 evaluate to true.
      *)
      |> List.dedup ~compare

    let no_change_unless (t, values) =
      (* [false_unless t = Some l] means [l] is a minimal set of guards such that
         [t => disj l], meaning [t] is going to remain false until some of the guards
         become true. *)
      let rec false_unless = function
        | Eq (v, field) ->
          Option.map (Assignment.find values v) ~f:(fun value ->
            [Guard.Eq (field, value)])
        | And ts ->
          List.find_map ts ~f:false_unless
        | Or ts ->
          Option.all (List.map ts ~f:false_unless) |> Option.map ~f:List.concat
        | _ -> None
      in
      (* [true_unless t = Some l] means [not t => disj l], meaning 't is going to remain
         true, unless some of the guards become true' *)
      let rec true_unless = function
        | Not_eq (v, field) ->
          Option.map (Assignment.find values v) ~f:(fun value ->
            [Guard.Eq (field, value)])
        | Or ts ->
          List.find_map ts ~f:true_unless
        | And ts ->
          Option.all (List.map ts ~f:true_unless) |> Option.map ~f:List.concat
        | _ -> None
      in
      (* [no_change_unless t = Some l] means [l] is a smallest set of guards such that
         [negate (Guard.eval (disj l) x) => step f x = [f]]
      *)
      let rec no_change_unless = function
        | And ts | Or ts ->
          Option.all (List.map ts ~f:no_change_unless)
          |> Option.map ~f:List.concat
        | Until (True, t) ->
          (* [eventually t] will not reduce as long as [t] remains [false]. *)
          false_unless t
        | Release (False, t) ->
          (* [always t] will not reduce as long as [t] remains [true]. *)
          true_unless t
        | _ -> None
      in
      no_change_unless t

    include Comparable.Make (T)
  end

  module Sleeping_constraints : sig
    type t [@@deriving sexp_of]
    val create : unit -> t
    val num_guards : t -> int
    val to_list : t -> Constraint.t list
    val add : t -> Constraint.t -> [ `Added | `Not_added ]
    val wake_up : t -> State.t -> Constraint.t list
  end = struct
    type field = Field : 'a Field.t -> field [@@deriving sexp_of]

    type t =
      { mutable fields : field list
      ; sleeping : Constraint.Set.t Guard.Table.t
      } [@@deriving fields, sexp_of]

    let create () =
      { fields = []
      ; sleeping = Guard.Table.create ()
      }

    let to_list t =
      Hashtbl.data t.sleeping
      |> List.concat_map ~f:Set.to_list
      |> List.dedup ~compare:Constraint.compare

    let num_guards t =
      Hashtbl.length t.sleeping

    let add t c =
      match Constraint.no_change_unless c with
      | None -> `Not_added
      | Some guards ->
        List.iter guards ~f:(fun ((Guard.Eq (field, _)) as guard) ->
          Hashtbl.update t.sleeping guard ~f:(function
            | None -> Constraint.Set.singleton c
            | Some constraints -> Set.add constraints c);
          let has_field =
            List.exists t.fields ~f:(fun (Field field') ->
              Field.same field field')
          in
          if not has_field
          then t.fields <- (Field field) :: t.fields);
        `Added

    let remove t c =
      let guards = Option.value_exn (Constraint.no_change_unless c) in
      List.iter guards ~f:(fun guard ->
        Hashtbl.change t.sleeping guard ~f:(function
          | None -> None
          | Some constraints ->
            let constraints = Set.remove constraints c in
            if Set.is_empty constraints then None else Some constraints))

    let wake_up t state =
      let guards = List.filter_map t.fields ~f:(fun (Field field) ->
        Option.map (Field.get field state) ~f:(fun value ->
          Guard.Eq (field, value)))
      in
      let constraints =
        List.filter_map guards ~f:(fun guard ->
          let constraints = Hashtbl.find t.sleeping guard in
          constraints)
        |> List.concat_map ~f:Set.to_list
        |> List.dedup ~compare:Constraint.compare
      in
      List.iter constraints ~f:(remove t);
      constraints
  end

  (** Represents a disjunction of multiple constraints *)
  module Constraints : sig
    type formula = t
    type t [@@deriving sexp_of]
    val create : formula -> t
    val is_empty : t -> bool
    val num_active : t -> int
    val num_sleeping_guards : t -> int
    (** updates the constraints according to the new state and
        returns a list of assignments that satisfy some of the constraints *)
    val step : t -> State.t -> Assignment.t list * t
    val eof : t -> Assignment.t list
  end = struct
    type formula = t

    type t =
      { active : Constraint.t list
      ; sleeping : Sleeping_constraints.t
      } [@@deriving sexp_of]

    let create formula =
      { active = [Constraint.create formula]
      ; sleeping = Sleeping_constraints.create ()
      }

    let num_active {active; sleeping = _} =
      List.length active

    let num_sleeping_guards {active = _; sleeping} =
      Sleeping_constraints.num_guards sleeping

    let is_empty t =
      num_active t + num_sleeping_guards t = 0

    let step {active; sleeping} state =
      let active =
        Sleeping_constraints.wake_up sleeping state @ active
        |> List.concat_map ~f:(fun c ->
          Constraint.step c state)
        |> List.filter ~f:(fun c ->
          match Sleeping_constraints.add sleeping c with
          | `Added -> false
          | `Not_added -> true)
      in
      let results, active =
        List.partition_map active ~f:(fun c ->
          match Constraint.is_determined c with
          | Some result -> `Fst result
          | None -> `Snd c)
      in
      results, {sleeping; active}

    let eof {active; sleeping} =
      let constraints = Sleeping_constraints.to_list sleeping @ active in
      List.filter_map constraints ~f:Constraint.eof
  end

  module Normalize : sig
    val normalize : t -> t Or_error.t
  end = struct

    (* Heuristic: sort by effectiveness in constraining the set of solutions.

       One thing that this sorting aims to achieve is that every variable is
       bound before being used in a non-binding subformula. For instance, this
       is bad:
       (x != 10) && (x == field "v")
       But the opposite order is fine:
       (x == field "v") && (x != 10)

       The [check_bound] function below is used to check that no such bad
       occurrences are left after sorting.

       We put Or first, so it can be split by lift_or.
    *)
    let sort_conjuncts ts =
      let priority = function
        | Or _ -> 0
        | True | False -> 1
        | Eq _ -> 2
        | Not_eq _ | Predicate _ | Not_predicate _ -> 3
        | And _ -> 4
        | Delay _
        | Now _ | Weak_now _
        | Until _ | Release _  -> 5
      in
      let cmp t1 t2 =
        Int.compare (priority t1) (priority t2)
      in
      List.sort ts ~cmp

    (* This relies on the flattening behaviour of [conj] such that
       [And [And [Eq _; x]; Not_eq _]] does not occur. *)
    let rec sort_all_conjuncts t =
      match map_children t ~f:sort_all_conjuncts with
      | And ts -> And (sort_conjuncts ts)
      | t -> t

    (* With disjunctions on top we can split the formula into several simpler
       constraints which are easier to handle. For instance, we can put each of
       the constraints to sleep separately.

       This brings the formula into a kind of DNF where a disjunction never
       occurs directly underneath a conjunction or an [eventually].

       This transformation might incur an exponential blowup of the formula size.
    *)
    let rec lift_or t =
      let rec split_conj = function
        | Or ts :: tss ->
          List.cartesian_product ts (split_conj tss)
          |> List.map ~f:(fun (t, ts) -> t :: ts)
        (* The conjuncts will be sorted with Ors at the start. *)
        | ts -> [ts]
      in
      let t = map_children t ~f:lift_or in
      match t with
      | And ts ->
        let ts = sort_conjuncts ts in
        disj (List.map (split_conj ts) ~f:conj)
      | Until (t1, Or ts) ->
        disj (List.map ts ~f:(fun t2 -> Until (t1, t2)))
      | t -> t

    let check_is_path_formula t_top =
      let fail t =
        failwithf
          !"Will fail on an empty sequence: subformula %{sexp:t} of %{sexp:t}"
          t t_top ()
      in
      let rec check ~state_is_available t =
        let require_state () =
          if state_is_available then () else fail t
        in
        match t with
        | True | False -> ()
        | Until (t1, t2) | Release (t1, t2) ->
          List.iter [t1; t2] ~f:(check ~state_is_available:true)
        | Now t | Weak_now t  ->
          check t ~state_is_available:true
        | Delay t ->
          require_state ();
          check t ~state_is_available:false
        | Eq _ | Not_eq _ | Predicate _ | Not_predicate _ ->
          require_state ()
        | And ts | Or ts ->
          List.iter ts ~f:(check ~state_is_available)
      in
      check t_top ~state_is_available:false

    let check_bound t_top =
      let check_predicate ~bound t p =
        let vs = Predicate.variables p |> Variable.Id.Set.of_list in
        let unbound = Set.diff vs bound in
        if Pervasives.not (Set.is_empty unbound)
        then failwithf
               !"Variables %{sexp:Variable.Id.Set.t} are not bound\
                 in subexpression %{sexp:t} of %{sexp:t}"
               unbound t t_top ()
      in
      let rec check_conjuncts ~bound = function
        | [] -> ()
        | Eq (v, _) :: ts ->
          let bound = Set.add bound (Variable.id v) in
          check_conjuncts ~bound ts
        | t :: ts ->
          check_single ~bound t;
          check_conjuncts ~bound ts
      and check_single ~bound = function
        | And ts -> check_conjuncts ~bound ts
        | Not_eq (v, _) as t ->
          if Pervasives.not (Set.mem bound (Variable.id v))
          then
            failwithf
              !"Variable %s is not bound in subexpression %{sexp:t} of %{sexp:t}"
              (Variable.to_string v) t t_top ()
        | Predicate (_, p) as t ->
          check_predicate ~bound t p
        | Not_predicate (_, p) as t ->
          check_predicate ~bound t p
        | t ->
          iter_children t ~f:(check_single ~bound)
      in check_single t_top ~bound:Variable.Id.Set.empty

    let normalize t : t Or_error.t =
      let open Or_error.Monad_infix in
      let t = lift_or t in
      Or_error.try_with (fun () -> check_is_path_formula t)
      >>= fun () ->
      let t = sort_all_conjuncts t in
      Or_error.try_with (fun () -> check_bound t)
      >>= fun () ->
      Ok t
  end

  let validate t =
    Or_error.map (Normalize.normalize t) ~f:ignore

  let query ?debug ?(allow_nonmonotonic_times = false) ?(allow_missing_times = false) t input_pipe =
    let count = ref 0 in
    let last_time = ref None in
    let step constraints = function
      | `Eof ->
        Option.iter debug ~f:(fun debug ->
          Log.info debug "received EOF");
        Constraints.eof constraints, constraints
      | `Ok state ->
        incr count;
        let time = State.time state in
        if not allow_missing_times && Option.is_none time then
          failwithf "Missing time for state: %s \n " (State.to_string state) ();
        Option.iter (Option.both !last_time time) ~f:(fun (last_time, time) ->
          if Time_ns.(<) time last_time
          then begin
            let error =
              sprintf !"Time_ns %{sexp:Time_ns.t} in %s is smaller than previous time"
                time (State.to_string state)
            in
            Option.iter debug ~f:(fun debug -> Log.error debug "%s" error);
            if not allow_nonmonotonic_times then failwith error
          end);
        last_time := State.time state;
        let results, constraints = Constraints.step constraints state in
        Option.iter debug ~f:(fun debug ->
          Log.info debug
            !"state %d (%{sexp:Time_ns.t option}), \
              active constraints: %d, \
              sleeping guards: %d"
            !count
            time
            (Constraints.num_active constraints)
            (Constraints.num_sleeping_guards constraints);
          if Log.level debug <= `Debug
          then begin
            Log.debug debug !"received state: %s" (State.to_string state);
            Log.debug debug !"%{sexp:Constraints.t}" constraints
          end);
        results, constraints
    in
    let output t =
      Pipe.create_reader ~close_on_exception:true (fun output ->
        let rec loop constraints =
          Pipe.read input_pipe
          >>= fun input ->
          let (results, constraints) = step constraints input in
          begin
            match debug with
            | None -> return ()
            | Some debug -> Log.flushed debug
          end
          >>= fun () ->
          if Pipe.is_closed output
          then Deferred.unit
          else begin
            Pipe.transfer_in output ~from:(Queue.of_list results)
            >>= fun () ->
            if Constraints.is_empty constraints || input = `Eof
            then Deferred.unit
            else loop constraints
          end
        in
        loop (Constraints.create t)
        >>= fun () ->
        Pipe.close_read input_pipe;
        Deferred.unit)
    in
    let open Or_error.Monad_infix in
    Normalize.normalize t
    >>= fun t ->
    Option.iter debug ~f:(fun debug ->
      Log.info debug !"normalized formula:\n%{sexp:t}" t);
    Ok (output t)

  let eval ?debug ?allow_nonmonotonic_times ?allow_missing_times t input =
    Or_error.map (query ?debug ?allow_nonmonotonic_times ?allow_missing_times t input)
      ~f:(fun output ->
        Pipe.read output
        >>| fun res ->
        Pipe.close_read output;
        match res with
        | `Eof  -> false
        | `Ok _ -> true)
end

let%test_module _ = (module struct
  let test_eval' eval t states ~expect =
    Thread_safe.block_on_async_exn (fun () ->
      Or_error.ok_exn (eval t (Pipe.of_list states))
      >>| fun result ->
      [%test_result: bool] result ~expect)

  module State = struct
    type t = Time_ns.t * int option [@@deriving sexp, compare]
    let to_string t = Sexp.to_string (sexp_of_t t)
    let time (time, _) = Some time
  end

  module P = Make (State)

  module Fields = struct
    let time = P.Field.time

    let value =
      { P.Field. id = Type_equal.Id.create ~name:"value" Int.sexp_of_t
      ; get = (fun (_, value) -> value)
      ; hashable = Int.hashable
      }
  end

  let x  = P.Variable.create "x" ~sexp_of:Int.sexp_of_t
  (* Many operations that follow are defined with respect to this variable
     list. *)
  let variables = [x]

  module Assignment = struct
    include P.Assignment

    let bindings t =
      List.filter_map variables ~f:(fun v ->
        match find t v with
        | None -> None
        | Some value -> Some (v, value))

    let compare t1 t2 =
      [%compare: (int P.Variable.t * int) list]
        (bindings t1)
        (bindings t2)

    let singleton v value =
      set empty v value

    let of_list_exn bindings =
      List.fold bindings ~init:empty ~f:(fun t (key, value) ->
        add_exn t key value)

    (* Cross product: variables x values, can be restricted to only include
       the assignments that refine a given assignment. *)
    let all ?(refines = []) variables values : t list =
      let list_diff xs ys =
        List.filter xs ~f:(fun x ->
          not (List.exists ys ~f:(fun y -> P.Variable.same x y)))
      in
      let rec all vs t =
        match vs with
        | [] -> [t]
        | v :: vs ->
          List.map values ~f:(fun value -> Univ_map.add_exn t v value)
          |> List.concat_map ~f:(all vs)
      in
      let refines_variables = List.map refines ~f:fst in
      let vs = list_diff variables refines_variables in
      all vs (of_list_exn refines)
  end

  (* Direct definition of the semantics. *)
  module Naive = struct
    let eval t (xs : State.t list) (values : Assignment.t) =
      let open P in
      let fail_empty t_sub =
        failwithf
          !"Naive.eval: tried to evaluate subexpression \
            %{sexp:t} of %{sexp:t} on an empty sequence."
          t_sub t ()
      in
      let rec eval t xs =
        match t, xs with
        | True, _ ->
          true
        | False, _ ->
          false
        | Predicate (_, p), x :: _ ->
          Predicate.eval p x values
        | Not_predicate (_, p), x :: _ ->
          Pervasives.not (Predicate.eval p x values)
        | ( Predicate _ | Not_predicate _ | Eq _ | Not_eq _ | Delay _) as t, [] ->
          fail_empty t
        | Eq (v, field), x :: _ ->
          begin match Field.get field x with
          | None -> false
          | Some value ->
            let value' = Assignment.find_exn values v in
            Field.compare_values field value value' = 0
          end
        | Not_eq (v, e), xs ->
          not (eval (Eq (v, e)) xs)
        | And ts, xs ->
          List.for_all ts ~f:(fun t -> eval t xs)
        | Or ts, xs ->
          List.exists ts ~f:(fun t -> eval t xs)
        | Now         _, [] -> false
        | Weak_now    _, [] -> true
        | Now         t, xs -> eval t xs
        | Weak_now    t, xs -> eval t xs
        | Delay       t, _ :: xs -> eval t xs
        | Until (t1, t2) as t, x :: xs ->
          eval t2 (x :: xs) || (eval t1 (x :: xs) && eval t xs)
        | Until _, [] ->
          false
        | Release (t1, t2) as t, x :: xs ->
          eval t2 (x :: xs) && (eval t1 (x :: xs) || eval t xs)
        | Release _, [] ->
          true
      in
      eval t xs

    let all_solutions t variables values (xs : State.t list) =
      Assignment.all variables values
      |> List.filter ~f:(eval t xs)
  end

  let query_list t data =
    Pipe.of_list data
    |> P.query t
    |> Or_error.ok_exn
    |> Pipe.to_list

  (* Test both against the naive semantics and the manual result. *)
  let test_one ?expect ?(skip_naive = false) t variables data =
    let values = List.filter_opt (List.dedup (List.map data ~f:snd)) in
    query_list t data
    >>| fun result ->
    let result =
      List.concat_map result ~f:(fun assignment ->
        let assignment = Assignment.bindings assignment in
        Assignment.all ~refines:assignment variables values)
      |> List.dedup ~compare:Assignment.compare
      |> List.sort ~cmp:Assignment.compare
    in
    if not skip_naive
    then begin
      let naive_result =
        Naive.all_solutions t variables values data
        |> List.sort ~cmp:Assignment.compare
      in
      [%test_result: Assignment.t list] result ~expect:naive_result;
    end;
    Option.iter expect ~f:(fun expect ->
      [%test_result: Assignment.t list] result ~expect)

  (* (1 | ... | 5){1, 5}. *)
  let all_permutations =
    let rec generate n acc xss =
      let time =
        Time.of_span_since_epoch (Time.Span.of_sec (Float.of_int n))
        |> Time_ns.of_time
      in
      match xss with
      | [] -> [List.rev acc]
      | xs :: xss ->
        let products =
          List.concat_map xs ~f:(fun x ->
            generate (n + 1) ((time, x) :: acc) xss)
        in
        List.rev acc :: products
    in
    generate 0 [] (List.init 5 ~f:(fun _ ->
      [None; Some 1; Some 2; Some 3; Some 4; Some 5]))
    |> List.filter ~f:(Fn.non List.is_empty)

  let test_permutations t =
    Deferred.List.iter all_permutations ~f:(test_one t variables)

  let test_one ?skip_naive t xs ~expect =
    let expect = List.map expect ~f:(fun value ->
      Assignment.singleton x value)
    in
    test_one ?skip_naive t xs ~expect

  let test_eval = test_eval' P.eval

  let test ?(skip_naive = false) t data ~expect =
    Thread_safe.block_on_async_exn (fun () ->
      test_one ~skip_naive t variables data ~expect
      >>= fun () ->
      if skip_naive then Deferred.unit else test_permutations t)

  let invalid t =
    match P.validate t with
    | Error _ -> ()
    | Ok () ->
      failwithf !"%{sexp:P.t} expected to fail validation, but succeeded." t ()

  let value = Fields.value
  let value_is v = P.field_predicate value (fun v' -> v = v')

  open P
  open P.O

  let add_times values =
    let times n = List.init n ~f:(fun i ->
      Time.of_span_since_epoch (Time.Span.of_sec (Float.of_int i))
      |> Time_ns.of_time)
    in
    List.zip_exn (times (List.length values)) values

  let states =
    add_times [Some 2; None; Some 3; Some 4; Some 2; Some 1; Some 5]

  let repeating_states =
    add_times [Some 1; Some 1; Some 1]

  let repeating_states_with_none =
    add_times [Some 1; None; Some 1; Some 1]

  let%test_unit _ =
    test_eval (eventually (value_is 10)) states ~expect:false

  let%test_unit _ =
    test_eval (always (not (value_is 10))) states ~expect:true

  let%test_unit _ =
    test_eval (not (eventually (value_is 10))) states ~expect:true

  let%test_unit _ =
    test_eval (eventually ((x == value) && eventually (not (x == value))))
      repeating_states ~expect:false

  let%test_unit _ =
    test_eval (eventually ((x == value)
                           && eventually (not (x == value))))
      repeating_states_with_none ~expect:true

  let%test_unit _ =
    test_eval (eventually ((x == value)
                           && eventually (has value && not (x == value))))
      repeating_states_with_none ~expect:false

  let%test_unit _ =
    test_eval (eventually (predicate (Expression.return true)
                           || predicate (Expression.return true)))
      repeating_states_with_none ~expect:true

  let%test_unit _ =
    invalid (x == value)

  let%test_unit _ =
    invalid (eventually (not (x == value)))

  let%test_unit _ =
    test (eventually (not (x == value)
                      && x == value))
      states ~expect:[]

  let%test_unit _ =
    test (eventually (x == value))
      states ~expect:[1; 2; 3; 4; 5]

  let%test_unit _ =
    test (eventually ((x == value)
                      && eventually (not (x == value))))
      states ~expect:[1; 2; 3; 4]

  let%test_unit _ =
    invalid (eventually ((x == value))
             && eventually (not (x == value)))

  let gt var =
    predicate Expression.(map2 (variable var) (field value) ~f:(>))

  let sec x =
    sec x |> Time_ns.Span.of_span

  (* For tests involving an additional time variable we are going to skip naive
     evaluation. *)
  let followed_by_smaller_within span =
    let t = Variable.create "time" in
    eventually (x == value
                && t == Fields.time
                && eventually (before_var t ~add:span
                               && gt x))

  let not_followed_by_smaller_within span =
    let t = Variable.create "time" in
    eventually (x == value
                && t == Fields.time
                && not (eventually (before_var t ~add:span
                                    && gt x)))

  let%test_unit _ =
    test ~skip_naive:true
      (followed_by_smaller_within (sec 0.1))
      states ~expect:[]

  let%test_unit _ =
    test ~skip_naive:true
      (not_followed_by_smaller_within (sec 0.1))
      states ~expect:[1; 2; 3; 4; 5]

  let%test_unit _ =
    test ~skip_naive:true
      (followed_by_smaller_within (sec 1.0))
      states ~expect:[2; 4]

  let%test_unit _ =
    test ~skip_naive:true
      (not_followed_by_smaller_within (sec 1.0))
      (* 2 occurs both here and in the positive case. This is correct. *)
      states ~expect:[1; 2; 3; 5]

  let%test_unit _ =
    test ~skip_naive:true
      (followed_by_smaller_within (sec 10.0))
      states ~expect:[2; 3; 4]

  let%test_unit _ =
    test ~skip_naive:true
      (not_followed_by_smaller_within (sec 10.0))
      (* Absence of 2 here is correct. *)
      states ~expect:[1; 5]

  let%test_unit _ =
    let next_is_two =
          eventually (x == value
                && next (now (value_is 2)))
    in
    test next_is_two states ~expect:[4]

  let%test_unit _ =
    let next_is_not_two =
          eventually (x == value
                && not (next (now (value_is 2))))
    in
    test next_is_not_two states ~expect:[1; 2; 3; 5]

  let%test_unit _ =
    let occurs_twice =
      eventually (x == value
                  && (next (eventually (x == value))))
    in
    test occurs_twice states ~expect:[2]

  let%test_unit _ =
    let property = until (const true) (const false) in
    test property states ~expect:[]

  let%test_unit _ =
    let property = until (const false) (const true) in
    test property states ~expect:[1; 2; 3; 4; 5]

  let%test_unit _ =
    let property = release (const false) (const true) in
    test property states ~expect:[1; 2; 3; 4; 5]

  let%test_unit _ =
    let property = release (const true) (const false) in
    test property states ~expect:[]

  let%test_unit _ =
    let property =
      until
        (eventually (const true))
        (const false)
    in
    test property states ~expect:[]

  module No_time_states = struct
    type t = int [@@deriving sexp, compare]
    let to_string t = Sexp.to_string (sexp_of_t t)
    let time _ = None
  end

  module M = Make (No_time_states)

  let states = [1; 2; 0; 10; 1; 15]

  let test_eval = test_eval' (M.eval ~allow_missing_times:true)

  let time_of_int x =
    Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec x)

  let%test_unit _ =
    test_eval
      (M.eventually (M.after (time_of_int 0)))
      states
      ~expect:false

  let%test_unit _ =
    test_eval
      (M.eventually (M.before (time_of_int 100)))
      states
      ~expect:false

  module States_with_times = struct
    type t = (Time_ns.t * int) [@@deriving sexp, compare]
    let to_string t = Sexp.to_string (sexp_of_t t)
    let time (time, _) = Some time
  end

  module Q = Make (States_with_times)

  let test_eval = test_eval' Q.eval

  let states =
    List.mapi
      [1; 2; 0; 10; 1; 15]
      ~f:(fun i element -> (time_of_int i, element))
  ;;

  let%test_unit _ =
    test_eval
      (Q.eventually (Q.after (time_of_int 3)))
      states
      ~expect:true

  let%test_unit _ =
    test_eval
      (Q.eventually (Q.before (time_of_int 3)))
      states
      ~expect:true
end)
