open Core_kernel

open! Int.Replace_polymorphic_compare

(** Add a unique identity to things which is comparable. *)
module Identified : sig
  module Id : Unique_id.Id

  type +'a t = private Identified of Id.t * 'a

  val create  : 'a -> 'a t
  val extract : 'a t -> 'a

  val compare : _ t -> _ t -> int

  (** The id comparison corresponds with the regular comparison. *)
  val id : _ t -> Id.t
end = struct
  module Id = Unique_id.Int()

  type +'a t = Identified of Id.t * 'a

  let create x = Identified (Id.create (), x)
  let extract (Identified (_, x)) = x
  let id (Identified (id, _)) = id

  let compare (Identified (l, _)) (Identified (r, _)) = Id.compare l r
end

module Block = struct
  type t
end

module Id : sig
  type 'a t

  val create_string  : prefix:string -> string t
  val create_bool    : prefix:string -> bool t
  val create_block   : prefix:string -> Block.t t

  val extend_prefix  : prefix:string -> string

  val to_string : _ t -> string

  module Pack : sig
    include Comparable.S
    val to_string : t -> string
  end

  (** This is stable, i.e. [phys_equal (pack x) (pack x)]. *)
  val pack : _ t -> Pack.t
end = struct
  type 'a t = string

  module I = Unique_id.Int()
  module Pack = String

  let to_string s = s
  let create_string  ~prefix = sprintf !"%s_f%{I}" prefix (I.create ())
  let create_bool    ~prefix = sprintf !"%s_f%{I}" prefix (I.create ())
  let create_block   ~prefix = sprintf !"%s_b%{I}" prefix (I.create ())
  let extend_prefix  ~prefix = sprintf !"%s_p%{I}" prefix (I.create ())

  let pack s = s
end

module Variant_id = struct
  type 'a t = {
    options  : 'a array;
    equal    : 'a -> 'a -> bool;
    field_id : string Id.t;
  }
end

module Field = struct

  type 'a t =
    | Bool    : bool   t
    | String  : string t

  type pack = Pack : 'a t * 'a option -> pack

  let compare_pack l r = match l, r with
    | Pack (Bool, _), Pack (String, _) -> -1
    | Pack (String, _), Pack (Bool, _) -> 1
    | Pack (Bool, l), Pack (Bool, r) -> Option.compare Bool.compare l r
    | Pack (String, l), Pack (String, r) -> Option.compare String.compare l r
  ;;

  let generate_id (type a) (t : a t) ~prefix : a Id.t = match t with
    | Bool    -> Id.create_bool    ~prefix
    | String  -> Id.create_string  ~prefix
  ;;
end

module Form_error = struct
  type t = Pack : 'a Id.t * Error.t -> t

  let create reason ~id = Pack (id, reason)

  let of_string string ~id = create ~id (Error.createf !"%{Id}: %s" id string)
end

type ('g, 's, 'ids) params = 'g * 's * 'ids

module List_id = struct
  type 's t = List_id : ('g, 's, 'ids) params Type_equal.Id.t -> 's t
end

module Description = struct

  type ('g, 's, 'ids) t =
    | Not_editable : 'a -> ('a, 'a, unit) t
    | Field : 'a Field.t -> ('a, 'a, 'a Id.t) t
    | Pair  : ('a, 's, 'a_ids) t * ('b, 's, 'b_ids) t ->
      ('a * 'b, 's, 'a_ids * 'b_ids) t
    | Map : ('a, 's, 'ids) t * ('a -> 'b) -> ('b, 's, 'ids) t
    | Contra_map : ('g, 'b, 'ids) t * ('a -> 'b) -> ('g, 'a, 'ids) t
    | Map_ids : ('g, 's, 'a_ids) t * ('a_ids -> 'b_ids) -> ('g, 's, 'b_ids) t
    | Conv : {
        inner : ('a, 's, 'ids) t;
        to_outer : ('a -> 'ids -> block_id:Block.t Id.t -> ('b, Form_error.t list) Result.t);
      } -> ('b, 's, Block.t Id.t * 'ids) t
    | List : ('g, 's, 'ids) t -> ('g list, 's list, 'ids list * 's List_id.t) t

  let not_editable ~default = Not_editable default
  let string = Field String
  let bool   = Field Bool

  let both a b = Pair (a, b)
  let map x ~f = Map (x, f)
  let contra_map x ~f = Contra_map (x, f)
  let map_ids x ~f = Map_ids (x, f)

  let conv inner ~f:to_outer = Conv { inner; to_outer }
  let conv_without_block inner ~f:to_outer =
    conv inner ~f:(fun a ids ~block_id:_ -> to_outer a ids)
    |> map_ids ~f:(fun (_block_id, ids) -> ids)
  ;;
  let list el_desc = List el_desc

  let sexp ~of_sexp ~sexp_of =
    conv_without_block string
      ~f:(fun string string_id ->
        match of_sexp (Sexp.of_string string) with
        | x -> Ok x
        | exception exn ->
          Error [Form_error.create ~id:string_id (Error.of_exn exn)])
    |> contra_map ~f:(fun a -> Sexp.to_string (sexp_of a))
  ;;

  let rec has_duplicated_value values equal =
    match values with
    | [] | [_] -> false
    | x :: xs  -> List.mem xs x ~equal || has_duplicated_value xs equal
  ;;

  let valid_variant_options_exn list equal =
    if List.is_empty list
    then (failwith "Variant options cannot be an empty list.")
    else if has_duplicated_value list equal
    then (failwith "Variant options must have unique values.")
    else list
  ;;

  let variant options ~equal =
    let options = Array.of_list (valid_variant_options_exn options equal) in
    let variant_from_string =
      map string ~f:(fun s_idx ->
        let idx = Int.of_string s_idx in
        if idx >= 0 && idx < Array.length options
        then options.(idx)
        else (failwith "Selected variant option does not belong to the options list"))
    in
    let variant_to_from_string =
      contra_map variant_from_string ~f:(fun x ->
        let index_opt =
          Array.find_mapi options ~f:(fun i opt ->
            Option.some_if (equal x opt) i)
        in
        match index_opt  with
        | Some idx -> Int.to_string idx
        | None -> failwith "Initial variant value does not belong to the options list")
    in
    map_ids variant_to_from_string
      ~f:(fun field_id -> { Variant_id.options; equal; field_id; })
  ;;

  module Let_syntax = struct
    module Let_syntax = struct
      let map = map
      let both = both

      module Open_on_rhs = struct
        let (<^) x f = contra_map x ~f
        let (^<) f x = map x ~f

        let string = string
        let bool = bool
        let list = list
        let variant = variant
      end
    end
  end

  (* Unfortunately, everything in here is a duplicate
     of the [Record_builder] library. But the types are not compatible
     due to the [_ Id.t] generation.

     Maybe a future [Record_builder] can include general enough types that
     they can be unified.
  *)
  module Of_record = struct
    module Hlist = Record_builder.Hlist

    module Hlist_T = struct
      type ('b, 'a, 'ids) descr = ('b, 'a, 'ids) t

      type ('b, 'a, 'ids) t =
        | Nil : (unit, 'a, unit) t
        | Cons : ('b, 'a, 'ids) descr -> ('b, 'a, 'ids) t

      let nil = Nil

      let cons (type x xs i id ids)
            (x : (x, i, id) descr) (xs : (xs, i, ids) t)
        : (x * xs, i, id * ids) t
        = match xs with
        | Nil -> Cons (map_ids (map x ~f:(fun x -> x, ())) ~f:(fun ids -> ids, ()))
        | Cons xs -> Cons (both x xs)
      ;;

      let unpack ((Cons xs) : (_ Hlist.nonempty, _, _ Hlist.nonempty) t) = xs
    end

    module Make_creator_types = struct
      type ('tail, 'tail_ids, 'all_fields, 'all_ids, 'record) accum =
        (('tail, 'record, 'tail_ids) Hlist_T.t -> ('all_fields, 'record, 'all_ids) Hlist_T.t)
        * ('all_fields, 'tail) Hlist.Suffix_index.t

      type ('field, 'head, 'head_ids, 'tail, 'tail_ids, 'all_fields, 'all_ids, 'record) fold_step =
        ('head, 'head_ids, 'all_fields, 'all_ids, 'record) accum
        -> ('all_fields -> 'field) * ('tail, 'tail_ids, 'all_fields, 'all_ids, 'record) accum

      type ('field, 'field_ids, 'tail, 'tail_ids, 'all_fields, 'all_ids, 'record) handle_one_field =
        ( 'field
        , ('field, 'tail) Hlist.cons, ('field_ids, 'tail_ids) Hlist.cons
        , 'tail, 'tail_ids
        , 'all_fields Hlist.nonempty, 'all_ids Hlist.nonempty
        , 'record
        ) fold_step

      type ('all_fields, 'all_ids, 'record) handle_all_fields =
        ( 'record
        , 'all_fields Hlist.nonempty, 'all_ids Hlist.nonempty
        , Hlist.nil, Hlist.nil
        , 'all_fields Hlist.nonempty, 'all_ids Hlist.nonempty
        , 'record) fold_step
    end

    let field form field (build_hlist, suffix) =
      let build_hlist =
        Fn.compose build_hlist (Hlist_T.cons (contra_map ~f:(Core_kernel.Field.get field) form))
      and get_field =
        let index = Hlist.Element_index.(within ~suffix first_element) in
        fun hlist -> Hlist.nth hlist index
      and suffix = Hlist.Suffix_index.tail_of suffix
      in
      get_field, (build_hlist, suffix)
    ;;

    let build_for_record folding =
      let from_values, (build_up, _) =
        folding (Fn.id, Hlist.Suffix_index.whole_list)
      in
      map (Hlist_T.unpack (build_up Hlist_T.nil)) ~f:from_values
    ;;
  end

  type ('a, 'ids) closed = ('a, 'a, 'ids) t
end

type ('a, 'ids) t = {
  description : ('a, 'ids) Description.closed;
  witness     : (('a, 'ids) Description.closed) Type_equal.Id.t;
}

type ('a, 'ids) form = ('a, 'ids) t

let create ~name description =
  {
    description;
    witness = Type_equal.Id.create ~name sexp_of_opaque;
  }

let to_description { description; witness = _ } = description

module State = struct
  open Js_of_ocaml

  module State = struct
    type ('g, 's, 'ids) t =
      | Not_editable : 'a -> ('a, 'a, unit) t
      | Field : 'a Field.t * 'a option * 'a Id.t -> ('a, 'a, 'a Id.t) t
      | Pair  : ('a, 's, 'a_ids) t * ('b, 's, 'b_ids) t -> ('a * 'b, 's, 'a_ids * 'b_ids) t
      | Map : ('a, 's, 'ids) t * ('a -> 'b) -> ('b, 's, 'ids) t
      | Contra_map : ('g, 'b, 'ids) t * ('a -> 'b) -> ('g, 'a, 'ids) t
      | Map_ids : ('g, 's, 'a_ids) t * ('a_ids -> 'b_ids) -> ('g, 's, 'b_ids) t
      | Conv : {
          inner    : ('a, 's, 'ids) t;
          to_outer : ('a -> 'ids -> block_id:Block.t Id.t -> ('b, Form_error.t list) Result.t);
          block_id : Block.t Id.t
        } -> ('b, 's, Block.t Id.t * 'ids) t
      | List : ('g, 's, 'ids) params Type_equal.Id.t
        -> ('g list, 's list, 'ids list * 's List_id.t) t

    module Of_list = struct
      type ('g, 's, 'ids) state = ('g, 's, 'ids) t

      type t = Of_list : {
        witness : ('g, 's, 'ids) params Type_equal.Id.t;
        el_desc : ('g, 's, 'ids) Description.t Identified.t;
        list : ('g, 's, 'ids) state Identified.t list;
        prefix : string;
      } -> t

      let compare =
        Comparable.lexicographic
          [ Comparable.lift [%compare: Type_equal.Id.Uid.t]
              ~f:(fun (Of_list t) -> Type_equal.Id.uid t.witness)
          ; Comparable.lift [%compare: string]
              ~f:(fun (Of_list t) -> t.prefix)
          ; (fun (Of_list l) (Of_list r) -> Identified.compare l.el_desc r.el_desc)
          ; (fun (Of_list l) (Of_list r) ->
               let Type_equal.T = Type_equal.Id.same_witness_exn l.witness r.witness in
               List.compare Identified.compare l.list r.list)
          ]
      ;;
    end
  end

  open State

  type ('a, 'ids) closed = ('a, 'a, 'ids) State.t

  module List_states = struct
    type t = State.Of_list.t Type_equal.Id.Uid.Map.t
    [@@deriving compare]
  end

  (** Used for generating unique prefixes. *)
  module Form_id = Unique_id.Int()

  type t = Pack : {
    witness  : (('a, 'ids) Description.closed) Type_equal.Id.t;
    state    : ('a, 'ids) closed Identified.t;
    fields   : Field.pack Id.Pack.Map.t;
    errors   : Error.t Id.Pack.Map.t;
    uid_set  : Id.Pack.Set.t;
    list_map : List_states.t;
  } -> t

  let compare =
    Comparable.lexicographic
      [ Comparable.lift [%compare: Type_equal.Id.Uid.t]
          ~f:(fun (Pack t) -> Type_equal.Id.uid t.witness)
      ; Comparable.lift [%compare: Identified.Id.t]
          ~f:(fun (Pack t) -> Identified.id t.state)
      ; Comparable.lift [%compare: Field.pack Id.Pack.Map.t]
          ~f:(fun (Pack t) -> t.fields)
      ; Comparable.lift [%compare: Error.t Id.Pack.Map.t]
          ~f:(fun (Pack t) -> t.errors)
      ; Comparable.lift [%compare: Id.Pack.Set.t]
          ~f:(fun (Pack t) -> t.uid_set)
      ; Comparable.lift [%compare: List_states.t]
          ~f:(fun (Pack t) -> t.list_map)
      ]
  ;;

  let merge_disjoint_maps_exn l r =
    Map.merge l r ~f:(fun ~key data -> match data with
      | `Left x | `Right x -> Some x
      | `Both _ -> failwithf !"merge_disjoint_maps: Key in both maps: %{Sexp}"
                     ((Map.comparator l).Comparator.sexp_of_t key) ())
  ;;

  let rec fields : type g s ids.
    (g, s, ids) State.t -> List_states.t -> Field.pack Id.Pack.Map.t =
    fun t list_map ->
      match t with
      | Not_editable _ -> Id.Pack.Map.empty
      | Field (field, init, field_id) ->
        let id_pack = Id.pack field_id in
        Id.Pack.Map.singleton id_pack (Field.Pack (field, init))
      | Pair (a, b) ->
        merge_disjoint_maps_exn (fields a list_map) (fields b list_map)
      | Map (inner, _) -> fields inner list_map
      | Contra_map (inner, _) -> fields inner list_map
      | Map_ids (inner, _) -> fields inner list_map
      | Conv { inner; to_outer = _; block_id = _; } -> fields inner list_map
      | List witness ->
        let (Of_list.Of_list list_state) = Map.find_exn list_map (Type_equal.Id.uid witness) in
        List.map list_state.list ~f:(fun (Identified (_, t)) -> fields t list_map)
        |> List.reduce_balanced ~f:merge_disjoint_maps_exn
        |> Option.value ~default:Id.Pack.Map.empty
  ;;

  let rec uid_set : type g s ids.
    (g, s, ids) State.t -> List_states.t -> Id.Pack.Set.t =
    fun t list_map ->
      match t with
      | Not_editable _ -> Id.Pack.Set.empty
      | Field (_, _, field_id) -> Id.Pack.Set.singleton (Id.pack field_id)
      | Pair (a, b) -> Id.Pack.Set.union (uid_set a list_map) (uid_set b list_map)
      | Map (inner, _) -> uid_set inner list_map
      | Contra_map (inner, _) -> uid_set inner list_map
      | Map_ids (inner, _) -> uid_set inner list_map
      | Conv { inner; to_outer = _; block_id; } ->
        Id.Pack.Set.add (uid_set inner list_map) (Id.pack block_id)
      | List witness ->
        let (Of_list.Of_list list_state) = Map.find_exn list_map (Type_equal.Id.uid witness) in
        List.map list_state.list ~f:(fun (Identified (_, t)) -> uid_set t list_map)
        |> Id.Pack.Set.union_list
  ;;

  let rec create' : type g s ids.
    init:s option ->
    (g, s, ids) Description.t -> prefix:string ->
    (g, s, ids) State.t * List_states.t =
    fun ~init t ~prefix ->
      match t with
      | Description.Not_editable default ->
        Not_editable (Option.value ~default init), Type_equal.Id.Uid.Map.empty
      | Description.Field field ->
        let id = Field.generate_id field ~prefix in
        Field (field, init, id), Type_equal.Id.Uid.Map.empty
      | Description.Pair (a, b) ->
        let a, list_states_a = create' a ~init ~prefix in
        let b, list_states_b = create' b ~init ~prefix in
        Pair (a, b), merge_disjoint_maps_exn list_states_a list_states_b
      | Description.Map (inner, f) ->
        let inner, list_map = create' inner ~init ~prefix in
        Map (inner, f), list_map
      | Description.Contra_map (inner, f) ->
        let inner, list_states = create' inner ~init:(Option.map init ~f) ~prefix in
        Contra_map (inner, f), list_states
      | Description.Map_ids (inner, f) ->
        let inner, list_states = create' inner ~init ~prefix in
        Map_ids (inner, f), list_states
      | Description.Conv { inner; to_outer } ->
        let block_id = Id.create_block ~prefix in
        let inner, list_states = create' inner ~init ~prefix:(Id.to_string block_id) in
        Conv { inner; to_outer; block_id }, list_states
      | Description.List el_desc ->
        let prefix = Id.extend_prefix ~prefix in
        let init = Option.value init ~default:[] in
        let forms, all_list_states =
          List.unzip (List.map init ~f:(fun init ->
            let state, list_states =
              create' el_desc ~init:(Some init) ~prefix
            in
            Identified.create state, list_states))
        in
        let list_states =
          Option.value ~default:Type_equal.Id.Uid.Map.empty (
            List.reduce_balanced ~f:merge_disjoint_maps_exn all_list_states)
        in
        let witness = Type_equal.Id.create ~name:prefix sexp_of_opaque in
        let el_desc = Identified.create el_desc in
        let list_desc = Of_list.Of_list { witness; el_desc; list = forms; prefix; }  in
        List witness, Map.set list_states ~key:(Type_equal.Id.uid witness) ~data:list_desc
  ;;

  let create ?init {description; witness} =
    let prefix = sprintf !"form%{Form_id}" (Form_id.create ()) in
    let state, list_map = create' description ~init ~prefix in
    let uid_set = uid_set state list_map in
    let fields = fields state list_map in
    Pack { witness; state = Identified.create state; fields; uid_set; errors = Id.Pack.Map.empty; list_map }
  ;;

  (* Returns the keys in an arbitrary order. *)
  let rec collect_list_keys_in_use : type g s ids.
    (g, s, ids) State.t -> list_map:List_states.t
    -> tail:Type_equal.Id.Uid.t list -> Type_equal.Id.Uid.t list =
    fun t ~list_map ~tail ->
      match t with
      | Not_editable _ -> tail
      | Field _ -> tail
      | Pair (a, b) ->
        collect_list_keys_in_use a ~list_map ~tail:(collect_list_keys_in_use b ~list_map ~tail)
      | Map (inner, _) -> collect_list_keys_in_use inner ~list_map ~tail
      | Contra_map (inner, _) -> collect_list_keys_in_use inner ~list_map ~tail
      | Map_ids (inner, _) -> collect_list_keys_in_use inner ~list_map ~tail
      | Conv { inner; to_outer = _; block_id = _; } ->
        collect_list_keys_in_use inner ~list_map ~tail
      | List witness ->
        let (Of_list.Of_list list_state) = Map.find_exn list_map (Type_equal.Id.uid witness) in
        List.fold list_state.list ~init:(Type_equal.Id.uid witness :: tail)
          ~f:(fun tail inner_state ->
            collect_list_keys_in_use (Identified.extract inner_state) ~list_map ~tail)
  ;;

  let require_witness_matches_exn t_witness ~form =
    match Type_equal.Id.same_witness form.witness t_witness with
    | Some x -> x
    | None ->
      failwithf "state not matching description: %s <> %s"
        (Type_equal.Id.name form.witness)
        (Type_equal.Id.name t_witness)
        ()
  ;;

  let rec field_ids' : type g s ids. (g, s, ids) State.t -> List_states.t -> ids =
    fun t list_map ->
      match t with
      | Not_editable _ -> ()
      | Field (_, _, field_id) -> field_id
      | Pair (a, b) -> field_ids' a list_map, field_ids' b list_map
      | Map (inner, _) -> field_ids' inner list_map
      | Contra_map (inner, _) -> field_ids' inner list_map
      | Map_ids (inner, f) -> f (field_ids' inner list_map)
      | Conv { inner; to_outer = _; block_id; } -> block_id, field_ids' inner list_map
      | List witness ->
        let (Of_list.Of_list list_state) = Map.find_exn list_map (Type_equal.Id.uid witness) in
        let Type_equal.T = Type_equal.Id.same_witness_exn witness list_state.witness in
        let inner_ids =
          List.map list_state.list ~f:(fun t ->
            field_ids' (Identified.extract t) list_map)
        in
        inner_ids, List_id.List_id witness
  ;;

  let field_ids : type a ids. t -> (a, ids) form -> ids =
    fun (Pack t) form ->
      let Type_equal.T = require_witness_matches_exn ~form t.witness in
      field_ids' (Identified.extract t.state) t.list_map

  let getElementById id = Dom_html.document##getElementById (Js.string id)

  let get_with_tagged_element id ~f =
    match Dom_html.opt_tagged (getElementById (Id.to_string id)) with
    | None     -> Error [Form_error.of_string "element not found" ~id]
    | Some elt -> Result.map_error (f elt) ~f:(fun msg -> [Form_error.of_string msg ~id])
  ;;

  let string_value tagged_element =
    match tagged_element with
    | Dom_html.Input el    -> Ok (Js.to_string el##.value)
    | Dom_html.Textarea el -> Ok (Js.to_string el##.value)
    | Dom_html.Select el   -> Ok (Js.to_string el##.value)
    | _ -> Error "expected an input, textarea or select element"
  ;;

  let checked_value tagged_element =
    match tagged_element with
    | Dom_html.Input el -> Ok (if Js.to_bool el##.checked then (Some el##.value) else None)
    | _ -> Error "expected input element"
  ;;

  let get_checked_radio_button id =
    let radio_list =
      Dom_html.document##getElementsByName (Js.string (Id.to_string id))
      |> Dom.list_of_nodeList
    in
    if List.is_empty radio_list
    then (Error [Form_error.of_string ~id "no radio buttons found"])
    else
      (let selected_idxs =
         List.filter_map radio_list
           ~f:(fun el ->
             match checked_value (Dom_html.tagged el) with
             | Ok (Some v)       -> Some (Js.to_string v)
             | Ok None | Error _ -> None)
       in
       match selected_idxs with
       | [idx_str] -> Ok idx_str
       | []        -> Error [Form_error.of_string ~id "not a single radio button is checked"]
       | _         -> Error [Form_error.of_string ~id "multiple radio buttons are checked"]
      )
  ;;

  let get_field : type a . a Field.t -> a Id.t -> (a, Form_error.t list) Result.t =
    fun field id ->
      match field with
      | Field.Bool ->
        Result.map ~f:Option.is_some (get_with_tagged_element id ~f:checked_value)
      | Field.String  ->
        let single_element_result = get_with_tagged_element id ~f:string_value in
        let radio_buttons_result = get_checked_radio_button id in
        match single_element_result, radio_buttons_result with
        | Ok x, Error _ | Error _, Ok x -> Ok x
        | Error a, Error b -> Error (a @ b)
        | Ok _select, Ok _radio -> Error [Form_error.of_string ~id "found multiple inputs"]
  ;;

  let rec read_value' :
    type g s ids. (g, s, ids) State.t -> List_states.t
    -> (g, Form_error.t list) Result.t =
    fun t list_map ->
      match t with
      | Not_editable a -> Ok a
      | Field (field, _, field_id) -> get_field field field_id
      | Pair (a, b) ->
        let val_a = read_value' a list_map in
        let val_b = read_value' b list_map in
        Result.combine val_a val_b ~ok:Tuple2.create ~err:List.append
      | Map (inner, f) -> Result.map (read_value' inner list_map) ~f
      | Contra_map (inner, _) -> read_value' inner list_map
      | Map_ids (inner, _) -> read_value' inner list_map
      | Conv { inner; to_outer; block_id; } ->
        Result.bind (read_value' inner list_map)
          ~f:(fun x -> to_outer x (field_ids' inner list_map) ~block_id)
      | List witness ->
        let (Of_list.Of_list list_state) = Map.find_exn list_map (Type_equal.Id.uid witness) in
        let Type_equal.T = Type_equal.Id.same_witness_exn witness list_state.witness in
        let ok_list, err_list =
          List.partition_map list_state.list
            ~f:(fun x -> Result.ok_fst (read_value' (Identified.extract x) list_map))
        in
        match err_list with
        | [] -> Ok ok_list
        | errors -> Error (List.concat errors)
  ;;

  let check_uid_exn set id witness =
    if Id.Pack.Set.mem set id
    then ()
    else (failwithf "Id %S does not belong to the form %S"
            (Id.Pack.to_string id)
            (Type_equal.Id.name witness) ())
  ;;

  let read_value : type a ids. t -> (a, ids) form -> a option * t =
    fun (Pack t) form ->
      let Type_equal.T = require_witness_matches_exn ~form t.witness in
      let value_option, errors =
        match read_value' (Identified.extract t.state) t.list_map with
        | Error errors -> None      , errors
        | Ok value     -> Some value, []
      in
      let uid_error_alist =
        List.map errors
          ~f:(fun (Form_error.Pack (field_id, err)) ->
            let pack = Id.pack field_id in
            check_uid_exn t.uid_set pack form.witness;
            pack, err)
      in
      let errors =
        Id.Pack.Map.of_alist_multi uid_error_alist
        |> Id.Pack.Map.map ~f:(function
          | []  -> assert false
          | [e] -> e
          | l   -> Error.of_list l)
      in
      value_option, Pack { t with errors }
  ;;

  let error (Pack t) id =
    let pack = Id.pack id in
    check_uid_exn t.uid_set pack t.witness;
    Id.Pack.Map.find t.errors pack
  ;;

  let errors (Pack t) = Map.data t.errors

end

(* Do not forget to always add the optional key,
   otherwise Incr_dom diff doesn't work properly. *)
module Input = struct

  open Incr_dom.Vdom

  let field_pack (State.Pack state) id = Id.Pack.Map.find_exn state.fields (Id.pack id)

  let string_field state id : string option =
    let Field.Pack (field, opt) = field_pack state id in
    match field with
    | Field.String  -> opt
    | Field.Bool    -> failwith "expected string field"
  ;;

  let text state id attr =
    let value = Option.value ~default:"" (string_field state id) in
    let id = Id.to_string id in
    Node.input ~key:id
      ([Attr.type_ "text"; Attr.id id; Attr.value value] @ attr) []
  ;;

  let bool_field state id : bool option =
    let Field.Pack (field, opt) = field_pack state id in
    match field with
    | Field.Bool    -> opt
    | Field.String  -> failwith "expected bool field"
  ;;

  let checkbox state id attr =
    let value = bool_field state id in
    let id = Id.to_string id in
    let attr = (Attr.type_ "checkbox") :: (Attr.id id) :: attr in
    let attr =
      match value with
      | Some true         ->  attr @ [Attr.checked]
      | None | Some false ->  attr
    in
    Node.input ~key:id attr []
  ;;

  let textarea state id attr =
    let text = Option.value ~default:"" (string_field state id) in
    let id = Id.to_string id in
    Node.textarea ~key:id
      ((Attr.id id) :: attr) [Node.text text]
  ;;

  let radio_button state {Variant_id.options; field_id; _} attr =
    let selected_idx =
      Option.value_map ~default:0 ~f:Int.of_string (string_field state field_id)
    in
    let id = Id.to_string field_id in
    List.mapi (Array.to_list options) ~f:(fun idx opt ->
      let selected_attr = if selected_idx = idx then [Attr.checked] else [] in
      let radio_attr =
        [Attr.type_ "radio"; Attr.create "name" id; Attr.value (Int.to_string idx)]
      in
      opt, Node.input ~key:(sprintf "%s-%d" id idx) (selected_attr @ radio_attr @ attr) []
    )
  ;;

  let dropdown state {Variant_id.options; equal = _; field_id} attr ?compare to_string =
    let selected_idx =
      Option.value_map ~default:0 ~f:Int.of_string (string_field state field_id)
    in
    let id = Id.to_string field_id in
    let select_options =
      List.mapi (Array.to_list options) ~f:(fun idx opt ->
        let selected_attr =
          if selected_idx = idx
          then [Attr.create "selected" "selected"]
          else []
        in
        let option_attr = selected_attr @ [Attr.value (Int.to_string idx)] in
        opt, Node.option option_attr [Node.text (to_string opt)]
      )
    in
    let sorted_options =
      match compare with
      | None -> select_options
      | Some cmp -> List.sort select_options ~compare:(fun (l, _) (r, _) -> cmp l r)
    in
    Node.select ~key:id (Attr.id id :: attr) (List.map sorted_options ~f:snd)
  ;;

end

module List = struct
  open State.State
  open State

  (* This is similar to [Either.t] but makes the code much easier to read. *)
  type ('a,'b) old_or_new =
    | Old of 'a
    | New of 'b

  type 's list_modifier = {
    transform : 'elt. 'elt list -> create_new_form:(?init:'s -> unit -> 'elt) -> 'elt list
  }

  let check_uniqness list =
    let old_idx_set = Int.Hash_set.create () in
    let all_old_unique =
      List.for_all list ~f:(function
        | Old (_, idx) ->
          begin
            match Hash_set.strict_add old_idx_set idx with
            | Ok ()   -> true
            | Error _ -> false
          end
        | New _ -> true)
    in
    match all_old_unique with
    | true -> old_idx_set
    | false -> failwith "Duplicating existing elements of a list is not allowed"
  ;;

  let modify_list' (type g s ids) list_map
        ~(list_id : (g, s, ids) params Type_equal.Id.t)
        ~(f : s list_modifier)
    =
    let (Of_list.Of_list ({ el_desc; list; prefix; witness } as list_state)) =
      Map.find_exn list_map (Type_equal.Id.uid list_id) in
    let Type_equal.T = Type_equal.Id.same_witness_exn list_id witness in
    let new_list =
      let old_list = List.mapi list ~f:(fun i x -> Old (x, i)) in
      let create_new_form ?init () = New init in
      f.transform old_list ~create_new_form
    in
    let remaining_indicies = check_uniqness new_list in
    let removed_keys =
      List.foldi list ~init:[] ~f:(fun i tail state ->
        if not (Hash_set.mem remaining_indicies i) then
          (collect_list_keys_in_use (Identified.extract state) ~list_map ~tail)
        else
          tail)
    in
    let cleared_list_map =
      List.fold removed_keys ~init:list_map ~f:Map.remove
    in
    let modified_list =
      List.map new_list ~f:(function
        | Old _ as x -> x
        | New init ->
          let state, list_states = create' ~init ~prefix (Identified.extract el_desc) in
          New (Identified.create state, list_states)
      )
    in
    let states =
      List.map modified_list
        ~f:(function Old (state,_) | New (state,_) -> state)
    in
    let list_map_to_add =
      List.filter_map modified_list ~f:(function
        | Old _               -> None
        | New (_,list_states) -> Some list_states)
    in
    let list_states =
      List.reduce_balanced ~f:merge_disjoint_maps_exn (cleared_list_map :: list_map_to_add)
      |> Option.value ~default:Type_equal.Id.Uid.Map.empty
    in
    Map.set list_states ~key:(Type_equal.Id.uid list_id) ~data:(
      Of_list.Of_list { list_state with list = states })
  ;;

  let modify_list (Pack t) (List_id.List_id list_id) ~f =
    let list_map = modify_list' ~list_id ~f t.list_map in
    let uid_set = uid_set (Identified.extract t.state) list_map in
    let fields = fields (Identified.extract t.state) list_map in
    Pack { t with uid_set; fields; list_map; }
  ;;

  let cons t ?init list_id =
    let transform =
      fun list ~create_new_form -> create_new_form ?init () :: list
    in
    modify_list t list_id ~f:{ transform }
  ;;

  let append t ?init list_id =
    let transform =
      fun list ~create_new_form -> list @ [create_new_form ?init ()]
    in
    modify_list t list_id ~f:{ transform }
  ;;

  let remove_nth t list_id n =
    let transform =
      fun list ~create_new_form:_ -> List.filteri list ~f:(fun i _ -> i <> n)
    in
    modify_list t list_id ~f:{ transform }
  ;;
end
