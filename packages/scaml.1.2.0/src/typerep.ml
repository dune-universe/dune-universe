open Spotlib.Spot
open Typerep_lib.Std

module C = Michelson.Constant

module Convert = struct
  open C

  let name = "Convert"
  let required = []

  type 'a t = 'a -> C.t

  include Typerep_lib.Variant_and_record_intf.M(struct
      type nonrec 'a t = 'a t
    end)

  exception Unsupported of string
  let unsupported n = raise (Unsupported n)

  let int _ = unsupported "int"
  let int32 _ = unsupported "int32"
  let int64 _ = unsupported "int64"
  let nativeint _ = unsupported "nativeint"
  let char _ = unsupported "char"
  let float _ = unsupported "float"
  let string s = String s
  let bytes _ = unsupported "bytes"
  let bool b = Bool b
  let unit () = Unit
  let option f = function
    | None -> Option None
    | Some x -> Option (Some (f x))
  let list f xs = List (List.map f xs)
  let array _ _ = unsupported "array"
  let lazy_t _ _ = unsupported "lazy_t"
  let ref_ _ _ = unsupported "ref"
  let function_ _ _ _ = unsupported "function"

  let tuple xs =
    Binplace.fold (Binplace.place xs)
      ~leaf: (fun x -> x)
      ~branch: (fun x y -> Pair (x,y))

  let tuple2 f1 f2 (x1, x2) = tuple [f1 x1; f2 x2]
  let tuple3 f1 f2 f3 (x1, x2, x3) = tuple [f1 x1; f2 x2; f3 x3]
  let tuple4 f1 f2 f3 f4 (x1, x2, x3, x4) = tuple [f1 x1; f2 x2; f3 x3; f4 x4]
  let tuple5 f1 f2 f3 f4 f5 (x1, x2, x3, x4, x5) = tuple [f1 x1; f2 x2; f3 x3; f4 x4; f5 x5]

  let record : 'a Record.t -> 'a t = fun r value ->
    let xs = List.rev & Record.fold r ~init:[] ~f:(fun acc (Field f) ->
        let c = Field.traverse f in
        let k = Field.label f in
        let v = Field.get f value in
        (k, c v) :: acc)
    in
    match xs with
    | [] -> assert false
    | [_k, m] -> m
    | _ ->
        Binplace.fold (Binplace.place xs)
          ~leaf: (fun (_k,x) -> x)
          ~branch: (fun x y -> Pair (x,y))

  let variant : 'a Variant.t -> 'a t = fun v value ->
    let nulls, nonnulls =
      Variant.fold v ~init:(0,0) ~f:(fun (nulls, nonnulls) (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then (nulls + 1, nonnulls) else (nulls, nonnulls+1))
    in
    let rec embed args = function
      | [] -> args
      | Binplace.Left::xs -> Left (embed args xs)
      | Binplace.Right::xs -> Right (embed args xs)
    in
    match nulls, nonnulls with
    | 0, 0 -> assert false
    | _, 0 ->
        let Value (tag, _) = Variant.value v value in
        Int (Z.of_int (Tag.ocaml_repr tag ))
    | 0, _ ->
        let Value (tag, args) = Variant.value v value in
        let args = Tag.traverse tag args in
        let i = Tag.ocaml_repr tag in
        let sides = Binplace.path i nonnulls in
        embed args sides
    | _, _ ->
        let Value (tag, args) = Variant.value v value in
        let arity = Tag.arity tag in
        if arity = 0 then
          embed (Int (Z.of_int (Tag.ocaml_repr tag)))
            (Binplace.path 0 (nonnulls + 1))
        else
          let args = Tag.traverse tag args in
          let i = Tag.ocaml_repr tag in
          let sides = Binplace.path (i+1) (nonnulls+1) in
          embed args sides

  module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = 'a
      type 'a output = C.t
      type 'a t = 'a input -> 'a output
    end)
end

module ConvertType = struct

  let name = "ConvertType"
  let required = []

  type 'a t = unit -> Michelson.Type.t

  module Ty = Michelson.Type

  include Typerep_lib.Variant_and_record_intf.M(struct
      type nonrec 'a t = 'a t
    end)

  exception Unsupported of string
  let unsupported n = raise (Unsupported n)

  let int _ = unsupported "int"
  let int32 _ = unsupported "int32"
  let int64 _ = unsupported "int64"
  let nativeint _ = unsupported "nativeint"
  let char _ = unsupported "char"
  let float _ = unsupported "float"
  let bytes _ = unsupported "bytes"
  let array _ _ = unsupported "array"
  let lazy_t _ _ = unsupported "lazy_t"
  let ref_ _ _ = unsupported "ref"
  let function_ _ _ _ = unsupported "function"

  let string () = Ty.tyString
  let bool () = Ty.tyBool
  let unit () = Ty.tyUnit
  let option f () = Ty.tyOption (None, f ())
  let list f () = Ty.tyList (f ())

  let encode_by branch xs =
    Binplace.fold
        ~leaf:(fun x -> x)
        ~branch
    & Binplace.place xs

  let tuple2 f1 f2 () = encode_by (fun ty1 ty2 -> Ty.tyPair (None,ty1, None,ty2)) [f1 (); f2 ()]
  let tuple3 f1 f2 f3 () = encode_by (fun ty1 ty2 -> Ty.tyPair (None,ty1, None,ty2)) [f1 (); f2 (); f3 ()]
  let tuple4 f1 f2 f3 f4 () = encode_by (fun ty1 ty2 -> Ty.tyPair (None,ty1, None,ty2)) [f1 (); f2 (); f3 (); f4 ()]
  let tuple5 f1 f2 f3 f4 f5 () = encode_by (fun ty1 ty2 -> Ty.tyPair (None,ty1, None,ty2)) [f1 (); f2 (); f3 (); f4 (); f5 ()]

  let record : 'a Record.t -> 'a t = fun r () ->
    let xs = List.rev & Record.fold r ~init:[] ~f:(fun acc (Field f) ->
        let c = Field.traverse f () in
        let k = Field.label f in
        (k, c) :: acc)
    in
    snd
    @@ encode_by (fun (f1,ty1) (f2,ty2) -> None, Ty.tyPair (f1,ty1, f2,ty2))
    @@ List.map (fun (l,ty) -> (Some l, ty)) xs

  let variant : 'a Variant.t -> 'a t = fun v () ->
    let nulls, nonnulls =
      Variant.fold v ~init:(0,0) ~f:(fun (nulls, nonnulls) (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then (nulls + 1, nonnulls) else (nulls, nonnulls+1))
    in
    let n_nulls =
      Variant.fold v ~init:[] ~f:(fun revs (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then Tag.label tag :: revs else revs)
    in
    let c_nonnulls =
      Variant.fold v ~init:[] ~f:(fun revs (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then revs
          else
            (Tag.label tag, Tag.traverse tag ()) :: revs)
    in
    match nulls, nonnulls with
    | 0, 0 -> assert false
    | _, 0 ->
        Ty.tyInt
    | 0, _ ->
        snd
        & encode_by (fun (f1,ty1) (f2,ty2) -> None, Ty.tyOr (f1,ty1, f2,ty2))
        & List.map (fun (l,ty) -> Some l, ty) c_nonnulls
    | _, _ ->
        snd
        & encode_by (fun (f1,ty1) (f2,ty2) -> None, Ty.tyOr (f1,ty1, f2,ty2))
        & (Some (String.concat "_" n_nulls), Ty.tyInt)
          :: List.map (fun (l,ty) -> Some l, ty) c_nonnulls

  module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = unit
      type 'a output = Michelson.Type.t
      type 'a t = 'a input -> 'a output
    end)
end

module Revert = struct
  open C

  let name = "Revert"
  let required = []

  type 'a t = C.t -> 'a option

  include Typerep_lib.Variant_and_record_intf.M(struct
      type nonrec 'a t = 'a t
    end)

  exception Unsupported of string
  let unsupported n = raise (Unsupported n)

  open Option.Infix

  let int _ = unsupported "int"
  let int32 _ = unsupported "int32"
  let int64 _ = unsupported "int64"
  let nativeint _ = unsupported "nativeint"
  let char _ = unsupported "char"
  let float _ = unsupported "float"
  let string = function
    | String s -> Some s
    | _ -> None
  let bytes _ = unsupported "bytes"
  let bool = function
    | Bool b -> Some b
    | _ -> None
  let unit = function
    | Unit -> Some ()
    | _ -> None
  let option f x = match x with
    | Option None -> Some None
    | Option (Some x) -> f x >>| fun x -> Some x
    | _ -> None
  let list f = function
    | List xs -> Option.mapM f xs
    | _ -> None
  let array _ _ = unsupported "array"
  let lazy_t _ _ = unsupported "lazy_t"
  let ref_ _ _ = unsupported "ref"
  let function_ _ _ _ = unsupported "function"

  let rec access v sides = match v, sides with
    | _, [] -> Some v
    | Pair (v, _), Binplace.Left::sides -> access v sides
    | Pair (_, v), Right::sides -> access v sides
    | _ -> None

  let tuple n v =
    Option.mapM (fun sides -> access v sides)
    & List.init n & fun i -> Binplace.path i n

  let tuple2 f1 f2 v = tuple 2 v >>= function
    | [x1; x2] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> Some (x1, x2)
    | _ -> None
  let tuple3 f1 f2 f3 v = tuple 3 v >>= function
    | [x1; x2; x3] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> f3 x3 >>= fun x3 -> Some (x1, x2, x3)
    | _ -> None
  let tuple4 f1 f2 f3 f4 v = tuple 4 v >>= function
    | [x1; x2; x3; x4] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> f3 x3 >>= fun x3 -> f4 x4 >>= fun x4 -> Some (x1, x2, x3, x4)
    | _ -> None
  let tuple5 f1 f2 f3 f4 f5 v = tuple 5 v >>= function
    | [x1; x2; x3; x4; x5] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> f3 x3 >>= fun x3 -> f4 x4 >>= fun x4 -> f5 x5 >>= fun x5 -> Some (x1, x2, x3, x4, x5)
    | _ -> None

  let record : 'a Record.t -> 'a t = fun r m ->
    let len = Record.length r in
    tuple len m >>= fun ms ->
    let get f =
      let i = Field.index f in
      match Field.traverse f (List.nth ms i) with
      | Some x -> x
      | None -> raise Exit
    in
    let fields = { Record.get } in
    match Record.create r fields with
    | exception Exit -> None
    | v -> Some v

  let variant : 'a Variant.t -> 'a t = fun v m ->
    let _, rev_nulls, rev_nonnulls =
      Variant.fold v ~init:(0,[],[]) ~f:(fun (i, nulls, nonnulls) (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then (i+1, i::nulls, nonnulls) else (i+1, nulls, i::nonnulls))
    in
    let nulls = List.rev rev_nulls in
    let nonnulls = List.rev rev_nonnulls in
    let n_nulls = List.length nulls in
    let n_nonnulls = List.length nonnulls in
    match n_nulls, n_nonnulls with
    | 0, 0 -> assert false
    | _, 0 ->
        begin match m with
          | Int z ->
            if z >= Z.of_int n_nulls || z < Z.zero then None
            else
              let i = Z.to_int z in
              let pos = List.nth nulls i in
              assert (i = pos);
              let Tag tag = Variant.tag v pos in
              begin match Tag.create tag with
              | Const v -> Some v
              | _ -> assert false
              end
          | _ -> None
        end
    | 0, _ ->
        let tree = Binplace.place (List.init n_nonnulls (fun x -> x)) in
        let rec find m tree = match m, tree with
          | m, Binplace.Leaf i -> Some (m, i)
          | Left m, Branch (tree, _) -> find m tree
          | Right m, Branch (_, tree) -> find m tree
          | _ -> None
        in
        find m tree >>= fun (m, i) ->
        let Tag tag = Variant.tag v i in
        begin match Tag.create tag with
          | Const _ -> assert false
          | Args f -> Tag.traverse tag m >>| f
        end
    | _, _ ->
        let tree = Binplace.place (-1 :: List.init n_nonnulls (fun x -> x)) in
        let rec find m tree = match m, tree with
          | m, Binplace.Leaf i -> Some (m, i)
          | Left m, Branch (tree, _) -> find m tree
          | Right m, Branch (_, tree) -> find m tree
          | _ -> None
        in
        find m tree >>= fun (m, i) ->
        if i = -1 then begin
          match m with
          | Int z ->
              if z >= Z.of_int n_nulls || z < Z.zero then None
              else
                let i = Z.to_int z in
                let pos = List.nth nulls i in
                let Tag tag = Variant.tag v pos in
                begin match Tag.create tag with
                | Const v -> Some v
                | _ -> assert false
                end
          | _ -> None
        end else
          let pos = List.nth nonnulls i in
          let Tag tag = Variant.tag v pos in
          begin match Tag.create tag with
            | Const _ -> assert false
            | Args f -> Tag.traverse tag m >>| f
          end

  module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = C.t
      type 'a output = 'a option
      type 'a t = 'a input -> 'a output
    end)
end

module Revert' = struct
  module M = Tezos_micheline.Micheline
  module C = C

  let name = "Revert'"
  let required = []

  type 'a t = (int, string) M.node -> C.t option

  include Typerep_lib.Variant_and_record_intf.M(struct
      type nonrec 'a t = 'a t
    end)

  exception Unsupported of string
  let unsupported n = raise (Unsupported n)

  open Option.Infix

  let int _ = unsupported "int"
  let int32 _ = unsupported "int32"
  let int64 _ = unsupported "int64"
  let nativeint _ = unsupported "nativeint"
  let char _ = unsupported "char"
  let float _ = unsupported "float"
  let string = function
    | M.String (_, s) -> Some (C.String s)
    | _ -> None
  let bytes _ = unsupported "bytes"
  let bool = function
    | M.Prim (_, "True", [], []) -> Some (C.Bool true)
    | M.Prim (_, "False", [], []) -> Some (C.Bool false)
    | _ -> None
  let unit = function
    | M.Prim (_, "Unit", [], []) -> Some C.Unit
    | _ -> None
  let option f x = match x with
    | M.Prim (_, "Some", [t], []) -> f t >>= fun t -> Some (C.Option (Some t))
    | M.Prim (_, "None", [], []) -> Some (C.Option None)
    | _ -> None
  let list f = function
    | M.Seq (_, ts) -> Option.mapM f ts >>= fun ts -> Some (C.List ts)
    | _ -> None
  let array _ _ = unsupported "array"
  let lazy_t _ _ = unsupported "lazy_t"
  let ref_ _ _ = unsupported "ref"
  let function_ _ _ _ = unsupported "function"

  let rec tuple tree v = match tree, v with
    | Binplace.Leaf f, v -> f v
    | Binplace.Branch (tr1, tr2), M.Prim (_, "Pair", [t1; t2], []) ->
        tuple tr1 t1 >>= fun t1 -> tuple tr2 t2 >>= fun t2 -> Some (C.Pair (t1, t2))
    | _ -> None

  let tuple2 f1 f2 = tuple (Binplace.place [f1; f2])
  let tuple3 f1 f2 f3 = tuple (Binplace.place [f1; f2; f3])
  let tuple4 f1 f2 f3 f4 = tuple (Binplace.place [f1; f2; f3; f4])
  let tuple5 f1 f2 f3 f4 f5 = tuple (Binplace.place [f1; f2; f3; f4; f5])

  let rec access v sides = match v, sides with
    | _, [] -> Some v
    | M.Prim (_, "Pair", [v; _], []), Binplace.Left::sides -> access v sides
    | M.Prim (_, "Pair", [_; v], []), Right::sides -> access v sides
    | _ -> None

  let tuple n v =
    Option.mapM (fun sides -> access v sides)
    & List.init n & fun i -> Binplace.path i n

  let build cs =
    let rec f = function
      | Binplace.Leaf v -> v
      | Binplace.Branch (t1, t2) -> C.Pair (f t1, f t2)
    in
    f & Binplace.place cs

  let record : 'a Record.t -> 'a t = fun r m ->
    let len = Record.length r in
    tuple len m >>= fun ms ->
    let fields = List.init len & fun i -> Record.field r i in
    let mfs = List.combine ms fields in
    Option.mapM (fun (m, Record.Field f) -> Field.traverse f m) mfs
    >>| build

(*
  let pp_ml ppf ml =
    Tezos_micheline.Micheline_printer.print_expr ppf
      (Tezos_micheline.Micheline_printer.printable (fun _ -> "")
         (Tezos_micheline.Micheline.strip_locations ml))
*)

  let variant : 'a Variant.t -> 'a t = fun v m ->
    let _, rev_nulls, rev_nonnulls =
      Variant.fold v ~init:(0,[],[]) ~f:(fun (i, nulls, nonnulls) (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then (i+1, i::nulls, nonnulls) else (i+1, nulls, i::nonnulls))
    in
    let nulls = List.rev rev_nulls in
    let nonnulls = List.rev rev_nonnulls in
    let n_nulls = List.length nulls in
    let n_nonnulls = List.length nonnulls in
    let rec embed m tree ca = match m, tree with
      | _, Binplace.Leaf _ -> ca
      | M.Prim (_, "Left", [m], []), Branch (tree, _) -> C.Left (embed m tree ca)
      | Prim (_, "Right", [m], []), Branch (_, tree) -> C.Right (embed m tree ca)
      | _ -> assert false
    in
    match n_nulls, n_nonnulls with
    | 0, 0 -> assert false
    | _, 0 ->
        begin match m with
          | M.Int (_, z) -> Some (C.Int z)
          | _ -> None
        end
    | 0, _ ->
        let tree = Binplace.place (List.init n_nonnulls (fun x -> x)) in
        let rec find m tree = match m, tree with
          | m, Binplace.Leaf pos -> Some (m, pos)
          | M.Prim (_, "Left", [m], []), Branch (tree, _) -> find m tree
          | Prim (_, "Right", [m], []), Branch (_, tree) -> find m tree
          | _ -> None
        in
        find m tree >>= fun (ma, pos) ->
        let Tag tag = Variant.tag v pos in
        Tag.traverse tag ma >>= fun ca ->
        Some (embed m tree ca)

    | _, _ ->
        let tree = Binplace.place (-1 :: List.init n_nonnulls (fun x -> x)) in
        let rec find m tree = match m, tree with
          | m, Binplace.Leaf i -> Some (m, i)
          | M.Prim (_, "Left", [m], []), Branch (tree, _) -> find m tree
          | Prim (_, "Right", [m], []), Branch (_, tree) -> find m tree
          | _ -> None
        in
        find m tree >>= fun (ma, i) ->
        if i = -1 then begin
          match ma with
          | M.Int (_,z) ->
              if z >= Z.of_int n_nulls || z < Z.zero then None
              else Some (embed m tree (C.Int z))
          | _ -> None
        end else begin
          let pos = List.nth nonnulls i in
          let Tag tag = Variant.tag v pos in
          Tag.traverse tag ma >>= fun ca ->
          Some (embed m tree ca)
        end

  module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = (int, string) M.node
      type 'a output = C.t option
      type 'a t = 'a input -> 'a output
    end)
end

let to_michelson typerep v =
  let open SCaml in
  let module M = Type_generic.Make(Convert) in
  M.register typerep_of_int (fun (Int n) -> Int (Z.of_int n));
  M.register typerep_of_nat (fun (Nat n) -> Int (Z.of_int n));
  M.register typerep_of_tz (fun (Tz f) -> Int (Z.of_float (f *. 1000000.)));
  M.register1 (module struct
    type 'a t = 'a set
    let typename_of_t = typename_of_set
    let typerep_of_t = typerep_of_set
    let compute fa = fun (SCaml.Set xs) -> C.Set (List.map fa xs)
  end);
  M.register2 (module struct
    type ('a,'b) t = ('a,'b) map
    let typename_of_t = typename_of_map
    let typerep_of_t = typerep_of_map
    let compute fk fv = fun (SCaml.Map kvs) ->
      C.Map (List.map (fun (k,v) -> (fk k, fv v)) kvs)
  end);
  M.register typerep_of_bytes (fun (Bytes bs) -> Bytes bs);
  M.register typerep_of_address (fun (Address s) -> String s);
  M.register typerep_of_key_hash (fun (Key_hash s) -> String s);
  M.register typerep_of_timestamp (fun (Timestamp s) -> String s);
  M.register typerep_of_key (fun (Key s) -> String s);
  M.register typerep_of_signature (fun (Signature s) -> String s);
  let `generic f = M.of_typerep typerep in
  f v

exception Overflow
exception Rounded

let of_michelson typerep v =
  let open SCaml in
  let open Spotlib.Spot.Option.Infix in
  let module M = Type_generic.Make(Revert) in
  M.register typerep_of_int (function
      | Int z ->
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else Some (Int i)
      | _ -> None);
  M.register typerep_of_nat (function
      | Int z ->
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else if i < 0 then None
          else Some (Nat i)
      | _ -> None);
  M.register typerep_of_tz (function
      | Int z ->
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else
            let f = float i /. 1000000. in
            if int_of_float (f *. 1000000.) <> i then raise Rounded
            else Some (Tz f)
      | _ -> None);
  M.register1 (module struct
    type 'a t = 'a set
    let typename_of_t = typename_of_set
    let typerep_of_t = typerep_of_set
    let compute fa = function
      | C.Set xs -> Spotlib.Spot.Option.mapM fa xs >>| fun xs -> SCaml.Set xs
      | _ -> None
  end);
  M.register2 (module struct
    type ('k,'v) t = ('k,'v) map
    let typename_of_t = typename_of_map
    let typerep_of_t = typerep_of_map
    let compute fk fv = function
      | C.Map kvs ->
          Spotlib.Spot.Option.mapM (fun (k,v) ->
              fk k >>= fun k ->
              fv v >>| fun v -> (k,v)) kvs
          >>| fun kvs -> SCaml.Map kvs
      | _ -> None
  end);
  M.register typerep_of_bytes (function
      | Bytes bs -> Some (Bytes bs)
      | _ -> None);
  M.register typerep_of_address (function
      | String s -> Some (Address s)
      | _ -> None);
  M.register typerep_of_key_hash (function
      | String s -> Some (Key_hash s)
      | _ -> None);
  M.register typerep_of_timestamp (function
      | String s -> Some (Timestamp s)
      (* XXX Int case *)
      | _ -> None);
  M.register typerep_of_key (function
      | String s -> Some (Key s)
      | _ -> None);
  M.register typerep_of_signature (function
      | String s -> Some (Signature s)
      | _ -> None);
  let `generic f = M.of_typerep typerep in
  f v

let michelson_of_micheline typerep v =
  let open SCaml in
  let open Spotlib.Spot.Option.Infix in
  let module X = Type_generic.Make(Revert') in
  let module M = Tezos_micheline.Micheline in
  let module C = C in
  X.register typerep_of_int (function
      | M.Int (_, z) -> Some (C.Int z)
      | _ -> None);
  X.register typerep_of_nat (function
      | M.Int (_, z) when z < Z.zero -> None
      | Int (_, z) -> Some (C.Int z)
      | _ -> None);
  X.register typerep_of_tz (function
      | M.Int (_, z) ->
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else Some (C.Int z) (* XXX overflow exists *)
      | _ -> None);
  X.register1 (module struct
    type 'a t = 'a set
    let typename_of_t = typename_of_set
    let typerep_of_t = typerep_of_set
    let compute fa = function
      | M.Seq (_, xs) -> Spotlib.Spot.Option.mapM fa xs >>| fun xs -> C.Set xs
      | _ -> None
  end);
  X.register2 (module struct
    type ('k,'v) t = ('k,'v) map
    let typename_of_t = typename_of_map
    let typerep_of_t = typerep_of_map
    let compute fk fv = function
      | M.Seq (_, kvs) ->
          Spotlib.Spot.Option.mapM (function
              | M.Prim (_, "Elt", [k; v], []) ->
                  fk k >>= fun k ->
                  fv v >>| fun v -> (k,v)
              | _ -> None) kvs
          >>| fun kvs -> C.Map kvs
      | _ -> None
  end);
  X.register typerep_of_bytes (function
      | M.Bytes (_, bs (* in hex *)) ->
          let s = Stdlib.Bytes.to_string bs in
          let `Hex h = Hex.of_string s in
          Some (C.Bytes h)
      | _ -> None);
  X.register typerep_of_address (function
      | M.String (_, s) -> Some (C.String s)
      | _ -> None);
  X.register typerep_of_key_hash (function
      | M.String (_, s) -> Some (C.String s)
      | _ -> None);
  X.register typerep_of_timestamp (function
      | M.String (_, s) -> Some (C.String s) (* XXX int representation *)
      | _ -> None);
  X.register typerep_of_key (function
      | M.String (_, s) -> Some (C.String s)
      | _ -> None);
  X.register typerep_of_signature (function
      | M.String (_, s) -> Some (C.String s)
      | _ -> None);
  let `generic f = X.of_typerep typerep in
  f v

let to_michelson_type typerep =
  let open SCaml in
  let module M = Type_generic.Make(ConvertType) in
  let module Ty = Michelson.Type in
  M.register typerep_of_int (fun () -> Ty.tyInt);
  M.register typerep_of_nat (fun () -> Ty.tyNat);
  M.register typerep_of_tz (fun () -> Ty.tyMutez); (* XXX tricky *)
  M.register1 (module struct
    type 'a t = 'a set
    let typename_of_t = typename_of_set
    let typerep_of_t = typerep_of_set
    let compute fa () = Ty.tySet (fa ())
  end);
  M.register2 (module struct
    type ('k,'v) t = ('k,'v) map
    let typename_of_t = typename_of_map
    let typerep_of_t = typerep_of_map
    let compute fk fv () = Ty.tyMap (fk (), fv ())
  end);
  M.register typerep_of_bytes (fun () -> Ty.tyBytes);
  M.register typerep_of_address (fun () -> Ty.tyAddress);
  M.register typerep_of_key_hash (fun () -> Ty.tyKeyHash);
  M.register typerep_of_timestamp (fun () -> Ty.tyTimestamp);
  M.register typerep_of_key (fun () -> Ty.tyKey);
  M.register typerep_of_signature (fun () -> Ty.tySignature);
  let `generic f = M.of_typerep typerep in
  f ()

let to_micheline rep x =
  C.to_micheline ~block_comment:false @@ to_michelson rep x

let of_micheline rep x =
  let open Option.Infix in
  michelson_of_micheline rep x >>= fun m ->
  of_michelson rep m

let micheline_of_michelson = C.to_micheline ~block_comment:false

module TypeSafePack : SCaml.Obj.Internal.TypeSafePack = struct
  let expr_encoding =
    Tezos_micheline.Micheline.canonical_encoding_v1
      ~variant:"michelson_v1"
      Data_encoding.Encoding.string

  let pack' rep a =
    match
      Data_encoding.Binary.to_bytes expr_encoding
      @@ Tezos_micheline.Micheline.strip_locations
      @@ C.to_micheline ~block_comment:false
      @@ to_michelson rep a
    with
    | Error _ -> assert false
    | Ok bs -> "\005" ^ Stdlib.Bytes.to_string bs

  let unpack' rep s =
    if s = "" then None
    else if Stdlib.String.unsafe_get s 0 <> '\005' then None
    else
      let open Stdlib in
      let (>>=) = Option.bind in
      let mres =
        Data_encoding.Binary.of_bytes expr_encoding
        @@ Bytes.of_string @@ String.(sub s 1 (length s - 1))
      in
      Result.to_option mres >>= fun m ->
      of_micheline rep @@ Tezos_micheline.Micheline.root m
end

let () = SCaml.Obj.Internal.type_safe_pack := Some (module TypeSafePack)
