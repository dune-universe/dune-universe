open Core_kernel
open Bin_prot
open Typerep_extended.Std

module Computation_impl = struct

  type 'a t = unit -> Shape.t
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let int       () = Bin_prot.Std.bin_shape_int
  let int32     () = Bin_prot.Std.bin_shape_int32
  let int64     () = Bin_prot.Std.bin_shape_int64
  let nativeint () = Bin_prot.Std.bin_shape_nativeint
  let char      () = Bin_prot.Std.bin_shape_char
  let float     () = Bin_prot.Std.bin_shape_float
  let string    () = Bin_prot.Std.bin_shape_string
  let bool      () = Bin_prot.Std.bin_shape_bool
  let unit      () = Bin_prot.Std.bin_shape_unit

  let option    x () = Bin_prot.Std.bin_shape_option    (x())
  let list      x () = Bin_prot.Std.bin_shape_list      (x())
  let array     x () = Bin_prot.Std.bin_shape_array     (x())
  let lazy_t    x () = Bin_prot.Std.bin_shape_lazy_t    (x())
  let ref_      x () = Bin_prot.Std.bin_shape_ref       (x())

  (* bin_io does *NOT* support serialization of functions *)
  let function_ _ = assert false

  let tuple2 x1 x2              () = Shape.tuple [x1();x2()]
  let tuple3 x1 x2 x3           () = Shape.tuple [x1();x2();x3()]
  let tuple4 x1 x2 x3 x4        () = Shape.tuple [x1();x2();x3();x4()]
  let tuple5 x1 x2 x3 x4 x5     () = Shape.tuple [x1();x2();x3();x4();x5()]

  let record r () =
    Shape.record (List.map (List.range 0 (Record.length r)) ~f:(fun i ->
      match Record.field r i with
      | Record.Field field ->
        let shape : (unit -> Shape.t) = Field.traverse field in
        (Field.label field,shape())
    ))
  ;;

  let variant v () =
    if Variant.is_polymorphic v
    then
      Shape.(poly_variant (Location.of_string "binrep_shaper.ml"))
        (List.map (List.range 0 (Variant.length v)) ~f:(fun i ->
        match Variant.tag v i with
        | Variant.Tag tag ->
          if Tag.arity tag = 0
          then
            Shape.constr (Tag.label tag) None
          else
            let shape : (unit -> Shape.t) = Tag.traverse tag in
            Shape.constr (Tag.label tag) (Some (shape()))
      ))
    else
      Shape.variant (List.map (List.range 0 (Variant.length v)) ~f:(fun i ->
        match Variant.tag v i with
        | Variant.Tag tag ->
          let label = Tag.label tag in
          let arity = Tag.arity tag in
          let shape : (unit -> Shape.t) = Tag.traverse tag in
          let args =
            if arity = 0
            then []
            else
              let args_shapes =
                let args_shape = shape () in
                if arity = 1
                then [ args_shape ]
                else Shape.For_typerep.deconstruct_tuple_exn args_shape
              in
              match Tag.args_labels tag with
              | [] -> args_shapes
              | (_::_) as args_labels ->
                let fields =
                  match List.zip args_labels args_shapes with
                  | Some fields -> fields
                  | None ->
                    raise_s
                      [%sexp "Inconsistent shapes and labels in inline record variant tag"
                           , { label       : string
                             ; arity       : int
                             ; args_labels : string list
                             ; args_shapes : Shape.t list
                             }
                           , [%here]
                      ]
                in
                [ Shape.record fields ]
          in
          label, args))
  ;;

  module Named = Type_generic.Make_named_for_closure(struct
    type 'a input = unit
    type 'a output = Shape.t
    type 'a t = 'a input -> 'a output
  end)
end

include Type_generic.Make(struct
  include Computation_impl
  let name = "bin_shaper"
  let required = [ Type_struct.Generic.ident ]
end)

