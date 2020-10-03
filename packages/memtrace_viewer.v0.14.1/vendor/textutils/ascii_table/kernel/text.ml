open! Core_kernel
open! Import

type t = string [@@deriving compare, sexp_of]

include Container.Make0 (struct
    type nonrec t = t

    module Elt = Uchar

    let fold t ~init ~f =
      Uutf.String.fold_utf_8
        (fun init _pos code_point ->
           let uchar =
             match code_point with
             | `Malformed _ -> Uchar.of_char '?'
             | `Uchar uchar -> uchar
           in
           f init uchar)
        init
        t
    ;;

    let iter = `Define_using_fold
    let length = `Define_using_fold
  end)

let concat ts = String.concat ts
let is_empty = String.is_empty
let of_string = String.of_string
let split = String.split
let to_string = String.to_string


let width t = fold t ~init:0 ~f:(fun n _ -> n + 1)
let bytes = String.length

let chunks_of t ~width =
  match t with
  | "" -> [ "" ]
  | _ ->
    let code_point_ends_before_pos =
      Uutf.String.fold_utf_8 (fun acc start_pos _ -> start_pos :: acc) [] t
      |> List.cons (String.length t)
      |> List.rev
      |> List.tl_exn
    in
    let chunk_ends_before_pos =
      (* This clever [List.chunks_of] call repeats the (incorrect) assumption in [width]
         that every code point has width 1. We have a test to check that [width] and
         [chunks_of] remain consistent. *)
      code_point_ends_before_pos
      |> List.chunks_of ~length:width
      |> List.map ~f:List.last_exn
      |> Sequence.of_list
    in
    chunk_ends_before_pos
    |> Sequence.unfold_with ~init:0 ~f:(fun start_at end_before ->
      Yield (String.sub t ~pos:start_at ~len:(end_before - start_at), end_before))
    |> Sequence.to_list
;;

let of_uchar_list uchars =
  let buf = Buffer.create 8 (* arbitrary small number *) in
  List.iter uchars ~f:(Uutf.Buffer.add_utf_8 buf);
  Buffer.contents buf
;;

include Quickcheckable.Of_quickcheckable
    (struct
      module Uchar = struct
        type t = Uchar.t

        include Quickcheckable.Of_quickcheckable_filtered
            (Int)
            (struct
              type nonrec t = t

              let of_quickcheckable = Uchar.of_scalar
              let to_quickcheckable = Uchar.to_scalar
            end)
      end

      type t = Uchar.t list [@@deriving quickcheck]
    end)
    (struct
      type nonrec t = t

      let of_quickcheckable = of_uchar_list
      let to_quickcheckable = to_list
    end)

let iteri t ~f =
  ignore
    (fold t ~init:0 ~f:(fun i uchar ->
       f i uchar;
       i + 1)
     : int)
;;
