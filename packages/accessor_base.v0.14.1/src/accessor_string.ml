open! Base
open! Import

module T = struct
  let list =
    [%accessor Accessor.isomorphism ~get:String.to_list ~construct:String.of_char_list]
  ;;

  let reversed = [%accessor Accessor.isomorphism ~get:String.rev ~construct:String.rev]
  let each = [%accessor list @> Accessor_list.each]
  let eachi = [%accessor list @> Accessor_list.eachi]

  let split_n i =
    Accessor.isomorphism
      ~get:(fun string ->
        let suffix = String.drop_prefix string i in
        let prefix = String.drop_prefix string (String.length suffix) in
        prefix, suffix)
      ~construct:(fun (prefix, suffix) -> prefix ^ suffix)
  ;;

  let prefixed prefix =
    Accessor.variant
      ~match_:(fun string ->
        match String.chop_prefix string ~prefix with
        | None -> Second string
        | Some suffix -> First suffix)
      ~construct:(fun suffix -> prefix ^ suffix)
  ;;

  let suffixed suffix =
    Accessor.variant
      ~match_:(fun string ->
        match String.chop_suffix string ~suffix with
        | None -> Second string
        | Some prefix -> First prefix)
      ~construct:(fun prefix -> prefix ^ suffix)
  ;;

  let conv (type a) (module A : Stringable.S with type t = a) =
    Accessor.variant
      ~match_:(fun s ->
        match A.of_string s with
        | a -> First a
        | exception _ -> Second s)
      ~construct:A.to_string
  ;;

  let conv_strict (type a) (module A : Stringable.S with type t = a) =
    Accessor.isomorphism ~get:A.of_string ~construct:A.to_string
  ;;
end

include T
