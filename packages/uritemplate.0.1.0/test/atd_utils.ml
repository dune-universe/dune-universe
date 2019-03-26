module Variant_from_type : sig
  val normalize : Yojson.Safe.t -> Yojson.Safe.t
  val restore : Yojson.Safe.t -> Yojson.Safe.t
end =
struct
  let normalize (j: Yojson.Safe.t) = match j with
    | `String s -> `List [ `String "String"; `String s ]
    | `List l -> `List [ `String "List"; `List l ]
    | `Assoc a -> `List [ `String "Assoc"; `Assoc a ]
    | a -> a

  let restore a = a
end
