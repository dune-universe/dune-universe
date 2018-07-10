open Ctypes
module Defn = struct
  type t = T.Defn.t
  let t = T.Defn.t

  type field_type_t =
    | Integer
    | IntegerList
    | Real
    | RealList
    | String
    | StringList
    | WideString
    | WideStringList
    | Binary
    | Date
    | Time
    | DateTime

  let field_type_of_int = function
    | 0 -> Integer
    | 1 -> IntegerList
    | 2 -> Real
    | 3 -> RealList
    | 4 -> String
    | 5 -> StringList
    | 6 -> WideString
    | 7 -> WideStringList
    | 8 -> Binary
    | 9 -> Date
    | 10 -> Time
    | 11 -> DateTime
    | _ -> raise (Invalid_argument "Field.of_int")

  let get_type =
    Lib.c "OGR_Fld_GetType"
      (t @-> returning int)

  let get_type defn =
    get_type defn
    |> field_type_of_int

  let get_name =
    Lib.c "OGR_Fld_GetNameRef"
      (t @-> returning string)
end
