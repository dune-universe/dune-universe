open Ctypes

exception Invalid_driver

type t = T.t
let t = T.t

let get_short_name =
  Lib.c "GDALGetDriverShortName"
    (t @-> returning string)

let get_long_name =
  Lib.c "GDALGetDriverLongName"
    (t @-> returning string)

let get_by_name =
  Lib.c "GDALGetDriverByName"
    (string @-> returning t)

let get_by_name name =
  let driver = get_by_name name in
  if driver = null then
    None
  else
    Some driver

let get_by_name_exn name =
  match get_by_name name with
  | Some d -> d
  | None -> raise Invalid_driver

let identify =
  Lib.c "GDALIdentifyDriver"
    (string @-> ptr string_opt @-> returning t)

let identify ?(options = []) name =
  let options = Lib.convert_creation_options options in
  let driver = identify name (Lib.creation_options_to_ptr options) in
  if driver = null then
    None
  else
    Some driver
