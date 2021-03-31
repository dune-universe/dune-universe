module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
end

let raise_caqti_error err =
  match err with
  | Error err -> failwith (Caqti_error.show err)
  | Ok result -> result
;;
