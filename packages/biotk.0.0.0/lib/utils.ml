open Core_kernel

let unique_value xs ~equal ~f =
  match xs with
  | [] -> Error `Empty
  | h :: t ->
    let v = f h in
    let mismatches =
      List.filter_map t ~f:(fun x ->
          let w = f x in
          if equal v w then None else Some w
        )
    in
    if List.is_empty mismatches then Ok v
    else Error (`Not_unique (v :: mismatches))

let unique_string xs ~f =
  match unique_value xs ~equal:String.equal ~f with
  | Error `Empty -> Or_error.error_string "Empty list"
  | Error (`Not_unique mismatches) ->
    Or_error.error "multiple values" mismatches [%sexp_of: string list]
  | Ok r -> Ok r
