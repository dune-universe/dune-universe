type t =
  { a : string
  ; b : string
  ; c : string
  }
[@@deriving prms]

let x = { a = "a"; b = "b"; c = "c" }
let y = map ~f:(fun x -> x ^ "hh") x
let () = iter ~f:(fun x -> Printf.printf "%s" x) x
