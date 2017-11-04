open Core
open Re2

module Base = struct
  type t =
    (* When adding to this type, don't forget to add to examples below. *)
    [ `exists_header of string * Regex.t
    | `all_headers   of string * Regex.t
    ] [@@deriving sexp]

  let matches t email =
    match t with
    | `exists_header (header, regex) ->
      let headers =
        Headers.find_all (Email.headers email) header
      in
      List.exists headers ~f:(Regex.matches regex)
    | `all_headers (header, regex) ->
      let headers =
        Headers.find_all (Email.headers email) header
      in
      List.for_all headers ~f:(Regex.matches regex)

  let examples =
    [ `exists_header ("cc",
                      Regex.of_string ".*@janestreet.com")
    ; `all_headers ("cc",
                    Regex.of_string ".*@janestreet.com")
    ]
end

type t = Base.t Blang.t [@@deriving sexp]

let matches t email =
  Blang.eval t (fun base -> Base.matches base email)

let example : t =
  Blang.and_ (List.map Base.examples ~f:Blang.base)
