open Core_kernel

type t =
  { msg : string
  ; reraised_exn : Exn.t sexp_option
  ; here : Source_code_position.t
  ; query : string sexp_option
  ; params : Db_field.t option sexp_list
  ; formatted_query : string sexp_option
  ; results : Row.t list sexp_list }
[@@deriving sexp_of]

exception Mssql_error of t
[@@deriving sexp_of]

let make ~msg ?exn ~here ?query ?(params=[]) ?formatted_query ?(results=[]) () =
  Mssql_error
    { msg
    ; reraised_exn = exn
    ; here
    ; query
    ; params
    ; formatted_query
    ; results }

let failwith ?query ?params ?formatted_query ?results ?exn ?backtrace
      here msg =
  let exn = make ~here ~msg ?query ?params ?formatted_query ?results ?exn () in
  match backtrace with
  | None ->
    raise exn
  | Some backtrace ->
    Caml.Printexc.raise_with_backtrace exn backtrace

let failwithf ?query ?params ?formatted_query ?results ?exn ?backtrace
      here fmt =
  ksprintf (fun msg ->
    failwith ?query ?params ?formatted_query ?results ?exn ?backtrace here msg)
    fmt

let with_wrap ?query ?(params=[]) ?formatted_query ?(results=[]) here f =
  try
    f ()
  with
  | (Mssql_error t) ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    let exn =
      (* Preserve original info if set, but override if not set *)
      (* Note that we never mess with the original [%here] *)
      Mssql_error
        { t with
          query = Option.first_some t.query query
        ; params = (match t.params with [] -> params | _ -> t.params)
        ; formatted_query = Option.first_some t.formatted_query formatted_query
        ; results = (match t.results with [] -> results | _ -> t.results) }
    in
    Caml.Printexc.raise_with_backtrace exn backtrace
  | (Freetds.Dblib.Error (_, msg)) as exn ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    failwith here ?query ~params ?formatted_query ~backtrace ~exn msg
  | exn ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    failwith here ?query ~params ?formatted_query ~backtrace ~exn
      "Unexpected error in Dblib"
