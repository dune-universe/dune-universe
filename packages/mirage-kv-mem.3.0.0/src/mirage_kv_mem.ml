type write_error = Mirage_kv.write_error
let pp_write_error = Mirage_kv.pp_write_error

type error = Mirage_kv.error
let pp_error = Mirage_kv.pp_error

module Pure = struct

  open Rresult.R.Infix

  module M = Map.Make(String)

  type t =
   | Dictionary of Ptime.t * t M.t
   | Value of Ptime.t * string

  type key = Mirage_kv.Key.t

  let empty now () = Dictionary (now, M.empty)

  let get_node t key =
    let rec find t = function
      | [] -> Ok t
      | hd::tl -> match t with
        | Value _ -> Error (`Dictionary_expected key)
        | Dictionary (_, m) ->
          match M.find_opt hd m with
          | Some t' -> find t' tl
          | None -> Error (`Not_found key)
    in
    find t (Mirage_kv.Key.segments key)

  let get t key =
    get_node t key >>= function
    | Dictionary _ -> Error (`Value_expected key)
    | Value (_, value) -> Ok value

  let last_modified t key =
    get_node t key >>= function
    | Dictionary (mtime, _) -> Ok mtime
    | Value (mtime, _) -> Ok mtime

  let remove t key now =
    let rec remove t = function
      | [] -> Ok (Dictionary (now, M.empty))
      | [x] -> begin match t with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary (_, m) ->
            let m' = M.remove x m in
            Ok (Dictionary (now, m'))
        end
      | hd::tl -> match t with
        | Value _ -> Error (`Dictionary_expected key)
        | Dictionary (mtime, m) ->
          (match M.find_opt hd m with
           | None -> Error (`Dictionary_expected key)
           | Some t' -> Ok t') >>= fun node ->
          remove node tl >>| fun t' ->
          let m' = M.add hd t' m in
          Dictionary (mtime, m')
    in
    remove t (Mirage_kv.Key.segments key)

  let list t key =
    get_node t key >>= function
    | Value _ -> Error (`Dictionary_expected key)
    | Dictionary (_, m) ->
      let name_and_kind (k, v) =
        k, match v with Value _ -> `Value | Dictionary _ -> `Dictionary
      in
      Ok (List.map name_and_kind @@ M.bindings m)

  let set t key now data =
    let value = Value (now, data) in
    let rec add t' = function
      | [] -> Ok value
      | [x] ->
        begin match t' with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary (_, m) -> Ok (Dictionary (now, M.add x value m))
        end
      | hd::tl ->
        begin
          match t' with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary (mtime, m) ->
            let node = match M.find_opt hd m with
              | None -> Dictionary (now, M.empty)
              | Some t'' -> t''
            in
            add node tl >>= fun t''' ->
            let m' = M.add hd t''' m in
            Ok (Dictionary (mtime, m'))
        end
    in
    add t (Mirage_kv.Key.segments key)

  let pp fmt t =
    let rec pp_things ?(prefix = "") () fmt = function
      | Value (mtime, v) -> Fmt.pf fmt "Value %s %d (modified %a): %s@."
                              prefix (String.length v) (Ptime.pp_rfc3339 ()) mtime v
      | Dictionary (_, m) ->
        List.iter (fun (k, v) ->
            pp_things ~prefix:(prefix ^ "/" ^ k) () fmt v)
          (M.bindings m)
    in
    pp_things () fmt t

  let rec equal t t' = match t, t' with
    | Value (_, v), Value (_, v') -> String.equal v v'
    | Dictionary (_, m), Dictionary (_, m') -> M.equal equal m m'
    | _ -> false

end

module Make (CLOCK : Mirage_clock.PCLOCK) = struct
  type key = Mirage_kv.Key.t

  [@@@warning "-34"]
  type nonrec error = error
  let pp_error = pp_error

  [@@@warning "-34"]
  type nonrec write_error = write_error
  let pp_write_error = pp_write_error

  let now () = Ptime.v (CLOCK.now_d_ps ())

  let connect () = Lwt.return (ref (Pure.empty (now ()) ()))
  let disconnect _t = Lwt.return ()

  type t = Pure.t ref

  let last_modified dict key =
    Lwt.return @@ match Pure.last_modified !dict key with
    | Ok mtime -> Ok Ptime.(Span.to_d_ps (to_span mtime))
    | Error e -> Error e

  let digest dict key =
    Lwt.return @@ match Pure.get_node !dict key with
    | Ok (Value (_, data)) -> Ok (Digest.string data)
    | Ok (Dictionary (mtime, dict)) ->
      let data = Fmt.to_to_string Pure.pp (Dictionary (mtime, dict)) in
      Ok (Digest.string data)
    | Error e -> Error e

  let batch dict ?retries:_ f = f dict
  (* //cc samoht is this correct for in-memory store behaviour? *)

  let exists dict key =
    Lwt.return @@ match Pure.get_node !dict key with
    | Ok (Value _) -> Ok (Some `Value)
    | Ok (Dictionary _) -> Ok (Some `Dictionary)
    | Error (`Not_found _) -> Ok None
    | Error e -> Error e

  let get dict key = Lwt.return @@ Pure.get !dict key

  let remove dict key = Lwt.return @@ match Pure.remove !dict key (now ()) with
    | Error e -> Error e
    | Ok dict' -> dict := dict'; Ok ()

  let list dict key = Lwt.return @@ Pure.list !dict key

  let set dict key data = Lwt.return @@ match Pure.set !dict key (now ()) data with
    | Error e -> Error e
    | Ok dict' -> dict := dict'; Ok ()

  let pp fmt dict = Pure.pp fmt !dict

  let equal a b = Pure.equal !a !b
end
