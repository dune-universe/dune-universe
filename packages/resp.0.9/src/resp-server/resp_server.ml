module type AUTH = sig
  type t

  val check : t -> string array -> bool
end

module type SERVER = sig
  type ic
  type oc
  type server
  type data

  val run : server -> (ic * oc -> unit Lwt.t) -> unit Lwt.t
end

module Auth = struct
  module String = struct
    type t = string

    let check auth args = Array.length args > 0 && args.(0) = auth
  end

  module User = struct
    type t = (string, string) Hashtbl.t

    let check auth args =
      if Array.length args < 2 then false
      else
        match Hashtbl.find_opt auth args.(0) with
        | Some p ->
          p = args.(1)
        | None ->
          false
  end
end

module type S = sig
  include SERVER
  module Value : Resp.S with type Reader.ic = ic and type Writer.oc = oc
  module Auth : AUTH

  type client = ic * oc
  type command = data -> client -> string -> int -> unit Lwt.t
  type t

  val discard_n : client -> int -> unit Lwt.t
  val finish : client -> nargs:int -> int -> unit Lwt.t
  val ok : client -> unit Lwt.t
  val error : client -> string -> unit Lwt.t
  val invalid_arguments : client -> unit Lwt.t
  val send : client -> Resp.t -> unit Lwt.t
  val recv : client -> Resp.t Lwt.t

  val create :
       ?auth:Auth.t
    -> ?commands:(string * command) list
    -> ?default:string
    -> server
    -> data
    -> t

  val start : t -> unit Lwt.t
end

module Make
    (Server : SERVER)
    (Auth : AUTH)
    (Value : Resp.S
             with type Reader.ic = Server.ic
              and type Writer.oc = Server.oc) =
struct
  include Server
  module Value = Value
  module Auth = Auth

  let ( >>= ) = Lwt.( >>= )

  type client = ic * oc
  type command = data -> client -> string -> int -> unit Lwt.t

  type t =
    { server : server
    ; data : data
    ; auth : Auth.t option
    ; commands : (string, command) Hashtbl.t
    ; default : string }

  let ok (_, oc) = Value.write oc (`String "OK")
  let error (_, oc) msg = Value.write oc (`Error (Printf.sprintf "ERR %s" msg))
  let invalid_arguments client = error client "Invalid arguments"
  let send (_, oc) x = Value.write oc x
  let recv (ic, _) = Value.read ic

  let hashtbl_of_list l =
    let ht = Hashtbl.create (List.length l) in
    List.iter (fun (k, v) -> Hashtbl.replace ht (String.lowercase_ascii k) v) l;
    ht

  let create ?auth ?(commands = []) ?(default = "default") server data =
    let commands = hashtbl_of_list commands in
    {server; data; auth; commands; default}

  let check_auth auth args =
    match auth with
    | Some auth ->
      Auth.check auth args
    | None ->
      true

  let split_command_s arr : string * string array =
    ( String.lowercase_ascii @@ Resp.to_string_exn arr.(0)
    , Array.map Resp.to_string_exn (Array.sub arr 1 (Array.length arr - 1)) )

  let rec discard_n client n =
    if n > 0 then Value.read (fst client) >>= fun _ -> discard_n client (n - 1)
    else Lwt.return ()

  let finish client ~nargs used = discard_n client (nargs - used)

  let rec handle t client authenticated =
    let argc = ref 0 in
    Lwt.catch
      (fun () ->
        if not authenticated then handle_not_authenticated t client
        else
          Value.Reader.read_lexeme (fst client)
          >>= function
          | Ok (`As n) -> (
            argc := n - 1;
            Value.read (fst client)
            >>= function
            | `String s
            | `Bulk s ->
              let s = String.lowercase_ascii s in
              let f =
                try Hashtbl.find t.commands s with Not_found ->
                  Hashtbl.find t.commands t.default
              in
              f t.data client s !argc >>= fun () -> handle t client true
            | _ ->
              discard_n client !argc
              >>= fun () ->
              error client "invalid commands name"
              >>= fun () -> handle t client true )
          | Error e ->
            error client (Resp.string_of_error e)
            >>= fun () -> handle t client true
          | _ ->
            error client "invalid command format"
            >>= fun () -> handle t client true )
      (function
        | Resp.Exc exc ->
          error client (Resp.string_of_error exc)
          >>= fun () -> handle t client true
        | Not_found ->
          discard_n client !argc
          >>= fun () ->
          error client "command not found" >>= fun () -> handle t client true
        | Failure msg
        | Invalid_argument msg ->
          error client msg
        | End_of_file ->
          Lwt.return ()
        | exc ->
          raise exc)

  and handle_not_authenticated t client =
    Value.read (fst client)
    >>= function
    | `Array arr -> (
      let cmd, args = split_command_s arr in
      match (cmd, args) with
      | "auth", args ->
        if check_auth t.auth args then
          ok client >>= fun () -> handle t client true
        else
          error client "authentication required"
          >>= fun () -> handle t client false
      | _, _ ->
        error client "authentication required"
        >>= fun () -> handle t client false )
    | _ ->
      error client "authentication required"
      >>= fun () -> handle t client false

  let start t = run t.server (fun client -> handle t client (t.auth = None))
end
