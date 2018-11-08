open Result
open Sturgeon_sexp
open Sturgeon_session
type session = t
open Inuit

type simple_flag = [ `Clickable | `Clicked | `Editable | `Prompt | `Focus ]
type flag = [ simple_flag | `Custom of (string * Sturgeon_sexp.basic) ]

let dump_sexp sexp =
  let inj _ = S "<abstract>" and map x = x in
  to_string (transform_cons ~inj ~map sexp)

let sexp_of_revision {Remote. remote; local} =
  C (I remote, I local)

let revision_of_sexp = function
  | C (I remote, I local) -> {Remote. remote; local}
  | sexp -> failwith ("revision_of_sexp: cannot parse " ^ dump_sexp sexp)

let symbol_of_flag = function
 | `Clickable -> "clickable"
 | `Editable  -> "editable"
 | `Prompt    -> "prompt"
 | `Focus     -> "focus"
 | `Clicked   -> "clicked"

let sexp_of_flags (flags : flag list) =
  let customs = ref sym_nil in
  let rec aux acc = function
    | [] -> acc
    | #simple_flag as x :: xs -> aux (C (S (symbol_of_flag x), acc)) xs
    | `Custom (key, value) :: xs ->
      customs := C (S key, C (transform_cons ~inj:void value, !customs));
      aux acc xs
  in
  let acc = aux (S "nil") flags in
  if !customs = sym_nil then acc else C (C (S "custom", !customs), acc)

let flags_of_sexp sexp =
  let rec aux acc = function
    | C (S "clickable", xs) -> aux (`Clickable :: acc) xs
    | C (S "clicked", xs)   -> aux (`Clicked :: acc) xs
    | C (S "editable", xs)  -> aux (`Editable :: acc) xs
    | C (S "prompt", xs)    -> aux (`Prompt :: acc) xs
    | C (S "focus", xs)     -> aux (`Focus :: acc) xs
    | C (C (S "custom", x), xs) ->
      let x = transform_cons ~inj:(fun remote ->
          begin try
              let (Once cont | Many cont) = remote in
              cont (Error `Cancel)
            with _ -> ()
          end;
          prerr_endline "sturgeon: invalid session, continuation in flags";
          sym_nil)
          ~map:(fun x -> x)
          x
      in
      let rec custom = function
        | S "nil" -> acc
        | C (C ((S key | T key), value), xs) ->
          `Custom (key, value) :: custom xs
        | C (x, xs) ->
          prerr_endline ("Incorrect custom flag: " ^ dump_sexp x);
          custom xs
        | xs ->
          prerr_endline ("Incorrect custom flags: " ^ dump_sexp xs);
          acc
      in
      aux (custom x) xs
    | S "nil" -> acc
    | C (sexp, xs) -> prerr_endline ("Unknown flags: " ^ dump_sexp sexp);
      aux acc xs
    | sexp -> prerr_endline ("Incorrect flags: " ^ dump_sexp sexp); acc
  in
  aux [] sexp

let sexp_of_operation text_len = function
  | Patch.Propertize n ->
    C (S "propertize", I n)
  | Patch.Remove n ->
    C (S "remove", I n)
  | Patch.Insert text ->
    let text = C (T text, I text_len) in
    C (S "insert", text)
  | Patch.Replace (n, text) ->
    let text = C (T text, I text_len) in
    C (S "replace", C (I n, text))

let sexp_of_remote_patch = function
  | Remote.Patch (rev, {Patch. offset; operation; text_len; flags}) ->
    let operation = sexp_of_operation text_len operation in
    let patch = C (I offset, C (operation, C (sexp_of_flags flags, sym_nil))) in
    C (S "patch", C (sexp_of_revision rev, patch))
  | Remote.Ack rev ->
    C (S "ack", C (sexp_of_revision rev, sym_nil))

let operation_of_sexp = function
  | C (S "propertize", I n) -> (0, Patch.Propertize n)
  | C (S "remove", I n) -> (0, Patch.Remove n)
  | C (S "insert", C (T text, I text_len)) ->
      (text_len, Patch.Insert text)
  | C (S "replace", C (I n, C (T text, I text_len))) ->
      (text_len, Patch.Replace (n, text))
  | sexp -> failwith ("operation_of_sexp: cannot parse " ^ dump_sexp sexp)

let remote_patch_of_sexp = function
  | C (S "ack", C (revision, S "nil")) ->
    Remote.Ack (revision_of_sexp revision)
  | C (S "patch",
       C (revision, C (I offset, C (operation, C (flags, S "nil"))))) ->
    let _text_len, operation = operation_of_sexp operation in
    let patch = Patch.make ~offset (flags_of_sexp flags) operation in
    (* FIXME: check _text_len = patch.Patch.text_len *)
    Remote.Patch ((revision_of_sexp revision), patch)
  | sexp -> failwith ("remote_patch_of_sexp: cannot parse " ^ dump_sexp sexp)

(** Buffer management *)

type aux_command =
  [ `Split of [`Left|`Right|`Top|`Bottom] * string * Sturgeon_session.t
  | `Fit ]

type buffer = {
  command: aux_command Socket.controller;
  patches: flag patch socket;
}

let sexp_of_buffer_command : aux_command -> Sturgeon_session.t = function
  | `Split (dir, name, session) ->
    let dir = match dir with
      | `Left -> "left" | `Right -> "right"
      | `Top -> "top" | `Bottom -> "bottom"
    in
    sexp_of_list [S "split"; S dir; T name; session]
  | `Fit -> C (S "fit", sym_nil)

(** *)

let remote_buffer () =
  let queue = ref [] in
  let patch_socket =
    Socket.make ~receive:(fun x -> queue := sexp_of_remote_patch x :: !queue)
  and command_socket =
    Socket.make ~receive:(fun x -> queue := sexp_of_buffer_command x :: !queue)
  in
  let handler = function
    | Ok (C (S "many", M (Many cont))) ->
      let rec aux = function
        | x :: xs -> cont (Ok x); aux xs
        | [] ->
          let xs = !queue in
          queue := [];
          if xs <> [] then aux (List.rev xs)
      in
      aux [];
      Socket.set_receive patch_socket
        (fun msg -> cont (Ok (sexp_of_remote_patch msg)));
      Socket.set_receive command_socket
        (fun msg -> cont (Ok (sexp_of_buffer_command msg)))
    | Ok sexp ->
      Socket.send patch_socket (remote_patch_of_sexp sexp)
    | Error _ ->
      Socket.close patch_socket;
      Socket.close command_socket
  in
  let command = Socket.make ~receive:ignore in
  let remote, patches = Remote.make () in
  Socket.connect
    ~a:(Socket.endpoint command_socket)
    ~b:(Socket.endpoint command);
  Socket.connect ~a:remote ~b:(Socket.endpoint patch_socket);
  ({ command; patches; },
   M (Many handler))

type shell_status =
  | Pending of session list
  | Connected of session cont

type shell = {
  mutable status : shell_status;
}

let buffer_greetings () =
  let shell = { status = (Pending []) } in
  let session = sexp_of_list [S "buffer-shell"; M (Once (function
      | Ok (M (Many t)) ->
        begin match shell.status with
          | Connected _ ->
            failwith "Stui.buffer_greetings: invalid session, already connected"
          | Pending xs ->
            shell.status <- Connected t;
            List.iter (fun x -> t (Ok x)) (List.rev xs)
        end
      | _ -> failwith "Stui.buffer_greetings: invalid session, unknown command"
    ))]
  in
  (session, shell)

let send shell command =
  let command = sexp_of_list command in
  match shell.status with
  | Connected cont -> cont (Ok command)
  | Pending xs -> shell.status <- Pending (command :: xs)

let create_buffer shell ~name =
  let buffer, session = remote_buffer () in
  send shell [S "create-buffer"; T name; session];
  buffer

let message shell text =
  send shell [S "message"; T text]

type 'a menu = string * [ `Item of 'a | `Sub of 'a menu list ]

let popup_menu shell title items action =
  let next_index = ref 0 in
  let payloads = ref [] in
  let rec aux (title, entry) = match entry with
    | `Item value ->
      payloads := value :: !payloads;
      let index = !next_index in
      incr next_index;
      V [T title; I index]
    | `Sub entries ->
      let entries = List.map aux entries in
      C (T title, sexp_of_list entries)
  in
  let items = List.map aux items in
  let values = Array.of_list (List.rev !payloads) in
  let action = Once (function
      | Ok (I n) -> action (Ok values.(n))
      | Ok sexp ->
        Sturgeon_session.cancel sexp;
        action (Error (`Other (T "Invalid value (incorrect protocol)")))
      | Error err -> action (Error err)
    )
  in
  send shell [S "popup-menu"; T title; sexp_of_list items; M action]

let read_file_name shell ~prompt ?dir ?default action =
  let maybe_T = function
    | None -> sym_nil
    | Some text -> T text
  in
  let action = Once (function
      | Ok (T name) -> action (Ok name)
      | Ok sexp ->
        Sturgeon_session.cancel sexp;
        action (Error (`Other (T "Invalid value (incorrect protocol)")))
      | Error err -> action (Error err)
    )
  in
  send shell
    [S "read-file-name"; T prompt; maybe_T dir; maybe_T default; M action]

let manual_connect buffer socket =
  Socket.connect ~a:buffer.patches ~b:socket

let open_cursor buffer =
  let cursor, pipe = Inuit.Cursor.make () in
  manual_connect buffer pipe;
  cursor

let create_cursor shell ~name =
  open_cursor (create_buffer shell ~name)

let fit_to_window buffer =
  Socket.send buffer.command `Fit

let split buffer ~name dir =
  let buffer', session = remote_buffer () in
  Socket.send buffer.command (`Split (dir, name, session));
  buffer'
