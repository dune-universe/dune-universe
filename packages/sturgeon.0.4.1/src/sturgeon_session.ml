open Result
open Sturgeon_sexp

type reason = [ `Cancel | `Finalize | `Other of basic ]

type 'a cont = ('a, reason) result -> unit

type remote =
  | Once of t cont
  | Many of t cont

and t = remote sexp

let sexp_of_reason = function
  | `Cancel -> S "cancel"
  | `Finalize -> S "finalize"
  | `Other sexp -> sexp

let reason_of_sexp : basic -> reason = function
  | S "cancel" -> `Cancel
  | S "finalize" -> `Finalize
  | sexp -> `Other sexp

let add_finalizer (remote : t cont) addr =
  let finalize _addr = remote (Error `Finalize) in
  Gc.finalise finalize addr

(* Cancel : abort computation, release ressources  *)
let lower_cancel ?stderr (t : t) : basic =
  let exns = ref [] in
  let map : basic -> basic = function
    | C (S "meta", x) -> C (S "meta", C (S "escape", x))
    | x -> x
  and inj (remote : remote) : basic =
    begin
      let (Once t | Many t) = remote in
      try t (Error `Cancel)
      with exn -> exns := exn :: !exns
    end;
    match remote with
    | Once _ -> C (S "meta", C (S "once", S "cancelled"))
    | Many _ -> C (S "meta", C (S "many", S "cancelled"))
  in
  let result = transform_cons ~inj ~map t in
  if !exns <> [] then
    (match stderr with
     | Some f -> f (`Exceptions_during_cancellation (t, !exns))
     | None -> ());
  result

let cancel ?stderr (t : t) =
  let exns = ref [] in
  let rec aux = function
    | C (a, b) -> aux a; aux b
    | P t -> aux t
    | S _ | T _ -> ()
    | I _ | F _ -> ()
    | V xs -> List.iter aux xs
    | M (Once t | Many t) ->
      try t (Error `Cancel)
      with exn -> exns := exn :: !exns
  in
  let result = aux t in
  if !exns <> [] then
    (match stderr with
     | Some f -> f (`Exceptions_during_cancellation (t, !exns))
     | None -> ());
  result

type 'a error =
  [ `Already_closed  of (t, reason) result
  | `Query_after_eof of t
  | `Invalid_command of basic
  | `Feed_unknown    of basic
  | `Quit_unknown    of basic
  | `Exceptions_during_cancellation of t * exn list
  | `Exceptions_during_shutdown of exn list
  ]

type status = {
  mutable state: [`Greetings | `Main | `Closed];
  mutable gensym: int;
  table: (int, remote) Hashtbl.t;
}

type output = basic -> unit

let gensym status =
  status.gensym <- status.gensym + 1;
  status.gensym

let connect
    ?(greetings=sym_nil) ?cogreetings
    ?(stderr : ('a error -> unit) option)
    stdout
  : output * status
  =

  let status = {
    state = `Greetings;
    gensym = 0;
    table = Hashtbl.create 7;
  } in

  (* Lower: turn closures into ground sexp *)

  let lower (t : t) : basic =
    let map : basic -> basic = function
      | C (S "meta", x) -> C (S "meta", C (S "escape", x))
      | x -> x
    and inj (remote : remote) : basic =
      let addr = gensym status in
      Hashtbl.add status.table addr remote;
      let sym = match remote with
        | Once _ -> S "once"
        | Many _ -> S "many"
      in
      C (S "meta", C (sym, I addr))
    in
    transform_cons ~inj ~map t
  in

  (* Upper: inject closures into ground sexp *)

  let upper (t : basic) : t =
    let map : t -> t = function
      | C (S "meta", C (S "escape", x)) ->
        C (S "meta", x)
      | C (S "meta", C (S ("once" | "many" as kind), addr)) ->
        let addr = lower_cancel ?stderr addr in
        let is_once = kind = "once" in
        let closed = ref false in
        let lift_remote msg =
          if status.state = `Closed then
            match msg with
            | Ok x -> cancel ?stderr x
            | Error _ -> ()
          else if !closed then
            if msg = Error `Finalize then ()
            else begin
              begin match msg with
                | Ok x -> cancel ?stderr x
                | Error _ -> ()
              end;
              match stderr with
              | Some f -> f (`Already_closed msg)
              | None -> ()
            end
          else match msg with
            | Ok x ->
              closed := is_once;
              stdout (C (S "feed", C (addr, lower x)))
            | Error x ->
              closed := true;
              stdout (C (S "quit", C (addr, sexp_of_reason x)))
        in
        add_finalizer lift_remote addr;
        M (if is_once then Once lift_remote else Many lift_remote)
      | x -> x
    and inj : void -> t = void
    in
    transform_cons ~inj ~map t
  in

  let get_addr = function
    | I addr -> addr, Hashtbl.find status.table addr
    | _ -> raise Not_found
  in

  let remote (cmd : basic) =
    match status.state with
    | `Closed -> cancel ?stderr (upper cmd)
    | `Greetings ->
      begin match cmd with
        | C (S "greetings", C (I 1, payload)) ->
          status.state <- `Main;
          begin match cogreetings with
            | Some f -> f (upper payload)
            | None -> cancel ?stderr (upper payload)
          end
        | _ -> cancel ?stderr (upper cmd)
      end
    | `Main -> match cmd with
      | C (S "feed", C (addr, payload)) as msg ->
        let x = upper payload in
        begin match get_addr addr with
          | addr, Once t ->
            Hashtbl.remove status.table addr;
            t (Ok x)
          | _, Many t -> t (Ok x)
          | exception Not_found ->
            cancel ?stderr x;
            begin match stderr with
              | Some f -> f (`Feed_unknown msg)
              | None -> ()
            end
        end

      | C (S "quit", C (addr, x)) as msg ->
        begin match get_addr addr with
          | addr, (Once t | Many t) ->
            Hashtbl.remove status.table addr;
            t (Error (reason_of_sexp x))
          | exception Not_found ->
            begin match stderr with
              | Some f -> f (`Quit_unknown msg)
              | None -> ()
            end
        end

      | S "end" ->
        status.state <- `Closed;
        let exns = ref [] in
        Hashtbl.iter (fun _ (Many t | Once t) ->
            try t (Error `Cancel)
            with exn -> exns := exn :: !exns
          ) status.table;
        Hashtbl.reset status.table;
        begin try stdout (S "end")
          with exn -> exns := exn :: !exns
        end;
        if !exns <> [] then
          begin match stderr with
            | Some f -> f (`Exceptions_during_shutdown !exns)
            | None -> ()
          end

      | cmd ->
        cancel ?stderr (upper cmd);
        begin match stderr with
          | Some f ->
            f (`Invalid_command cmd)
          | None -> ()
        end
  in
  stdout (lower (C (S "greetings", C (I 1, greetings))));
  remote, status

let close remote = remote (S "end")

let pending_continuations status =
  Hashtbl.length status.table

let is_closed status = status.state = `Closed
