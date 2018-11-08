open Common
open Build
open Artifact
open DAG

let return x v = Return (x, v)

let ( >>= ) x f = Bind (x, f)

let ( =<>= ) a b = Join (a, b)

let join l = Join_list l

let ocaml f = Action.Ocaml f

let ensures ~o b = Ensures (b, o)

let ( <-- ) o f = ensures ~o (ocaml f)

let ( ** ) a b = Pair (a, b)

let list l = List l

let artifact ?(serialize = fun _ v -> Marshal.to_string v [Marshal.Closures])
    ?(deserialize_exn = fun _ s -> Marshal.from_string s 0) id ~to_string ~hash
    ~materialize =
  Custom {id; to_string; serialize; deserialize_exn; hash; materialize}

module File = struct
  type spec = File of {path: string}

  let create path =
    let id = File {path} in
    artifact id
      ~to_string:(fun _ -> path)
      ~serialize:(fun _ _ -> "")
      ~deserialize_exn:(fun _ _ -> `File path)
      ~hash:(fun _ _ -> Digest.(file path |> to_hex))
      ~materialize:(fun _ ->
        if Sys.file_exists path then Some (`File path) else None )

  (** We close the polymorphic variant to make wrong pattern matching
      a type error instead of a warning (+ exception). *)
  let make : string -> (string -> unit) -> (_, [`File of string]) Artifact.t t
      =
   fun path f -> create path <-- fun () -> f path ; `File path

  let return path = return (create path) (`File path)

  module List = struct
    let make sl f =
      list (List.map ~f:create sl)
      <-- fun () ->
      f sl ;
      List.map sl ~f:(fun p -> `File p)

    let return sl =
      Return (list (List.map ~f:create sl), List.map sl ~f:(fun p -> `File p))
  end
end

module String_value = struct
  type spec = String of {id: string}

  let create id =
    artifact
      (String {id})
      ~to_string:(fun _ -> sprintf "${%s}" id)
      ~serialize:(fun _ v -> v)
      ~deserialize_exn:(fun _ v -> v)
      ~hash:(fun _ v -> Digest.(string v |> to_hex))
      ~materialize:(fun _ -> None)
end

let file f = File.create f

let string s = String_value.create s

let return_value (id : string) value =
  return
    (artifact id
       ~to_string:(fun _ -> id)
       ~serialize:(fun _ _ -> Marshal.to_string value [Marshal.Closures])
       ~deserialize_exn:(fun _ s -> Marshal.from_string s 0)
       ~hash:(fun _ _ ->
         Marshal.to_string value [Marshal.Closures]
         |> Digest.string |> Digest.to_hex )
       ~materialize:(fun _ -> Some value))
    value

let phony id =
  artifact id
    ~to_string:(fun _ -> id)
    ~serialize:(fun _ _ -> "")
    ~deserialize_exn:(fun _ s -> ())
    ~hash:(fun _ _ -> id)
    ~materialize:(fun _ -> None)

let return_fresh v =
  return_value Digest.(string Marshal.(to_string v [Closures]) |> to_hex) v

module System = struct
  let home () = Sys.getenv "HOME"

  let cmdf ?in_dir ?(silent = true) fmt =
    ksprintf
      (fun c ->
        let cmd =
          match in_dir with None -> c | Some d -> sprintf "cd '%s' ; %s" d c
        in
        if not silent then printf "CMD: %s\n%!" cmd ;
        match Sys.command cmd with
        | 0 -> ()
        | other ->
            ksprintf failwith "Command %S did not return 0: %d" cmd other )
      fmt

  let cmd_to_string_list cmd =
    let i = Unix.open_process_in cmd in
    let rec loop acc =
      try loop (input_line i :: acc) with _ -> close_in i ; List.rev acc
    in
    loop []

  let feed_cmd s cmd =
    let o = Unix.open_process_out cmd in
    output_string o s ; close_out o

  let write_lines p l =
    let o = open_out p in
    List.iter l ~f:(fprintf o "%s\n") ;
    close_out o

  let read_lines p =
    let o = open_in p in
    let r = ref [] in
    try
      while true do
        r := input_line o :: !r
      done ;
      assert false
    with _ -> close_in o ; List.rev !r
end

module Make_unix = struct
  let run ?state_file m =
    let open Rresult.R in
    let st =
      match state_file with
      | None -> Build.State.create []
      | Some p -> Build.State.load p
    in
    let res = Build.build st (m ()) in
    Option.iter state_file (Build.State.save st) ;
    (res, Build.State.get_log st)
end
