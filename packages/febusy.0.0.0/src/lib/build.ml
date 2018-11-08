open Common

module Artifact = struct
  type 'a custom = Custom of 'a

  type ('kind, 'how_made) t =
    | Custom:
        { id: 'a
        ; to_string: 'a -> string
        ; serialize: 'a -> 'b -> string
        ; deserialize_exn: 'a -> string -> 'b
        ; hash: 'a -> 'b -> string
        ; materialize: 'a -> 'b option }
        -> ('a custom, 'b) t
    | Pair: ('a, 'c) t * ('b, 'd) t -> ('a * 'b, 'c * 'd) t
    | List: ('a, 'c) t list -> ('a list, 'c list) t

  let rec to_string : type a b. (a, b) t -> string = function
    | Custom {id; to_string} -> to_string id
    | Pair (a, b) -> sprintf "(%s, %s)" (to_string a) (to_string b)
    | List l ->
        sprintf "[%s]" (List.map ~f:to_string l |> String.concat ~sep:"; ")

  let rec deserialize_exn : type a b. (a, b) t -> string -> b =
   fun a s ->
    match a with
    | Custom {id; deserialize_exn} -> deserialize_exn id s
    | Pair (a, b) ->
        let (astr, bstr) : string * string = Marshal.from_string s 0 in
        (deserialize_exn a astr, deserialize_exn b bstr)
    | List l ->
        let sl : string list = Marshal.from_string s 0 in
        List.map2 ~f:deserialize_exn l sl

  let rec serialize : type a b. (a, b) t -> b -> string =
   fun a b ->
    match (a, b) with
    | Custom {id; serialize}, b -> serialize id b
    | Pair (a, b), (sa, sb) ->
        Marshal.to_string (serialize a sa, serialize b sb) []
    | List l, sl -> Marshal.to_string (List.map2 ~f:serialize l sl) []

  let rec hash : type a b. (a, b) t -> b -> string =
   fun a b ->
    match (a, b) with
    | Custom {id; hash}, b -> hash id b
    | Pair (a, b), (sa, sb) -> Digest.(string (hash a sa ^ hash b sb) |> to_hex)
    | List l, sl ->
        let open Digest in
        string (List.map2 ~f:hash l sl |> String.concat ~sep:"") |> to_hex

  let rec materialize : type a b. (a, b) t -> b option =
   fun a ->
    match a with
    | Custom {id; materialize} -> materialize id
    | Pair (a, b) ->
        let open Option in
        materialize a >>= fun ma -> materialize b >>= fun mb -> return (ma, mb)
    | List l ->
        List.fold ~init:(Some []) l ~f:(fun prev m ->
            match prev with
            | None -> None
            | Some l ->
              match materialize m with None -> None | Some v -> Some (v :: l)
        )
        |> Option.map ~f:List.rev
end

module Action = struct
  type _ t = Ocaml: (unit -> 'a) -> 'a t

  let rec run : type a. a t -> a = function
    | Ocaml f ->
        let res = f () in
        res

  let rec to_string : type a. a t -> string = function
    | Ocaml f -> sprintf "OCAML"
end

module DAG = struct
  type _ t =
    | Return: ('a, 'b) Artifact.t * 'b -> ('a, 'b) Artifact.t t
    | Bind:
        ('a, 'b) Artifact.t t
        * (('a, 'b) Artifact.t -> 'b -> ('c, 'd) Artifact.t t)
        -> ('c, 'd) Artifact.t t
    | Join:
        ('a, 'b) Artifact.t t * ('c, 'd) Artifact.t t
        -> ('a * 'c, 'b * 'd) Artifact.t t
    | Join_list: ('a, 'b) Artifact.t t list -> ('a list, 'b list) Artifact.t t
    | Ensures: 'a Action.t * ('b, 'a) Artifact.t -> ('b, 'a) Artifact.t t
end

type ('a, 'b) build_status =
  {artifact: ('a, 'b) Artifact.t; value: 'b; done_something: bool}

module Database = struct
  type t =
    { mutable cache: (string * string) list
    ; mutable hashes: (string * string) list }

  let create () = {cache= []; hashes= []}
end

module State = struct
  type hold_artifact = Artifact: ('a, 'b) build_status -> hold_artifact

  type t =
    { depth: int
    ; stack: hold_artifact list
    ; database: Database.t
    ; log: string list ref
    ; previous_hashes: (string * string) list }

  let incr w = {w with depth= w.depth + 2}

  let push st art = {st with stack= Artifact art :: st.stack}

  let create previous_hashes =
    { depth= 0
    ; stack= []
    ; database= Database.create ()
    ; previous_hashes
    ; log= ref [] }

  let load f =
    let prev =
      try
        let i = open_in f in
        let v : (string * string) list = Marshal.from_channel i in
        close_in i ; v
      with _ -> []
    in
    create prev

  let all_hashes s = s.database.hashes

  let save st f =
    let o = open_out f in
    Marshal.to_channel o (all_hashes st) [] ;
    close_out o ;
    ()

  let ps ?(with_stack= false) st fmt =
    let indent = String.make st.depth ' ' in
    ksprintf
      (fun s ->
        st.log :=
          !(st.log)
          @ [ sprintf "%s* %s\n%s%!" indent
                ( String.split ~on:(`Character '\n') s
                |> String.concat ~sep:(sprintf "\n%s" indent) )
                ( if with_stack then
                  sprintf "%s\\[%s]\n%!" indent
                    ( List.map st.stack ~f:(function Artifact bs ->
                          Artifact.to_string bs.artifact )
                    |> String.concat ~sep:", " )
                else "" ) ] )
      fmt

  let get_log st = String.concat ~sep:"" !(st.log)

  let lookup_cache st key = List.Assoc.get key st.database.cache

  type 'a looked_up = [`In_cache of 'a | `Not_in_cache of 'a | `Not_at_all]

  let lookup : type a b. t -> (a, b) Artifact.t -> b looked_up =
   fun st arti ->
    match
      ( Artifact.materialize arti
      , lookup_cache st (Artifact.to_string arti)
        |> Option.map ~f:(Artifact.deserialize_exn arti) )
    with
    | Some v, Some _ -> `In_cache v
    | None, Some v -> `In_cache v
    | Some v, None -> `Not_in_cache v
    | None, None -> `Not_at_all

  (* lookup_cache st (Artifact.to_string arti)
         * |> Option.map ~f:(Artifact.deserialize_exn arti) *)

  let get_hash st arti value =
    let key = Artifact.to_string arti in
    match List.Assoc.get key st.database.hashes with
    | Some s -> s
    | None ->
        let h = Artifact.hash arti value in
        (st.database).hashes
        <- (key, h) :: List.Assoc.remove_assoc key st.database.hashes ;
        h

  let update_cache st arti value =
    let key = Artifact.to_string arti in
    let _ = get_hash st arti value in
    (st.database).cache
    <- (key, Artifact.serialize arti value)
       :: List.Assoc.remove_assoc key st.database.cache

  let dependencies_changed st =
    List.fold st.stack ~init:`No ~f:(fun prev -> function
      | Artifact bs ->
          let new_hash = get_hash st bs.artifact bs.value in
          match
            ( prev
            , List.Assoc.get
                (Artifact.to_string bs.artifact)
                st.previous_hashes )
          with
          | `No, None -> `No_hash (Artifact.to_string bs.artifact)
          | `No, Some old_hash ->
              if new_hash = old_hash then `No
              else `Hash_changed (Artifact.to_string bs.artifact)
          | other, _ -> other )
end

let ook artifact value done_something = Ok {artifact; value; done_something}

let rec build : type a b.
    _ -> (a, b) Artifact.t DAG.t -> ((a, b) build_status, Error.t) result =
  let open Rresult.R in
  let open State in
  let open DAG in
  fun state -> function
    | Return (a, v) -> (
        ps state "return %s" (Artifact.to_string a) ;
        try
          State.update_cache state a v ;
          ook a v false
        with e ->
          Error.exn e "Artifact returned but error: %s" (Artifact.to_string a)
        )
    | Bind (t, f) ->
        ps state "bind" ;
        build (incr state) t
        >>= fun bs ->
        (* ps state "built %s" (Artifact.to_string bs.v) ; *)
        build (push state bs |> incr) (f bs.artifact bs.value)
    | Join (a, b) ->
        ps state "Join" ;
        build (incr state) a
        >>= fun aa ->
        build (incr state) b
        >>= fun bb ->
        ook
          (Artifact.Pair (aa.artifact, bb.artifact))
          (aa.value, bb.value)
          (aa.done_something || bb.done_something)
    | Join_list al ->
        List.fold al ~init:(ook (Artifact.List []) [] false) ~f:(fun prev a ->
            prev
            >>= fun {artifact= Artifact.List l; value; done_something} ->
            build (incr state) a
            >>= fun aa ->
            return
              { artifact= Artifact.List (l @ [aa.artifact])
              ; value= value @ [aa.value]
              ; done_something= aa.done_something || done_something } )
    | Ensures (m, a) ->
        (*
             For artifact a:
             - if not exists (in cache) -> build
             - if any dependency changed Vs DB -> build
             - otherwise return cached
             - then update cache
          *)
        let log = ref [] in
        let logf fmt = ksprintf (fun s -> log := s :: !log) fmt in
        let actually_run () =
          logf "actually_run" ;
          try
            let b = Action.run m in
            State.update_cache state a b ;
            logf "success" ;
            ook a b true
          with e ->
            logf "error" ;
            Error.exn e "Ensuring %s" (Artifact.to_string a)
        in
        ( match State.lookup state a with
        | `In_cache b -> logf "in-cache" ; ook a b false
        | `Not_in_cache b -> (
            logf "not-in-cache" ;
            match State.dependencies_changed state with
            | `No_hash s -> logf "no-hash:%s" s ; actually_run ()
            | `Hash_changed s -> logf "hash-changed:%s" s ; actually_run ()
            | `No -> logf "no-dep-changed" ; ook a b false )
        | `Not_at_all -> logf "does-not-exist" ; actually_run () )
        >>= fun res ->
        ps ~with_stack:true state "to-ensure %s {%s}\n| %s"
          (Artifact.to_string a) (Action.to_string m)
          (List.rev_map !log ~f:(sprintf "%s") |> String.concat ~sep:" → ") ;
        Ok res
