(** Embedded monadic-ish DSL to build dependency graphs.*)

(** Just like ["make"], ["omake"], ["dune"], ["ketrew"], etc. we are
building direct-acyclic-graphs (DAGs) of build artifacts using custom
actions (OCaml functions).

The monadic API gives Febusy fully dynamic dependencies.
    
Values in the DAG have type ['artifact DAG.t] where ['artifact] is
of type [('specification, 'representative) Artifact.t].

- ['specification] defines how to deal with the build artifacts
  (e.g. “how to tell if we need to re-build”).
- ['representative] is the type of the values that the OCaml code you
  write to build nodes in the graph can manipulate (e.g. for
  file-system files, the representative is their path).
    
See function {!artifact} to build custom artifacts.
    
An artifact has to be associated with an ['a Action.t] to be added to
the DAG (see function {!ensures}).
*)

open Build

val return : ('a, 'b) Artifact.t -> 'b -> ('a, 'b) Artifact.t DAG.t
(** Very basic building bloc of the DAG, inject a fresh dependency by
    returning an artifact definition together with a corresponding
    value. *)

val ( >>= ) :
     ('a, 'b) Artifact.t DAG.t
  -> (('a, 'b) Artifact.t -> 'b -> ('c, 'd) Artifact.t DAG.t)
  -> ('c, 'd) Artifact.t DAG.t
(** Introduce a dependency in the DAG, for instance:
    [dependency >>= fun artifact representing_value -> (* ... *)].

    But note that, in practice, most often the first argument is ignored:
    [depedency >>= fun _ v -> (* ... *)].
*)

val ( =<>= ) :
     ('a, 'b) Artifact.t DAG.t
  -> ('c, 'd) Artifact.t DAG.t
  -> ('a * 'c, 'b * 'd) Artifact.t DAG.t
(** [a =<>= b] is a node in the graph that depends on [a] and [b]. *)

val join :
  ('a, 'b) Artifact.t DAG.t list -> ('a list, 'b list) Artifact.t DAG.t
(** [join l] is a node that depends on all the nodes in [l]. *)

val ocaml : (unit -> 'a) -> 'a Action.t
(** Make an action from an OCaml function. *)

val ensures : o:('a, 'b) Artifact.t -> 'b Action.t -> ('a, 'b) Artifact.t DAG.t
(** Tie an artifact to an action that builds it, this a basic node in the DAG. *)

val ( <-- ) : ('a, 'b) Artifact.t -> (unit -> 'b) -> ('a, 'b) Artifact.t DAG.t
(** [a <-- f] is a shortcut for [ensures ~o (ocaml f)]. *)

val ( ** ) :
  ('a, 'b) Artifact.t -> ('c, 'd) Artifact.t -> ('a * 'c, 'b * 'd) Artifact.t
(** Build a “pair” compound artifact. *)

val list : ('a, 'b) Artifact.t list -> ('a list, 'b list) Artifact.t
(** Build a “list” compound artifact. *)

val artifact :
     ?serialize:('a -> 'b -> string)
  -> ?deserialize_exn:('a -> string -> 'b)
  -> 'a
  -> to_string:('a -> string)
  -> hash:('a -> 'b -> string)
  -> materialize:('a -> 'b option)
  -> ('a Artifact.custom, 'b) Artifact.t
(** Create a “root” build artifact. *)

(** The [File] module defines two particular kinds of artifact: normal
    files and lists of normal files. *)
module File : sig
  type spec = File of {path: string}

  val create : string -> (spec Artifact.custom, [> `File of string]) Artifact.t

  val make :
       string
    -> (string -> unit)
    -> (spec Artifact.custom, [`File of string]) Artifact.t DAG.t

  val return :
    string -> (spec Artifact.custom, [`File of string]) Artifact.t DAG.t

  module List : sig
    val make :
         string list
      -> (string list -> 'a)
      -> (spec Artifact.custom list, [> `File of string] list) Artifact.t DAG.t

    val return :
         string list
      -> (spec Artifact.custom list, [> `File of string] list) Artifact.t DAG.t
  end
end

(** The [String_value] module is another kind of artifact. *)
module String_value : sig
  type spec = String of {id: string}

  val create : string -> (spec Artifact.custom, string) Artifact.t
end

val file :
  string -> (File.spec Artifact.custom, [> `File of string]) Artifact.t
(** [file s] is an alias [File.create s] *)

val string : string -> (String_value.spec Artifact.custom, string) Artifact.t
(** [string s] is an alias [String_value.create s] *)

val return_value :
  string -> 'a -> (string Artifact.custom, 'a) Artifact.t DAG.t
(** [return_value id v] puts [v] in the dependency graph identified by
    [id] (in the cache and digest database). [v] needs to be
    serializable with the {!Marshal} module (and the digest is
    computed from the value too). *)

val phony : string -> (string Build.Artifact.custom, unit) Build.Artifact.t
(** Artifact that is built “once-per-run.” *)

val return_fresh :
  'a -> (string Build.Artifact.custom, 'a) Build.Artifact.t Build.DAG.t
(** Return a value which is built “once-per-run” (has to be
    [Marshal]-serializable). *)

(** Useful functions to deal with the operating system. *)
module System : sig
  val home : unit -> string
  (** The value of ["$HOME"]. *)

  val cmdf :
    ?in_dir:string -> ?silent:bool -> ('a, unit, string, unit) format4 -> 'a
  (** Run a shell command (wrapper around [Sys.command] which throws
      an exception when the return value is non-zero). *)

  val cmd_to_string_list : string -> string list
  (** Run a shell command and get the output as a list of strings. *)

  val feed_cmd : string -> string -> unit
  (** Feed a string as [stdin] of a shell command. *)

  val write_lines : string -> string list -> unit
  (** Write lines into a given file path. *)

  val read_lines : string -> string list
  (** Read a file as a list of strings. *)
end

(** Run the build DAG in a mono-threaded Unix-ish environment. *)
module Make_unix : sig
  val run :
       ?state_file:string
    -> (unit -> ('a, 'b) Build.Artifact.t Build.DAG.t)
    -> (('a, 'b) Build.build_status, Common.Error.t) result * string
end
