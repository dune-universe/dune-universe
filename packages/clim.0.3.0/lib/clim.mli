(** Command Line Interface Maker *)

(** Converter type from [Cmdliner]. *)
type 'a conv = 'a Cmdliner.Arg.conv

(** Type flag meaning the argument isn't fully specified. *)
type spec

(** Type flag meaning the argument is fully specified. *)
type final

(** The argument type. *)
type ('a, 'b) arg

(** {3 Flags}

    Flags are boolean values which don't need value: only their presence ([true])
    or not ([false]) give their value. *)

(** [flag ~docs ~doc ~env aka] defines a new flag whose names are given in [aka]. *)
val flag : ?docs:string -> ?doc:string -> ?env:string -> string list ->
  (bool, spec) arg

(** {3 Optional Arguments}

    Optional arguments are the usual argument specified by a [-x] or [--xxx]
    notation on command line. *)

(** [opt ~docs ~docv ~doc ~env ~vopt conv default aka]  defines a new
    optional argument whose names are given in [aka]. *)
val opt : ?docs:string -> ?docv:string -> ?doc:string -> ?env:string ->
  ?vopt:'a -> 'a conv -> 'a -> string list ->
  ('a, spec) arg

(** [opt_all ~docs ~docv ~doc ~env ~vopt conv default aka] same as
    {!opt} but gathers several invocations of the options into a list.
    For example, this is useful for include directory options used in
    many tools. *)
val opt_all : ?docs:string -> ?docv:string -> ?doc:string -> ?env:string ->
  ?vopt:'a -> 'a conv -> 'a list -> string list ->
  ('a list, spec) arg


(** {3 Positional Arguments}

    Positional arguments have no name and are supposely mandatory and
    ordered on command line.

    For now, positional arguments combinators are somehow illformed
    and will be modified in near future so use it carefuly. *)

val pos : ?docs:string -> ?docv:string -> ?doc:string -> ?env:string ->
  ?rev:bool -> 'a conv -> 'a -> int ->
  ('a, spec) arg

val pos_all : ?docs:string -> ?docv:string -> ?doc:string -> ?env:string ->
  'a conv -> 'a list ->
  ('a list, spec) arg

(** {3 Contraints}

    To finalize an argument, the following constraints can be used. *)

(** [value a] finalizes the argument [a] as itself. *)
val value : ('a, spec) arg -> ('a, final) arg

(** [required a] ensures that argument [a] holding a optional value
    is given on command line. *)
val required : ('a option, spec) arg -> ('a, final) arg

(** [non_empty a] ensures that argument [a] holding a list is not
    empty. *)
val non_empty : ('a list, spec) arg -> ('a list, final) arg

(** [last a] ensures that only the last given value on command
    line will be used for [a]. *)
val last : ('a list, spec) arg -> ('a, final) arg

(** {2 Configurations}

    Configurations hold the command line argument to be used. *)

(** Configuration type. *)
type cfg

(** Creates an empty configuration. *)
val create : unit -> cfg

(** [from cfg] creates a new configuration based on [cfg]. *)
val from : cfg -> cfg

(** [register cfg arg] registers the argument [arg] info [cfg] and
    returns a getter to the option's value. *)
val register : cfg -> ('a, final) arg -> unit -> 'a

(** {2 Commands}

    Since commands can have multiple related environment variable,
    we inherit their definitions from [Cmdliner]. *)

(** Environment variable specification type. *)
type env = Cmdliner.Term.env_info

(** [env ~docs ~doc name] creates a ne environment variable specification
    named [name]. *)
val env : ?docs:string -> ?doc:string -> string -> env

(** The command type. It gathers all [Cmdliner] related informations into
    one simle data structure. *)
type 'a command = {
  cfg : cfg; (** The related configuration. *)
  cmd : (unit -> 'a); (** The command entrypoint. *)
  man_xrefs : Cmdliner.Manpage.xref list; (** Man references. *)
  man : Cmdliner.Manpage.block list; (** Additional man documentation. *)
  envs : env list; (** Environment variables. *)
  doc : string; (** Main documentation. *)
  version : string option; (** Version (if any). *)
  name : string; (** Binary name. *)
}

(** Command maker. *)
val command :
  ?cfg:cfg ->
  ?man_xrefs:Cmdliner.Manpage.xref list ->
  ?man:Cmdliner.Manpage.block list ->
  ?envs:env list ->
  ?doc:string ->
  ?version:string option ->
  ?name:string ->
  (unit -> 'b) ->
  'b command

(** [term cmd] returns the [Cmdliner] terms and info needed to use [Cmdliner].
    This ensures some compatiblity with [Cmdliner] if defining a legacy [Cmdliner]
    command term. *)
val term : 'a command -> ('a Cmdliner.Term.t * Cmdliner.Term.info)

(** [run cmd] parses the command line according to the [cmd] specification and
    returns the result of the underlying function. *)
val run : 'a command -> 'a


(** {2 Converters}

    The following converters are inherited from [Cmdliner]. *)
val bool : bool conv
val char : char conv
val int : int conv
val nativeint : nativeint conv
val int32 : int32 conv
val int64 : int64 conv
val float : float conv
val string : string conv
val enum : (string * 'a) list -> 'a conv
val file : string conv
val dir : string conv
val non_dir_file : string conv
val option : ?none:string -> 'a conv -> 'a option conv
val list : ?sep:char -> 'a conv -> 'a list conv
val array : ?sep:char -> 'a conv -> 'a array conv
val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t2 : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t3 : ?sep:char -> 'a conv -> 'b conv -> 'c conv -> ('a * 'b * 'c) conv
val t4 : ?sep:char -> 'a conv -> 'b conv -> 'c conv -> 'd conv -> ('a * 'b * 'c * 'd) conv


(** The Object-Oriented CLI definition *)
class virtual ['r] cli : object

  (** [arg a k] adds the argument specification [a] to the CLI and
      calls [k] on its final value. *)
  method arg : 'a. ('a, final) arg -> ('a -> unit) -> unit

  (** [set a r] adds the argument specification [a] to the CLI and
      sets the reference with its final value. *)
  method set : 'a. ('a, final) arg -> 'a ref -> unit

  (** Entrypoint definition. *)
  method virtual entrypoint : unit -> 'r

  (** Inherted {!Cmdliner} definitions. *)
  method man_xrefs : Cmdliner.Manpage.xref list
  method man : Cmdliner.Manpage.block list
  method envs : Cmdliner.Term.env_info list
  method doc : string
  method version : string option
  method name : string
  method term : 'r Cmdliner.Term.t * Cmdliner.Term.info

  (** Runs the entrypoint according to the CLI definition. *)
  method run : 'r
end
