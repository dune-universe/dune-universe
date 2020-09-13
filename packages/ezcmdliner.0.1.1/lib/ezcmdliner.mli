(** Easier interface to Cmdliner.

    [Cmdliner] is a great command line factory but composing binaries
    definitions is quite painful. This library adds a small layer
    upon [Cmdliner] to target this issue.

    {1 Quickstart}

    To define a new command line interface (CLI) to a function, you must
    create a new configuration storing the CLI parameters:
    {[
      open Ezcmdliner
      let cfg = create ()
    ]}

    Then you can add parameters in a quite similar way of [Cmdliner]:
    {[
      let foo = register cfg @@ value @@ opt
          ~doc:"Foo parameter"
          ~conv:string
          ~default:"foo"
          ["f"; "foo"]
    ]}
    This will add a optional string parameter [foo] with a default value
    ["foo"] which will be customizable through the CLI with options
    [-f] or [--foo]. The main difference with [Cmdliner] is the
    [register] function which adds the parameter to [cfg] and returns
    a function whose type is [unit -> 'a] where ['a] depends on
    [conv]. Here, [foo] is a [unit -> string] function and is
    a getter to the foo parameter value.

    The function that need the [foo] value can use it:
    {[
      let main () = Format.printf "foo = %s@." (foo ())
    ]}
    and we can use it to define the full CLI:
    {[
      let foo_cmd = command ~cfg ~doc:"Foo printing." main
    ]}
    [foo_cmd] represents the CLI specification but not the CLI
    execution. To execute this command, you must use the run
    function:
    {[
      let () = run foo_cmd
    ]}

    {1 Details}

    The main motivation for this library is concerning modularity
    and removing all the boilerplate code needed to define a nice CLI.
    [Cmdliner]
    do remove this boilerplate code for the most part but in some
    cases where modularity and some incremental CLI definition
    is needed, [Cmdiner] can be painful.

    As you can see in the {!Quickstart} section, defining new command
    from scratch is quite easy. But now, imagine you want to extend
    this CLI with new parameters or change a bit the behavior
    of the underlying command. Of course, this can be done with
    [Cmdliner] by proper code organization and exposing argument terms,
    but doing so is boring and feels wrong. It feels wrong because
    argument terms are designed to be intermediate objects and all
    programmers (should) know that exposing such stuff is a bad thing
    (even if not security issue is involved, it implies some kind of API
    noise).

    {2 Incremental CLI}

    [Ezcmdliner] avoids this by abstracing the argument term and
    somehow cut the term definition in a lazy way to allow the
    programmer to specify its CLI in an incremental way.

    For exemple, suppose you want to add a [bar] option to the
    previous example. There is no need to redefine the
    configuration, simply use the previous one:
    {[
      let ext_cfg = from cfg
    ]}
    Then add the [bar] option:
    {[
      let bar = register ext_cfg @@ value @@ opt
          ~doc:"Bar parameter"
          ~conv:int
          ~default:0
          ["b"; "bar"]
    ]}
    And now, we can change the CLI behavior:
    {[
      let bar_cmd = {
        foo_cmd with
        cmd = (fun () ->
            foo_cmd.cmd ();
            let b = bar () in
            Format.printf "square(bar) = %i@." (b * b));
        doc = foo_cmd ^ " Prints also the square of bar parameter.";
      }
      let () = run bar_cmd
    ]}
    As you can see, running the binary will print the [foo] value
    but also the square of [bar].

    {2 Lwt}

    [Ezcmdliner] is fully compatible with Lwt. Simply give a Lwt thread
    to {!command} to produce a ['a Lwt.t command] value. Running this
    command will produce a ['a Lwt.t] value that should be passed
    to [Lwt_main.run].

    {3 Partial CLI}

    [Ezcmdliner] doesn't handle partial CLI evaluation to use sub-commands
    for now but it will come in near future.

    {1 API}

    There are three concepts in this library: arguments, configurations
    and commands.

    {2 Arguments}

    Arguments are the CLI parameters which are roughly split in three
    sorts: flags, optional parameters and positional parameters. All
    of them are represented by the {!arg} type and follows the [Cmdliner]
    usual specification in a bit more concise way.

    For more information about various labelled aguments to {!flag} {!opt}
    and other argument makers, see the [Cmdliner.Arg] documentation. There
    are small differences though: useless parameters don't appear in
    related arguments and environment variables take their documentation
    stuff from their related argument (in [Cmdliner] they can have a
    distinct documentation).
*)

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
