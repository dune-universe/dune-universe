module Configurator : sig
  include module type of Configurator

  val ( ^/ ) : string -> string -> string
  (** File path concatenation *)

  val path_sep : char
  (** PATH variable seprator *)

  val exe : string
  (** executable extension *)

  val get_path : unit -> string list
  (** Get the PATH compontents *)

  val find_file_in : string list -> string list -> string option
  (** [find_file_in bases dirs] find one of the file specified in [bases] in the directories [dirs] *)

  val find_program : string -> string option
  (** Find a program in the PATH, ex. [find_program "ls" = Some "ls.exe"] *)

  val find_ocaml_program : string -> string option
  (** Find an OCaml program in the PATH, ex. [find_ocaml_program "foo" = Some "foo.opt.exe"] *)

  val find_ocaml_package : string -> string option
  (** Find OCamlFind package and returns its directory if found *)

  module Package_conf : sig
    type t = Configurator.Pkg_config.package_conf
      = { libs   : string list
        ; cflags : string list
        }

    val merge : t -> t -> t

    val empty : t
  end
end

(** Configuration result item *)
type item =
  | Pkg_config   of unit         option
  | File         of string       option
  | Program      of string       option
  | Library      of Configurator.Package_conf.t option
  | OCamlLibrary of string       option

module Make(A : sig val name : string end) : sig
  val pkg_config : item
  (** Configuration result of pkg-config command *)
    
  val find_program
    :  string
    -> item
  (** [find_program body] finds an executable named [body] in $PATH.
      In Windows, it seaches [body ^ ".exe"].
  *)

  val find_ocaml_program
    :  string
    -> item
  (** [find_ocaml_program body] finds an OCaml executable named [body] in $PATH.
      It is as same as [find_program body], but it first checks the native code compiled version
      named [body ^ ".opt"] (in Windows, [body ^ ".opt.exe"]).
  *)

  val find_ocaml_package
    :  string
    -> item
  (** Finds an OCamlFind package *)

  val by_pkg_config
    :  string
    -> unit
    -> Configurator.Package_conf.t option
  (** C library searching method using pkg-config command *)

  val by_cc
    :  c_flags: string list
    -> link_flags: string list
    -> headers: string list
    -> functions: string list
    -> unit
    -> Configurator.Package_conf.t option
  (** C library searching method using C compiler *)

  val find_library
    : (unit -> Configurator.Package_conf.t option) list
    -> item
  (** Finds a C library using the one of the given searching methods. *)

  val find_file
    :  string
    -> dirs:string list
    -> item
  (** Finds a file in given directories. *)

  val make_header : fname:string -> (string * item) list -> unit
  (** Make a C header file of the configuration results *)

  val write_package_conf_sexps : prefix:string -> item list -> unit
  (** [write_package_conf_sexps ~prefix items] creates sexp files 
      [prefix ^ "c_flags.sexp"] and [prefix ^ "c_library_flags.sexp"]
      for Dune [c_flags] and [c_library_flags] entries *)
end
