(** Library for in-browser testing, dependent on js_of_ocaml. *)

(** Types and functions for running tests in a browser. *)
module Runner : sig

  val setup : Webtest.Suite.t -> unit
  (** [setup test] sets up a test runner and attaches it to the document's onLoad
      handler. *)

  val run : ?with_colors:bool -> Webtest.Suite.t -> unit
  (** [run suite] runs tests and displays results on stdout *)
end
