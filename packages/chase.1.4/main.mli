(** Command line parser and formula reader *)

(** Options read from the command line *)
type options = {
    mutable bound : int;
    mutable limit : int;
    terse : bool;
    just_one : bool;
    mutable input_order : bool;
    compact : bool;
    sexpr : bool;
    margin : int;
    quant: bool;
    explicit: bool;
    flat : bool;
  }

(** [main run] parses the command line and reads the formulas and then
   calls [run] to start the main loop. *)
val main : (options -> out_channel -> Formula.form list -> unit) -> unit
