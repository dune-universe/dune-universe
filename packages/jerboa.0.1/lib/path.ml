(** [Jerboa.Path] contains the type definition of the Path record and also a variety of constructors.*)

(** [Path.t] is a record, which has a name part used for variable captring and a regex used for matching the path part.*)
type t = {
  name: string option;
  regex: Re.t
}

(** [Path.create_const regex] creates a path part, which won't be captured as a path variable:
  - regex: regex defines the wanted path part
*)
let create_const regex = {
  name = None;
  regex;
}

(** [Path.create_var name regex] create a path variable, which captures a path argument based on:
  - name: this will be the name of the captured path argument
  - regex: defines the value to be captured
*)
let create_var name regex = {
  name = Some name;
  regex;
}

(** [Path.anything] is a regex, which matches anything in the path part.*)
let anytihng = Re.rep1 (Re.compl [Re.char '/'])

(** [Path.const path_part] creates a simple path part based on:
  - path_part: string based path part
*)
let const path_part = create_const (Re.str path_part)

(** [Path.var name] create a path variable wich will capture the whole path part based on:
  - name: name of the path variable
*)
let var name = create_var name anytihng

let separator = create_const (Re.char '/')

let split_into_path_parts path =
  let path_separator = Re.compile separator.regex in
  Re.split path_separator path