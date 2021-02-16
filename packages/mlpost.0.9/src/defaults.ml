let defaultprelude = "\\documentclass{article}\n"

let default_filename_prefix = ""

let default_required_files : File.t list = []

let default_t1disasm : string option = None

let default_verbosity = false

let default_debug = false

let mk_ref_default x =
  let r = ref x in
  ((fun () -> !r), fun x -> r := x)

let get_prelude, set_prelude = mk_ref_default defaultprelude

let set_prelude_from_file f =
  set_prelude (Metapost_tool.read_prelude_from_tex_file f)

let get_t1disasm, set_t1disasm = mk_ref_default default_t1disasm

let get_filename_prefix, set_filename_prefix =
  mk_ref_default default_filename_prefix

let required_files = ref default_required_files

let get_required_files () = !required_files

let set_required_files l = required_files := List.map File.from_string l

let append_required_file f =
  required_files := File.from_string f :: !required_files

let get_verbosity, set_verbosity = mk_ref_default default_verbosity

let get_debug, set_debug = mk_ref_default default_debug

type job = string * Types.commandpic

type jobs = job list

let figures = Queue.create ()

let emit s f =
  let s = get_filename_prefix () ^ s in
  Queue.add (s, f) figures

let emited () = Queue.fold (fun l t -> t :: l) [] figures
