(*
type job = File.t * Types.commandpic
type jobs = job list

(** A queue of jobs; TODO move this to another module *)
val figures : job Queue.t
val emited : unit -> jobs
val emit : string -> Types.commandpic -> unit

*)
val generate :
  ?prelude:string ->
  ?verbose:bool ->
  ?clean:bool ->
  string ->
  (string * Types.commandpic) list ->
  unit

val mp :
  string -> ?prelude:string -> Defaults.jobs -> File.t * string Misc.IntMap.t
(** Generate files of corresponding type, using the argument of type [jobs],
   and return information about the created files *)

val mps :
  ?prelude:string -> ?verbose:bool -> string -> Defaults.jobs -> File.t list

val pdf :
  ?prelude:string -> ?verbose:bool -> string -> Defaults.jobs -> File.t list

val png :
  ?prelude:string -> ?verbose:bool -> string -> Defaults.jobs -> File.t list

val temp_mp :
  ?prelude:string ->
  ?verbose:bool ->
  ?clean:bool ->
  string ->
  Defaults.jobs ->
  File.t * string Misc.IntMap.t
(** Same as above, but use a temporary directory *)

val temp_mps :
  ?prelude:string ->
  ?verbose:bool ->
  ?clean:bool ->
  string ->
  Defaults.jobs ->
  File.t list

val temp_pdf :
  ?prelude:string ->
  ?verbose:bool ->
  ?clean:bool ->
  string ->
  Defaults.jobs ->
  File.t list

val temp_png :
  ?prelude:string ->
  ?verbose:bool ->
  ?clean:bool ->
  string ->
  Defaults.jobs ->
  File.t list

val dump_mp : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit
(** Same as above, but use a temporary directory and take jobs from the job
   queue *)

val dump_mps : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

val dump_png : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

val dump_pdf : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

val dump_tex : ?prelude:string -> string -> unit

val slideshow : Types.commandpic list -> int -> (int * Types.commandpic) list

val emit_slideshow : string -> Types.commandpic list -> unit

val dumpable : unit -> unit

val depend : string -> unit

(* compatibility *)
val read_prelude_from_tex_file : string -> string

val emit : string -> Types.commandpic -> unit

val dump :
  ?prelude:string ->
  ?pdf:bool ->
  ?eps:bool ->
  ?verbose:bool ->
  ?clean:bool ->
  string ->
  unit

val set_filename_prefix : string -> unit

val figures_names : unit -> string list

type job = Defaults.job

type jobs = Defaults.jobs
