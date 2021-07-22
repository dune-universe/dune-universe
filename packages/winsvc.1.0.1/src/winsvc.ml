external install : string -> string -> string -> string -> unit
  = "ocaml_winsvc_install"

external remove : string -> unit = "ocaml_winsvc_remove"

external run : string -> (unit -> unit) -> (unit -> unit) -> unit
  = "ocaml_winsvc_run"

exception Error of string

let () = Callback.register_exception "ocaml_winsvc_exn" (Error "register")

module type Sig = sig
  val name : string
  val display : string
  val text : string
  val arguments : string list
  val stop : unit -> unit
end

module Make (S : Sig) = struct
  let args = List.map Filename.quote S.arguments
  let executable = Printf.sprintf "%S" Sys.executable_name
  let path = String.concat " " (executable :: args)
  let install () = install S.name S.display S.text path
  let remove () = remove S.name
  let run main = run S.name main S.stop
end
