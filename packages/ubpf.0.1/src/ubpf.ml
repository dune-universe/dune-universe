
type t

external load : string -> elf:bool -> t = "ml_bpf_load"
external jit_compile : t -> unit = "ml_bpf_compile"
external exec : t -> string -> int = "ml_bpf_exec"
external release : t -> unit = "ml_bpf_close"

let load = function
| `Bpf s -> load s ~elf:false
| `Elf s -> load s ~elf:true

let compile code = let t = load code in jit_compile t; t
