type yrcompiler
type yrcptr = yrcompiler Ctypes.ptr
type yrcptrptr = yrcompiler Ctypes.ptr Ctypes.ptr
type yrmeta
type yrnamespace
type yrstring
type yrrule
type yrrules

type yara_meta = {
    identifier : string;
    typ : int;
}

type yara_namespace = {
    name : string;
}

type yara_string = {
    identifier : string;
    str : string;
}

type yara_rule = {
    identifier : string;
    tags : string;
    (*
    metas : yara_meta list;
    strings : yara_string list;
    ns : yara_namespace list;
    *)
}

val yara_init : unit -> unit Core.Or_error.t
val yara_deinit : unit -> unit Core.Or_error.t
val yara_create : unit -> yrcompiler Ctypes.structure Ctypes.ptr Core.Or_error.t
val yara_add_string : yrcompiler Ctypes.structure Ctypes.ptr -> string -> string -> unit Core.Or_error.t
val yara_add_file : yrcompiler Ctypes.structure Ctypes.ptr -> string -> string -> unit Core.Or_error.t
val yara_get_rules : yrcompiler Ctypes.structure Ctypes.ptr -> yrrules Ctypes.structure Ctypes.ptr Core.Or_error.t

val yara_scan_mem : yrrules Ctypes.structure Ctypes.ptr -> string -> (yara_rule -> unit) -> unit Core.Or_error.t
val yara_scan_file : yrrules Ctypes.structure Ctypes.ptr -> string -> (yara_rule -> unit) -> unit Core.Or_error.t
