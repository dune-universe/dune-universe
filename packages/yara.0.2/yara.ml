open Core
open Ctypes
open PosixTypes
open Foreign

[@@@ocaml.warning "-32"]
(* Yara types *)

(* MAX_THREADS is 32 - see include/yara/limits.h *)

(* --------------------------------------------------------------------------- *)
(* YR_COMPILER *)
type yrcompiler
let yrcompiler : yrcompiler structure typ = structure "YR_COMPILER"
(* YR_RULE *)
type yrrule
let yrrule : yrrule structure typ = structure "YR_RULE"
(* YR_META *)
type yrmeta
let yrmeta : yrmeta structure typ = structure "YR_META"
(* YR_NAMESPACE *)
type yrnamespace
let yrnamespace : yrnamespace structure typ = structure "YR_NAMESPACE"
(* YR_STRING *)
type yrstring
let yrstring : yrstring structure typ = structure "YR_STRING"
(* YR_RULES *)
type yrrules
let yrrules : yrrules structure typ = structure "YR_RULES"

(* Types shortcuts *)

type yrcptr = yrcompiler ptr
(* let yrcptr : yrcptr typ = ptr yrcompiler *)

type yrcptrptr = yrcompiler ptr ptr
(* let yrcptrptr : yrcptrptr typ = ptr ptr yrcompiler *)

(* --------------------------------------------------------------------------- *)
(* YR_META *)
let yrmeta_identifier = field yrmeta "identifier" string;;
let yrmeta_type = field yrmeta "type" int32_t;;

let () = seal yrmeta;;

type yara_meta = {
    identifier : string;
    typ : int;
}

(* --------------------------------------------------------------------------- *)
(* YR_NAMESPACE *)
let yrnamespace_name = field yrnamespace "name" string;;

let () = seal yrnamespace;;

type yara_namespace = {
    name : string;
}

(* --------------------------------------------------------------------------- *)
(* YR_STRING *)
let yrstring_gflags = field yrstring "g_flags" int32_t;;
let yrstring_gflags = field yrstring "length" int32_t;;
let yrstring_identifier = field yrstring "identifier" string;;
let yrstring_string = field yrstring "string" string;;
let yrstring_chained = field yrstring "chained_to" (ptr yrstring);;
let yrstring_rule = field yrstring "rule" (ptr yrrule);;

let () = seal yrstring;;

type yara_string = {
    identifier : string;
    str : string;
}

(* --------------------------------------------------------------------------- *)
(* YR_RULE *)
(* struct YR_RULE {
    * int32 g_flags;
    * int32 t_flags[32]; // MAX_THREADS
    * union {const char *identifier; uint64_t identifier_; };
    * union {const char *tags; uint64_t tags_;};
    * union {YR_META *metas; uint64_t metas_;};
    * union {YR_STRING *strings; uint64_t strings_;};
    * union {YR_NAMESPACE *ns, uin64_t ns_;};
    * uint64 time_cost;
    * };
 * *)

let yrrule_gflags = field yrrule "g_flags" int32_t;;
let yrrule_tflags = field yrrule "t_flags" (array 32 int32_t);;
let yrrule_identifier = field yrrule "identifier" string;;
let yrrule_tags = field yrrule "tags" (ptr_opt char);;
let yrrule_metas = field yrrule "metas" (ptr yrmeta);;
let yrrule_strings = field yrrule "strings" (ptr yrstring);;
let yrrule_ns = field yrrule "ns" (ptr yrnamespace);;
let yrrule_timecost = field yrrule "time_cost" uint64_t;;

let () = seal yrrule;;

(*
type yrrulesptr
let yrrulesptr : yrrulesptr typ = ptr yrrules
*)

type yara_rule = {
    identifier : string;
    tags : string;
    (*
    metas : yara_meta list;
    strings : yara_string list;
    ns : yara_namespace list;
    *)
}

(* --------------------------------------------------------------------------- *)
(* YR_CALLBACK_FUNC *)
(* typedef int YR_CALLBACK_FUNC(
    int message,
    void *message_data,
    void *user_data);
 *)
let yrcallback_t = int @-> ptr void @-> ptr void @-> returning int

(* --------------------------------------------------------------------------- *)
(* YR_COMPILER_CALLBACK_FUNC *)
(* typedef int YR_COMPILER_CALLBACK_FUNC(
    int error_level,
    const char* file_name,
    int line_number,
    const char* message,
    void *user_data);
 *)
let yrcompilercallback_t = int @-> string @-> int @-> string @-> ptr void @-> returning void

(* --------------------------------------------------------------------------- *)

(* Yara API *)

(*
 * int yr_initialize(void)
 *
 *)
let yr_initialize =
    foreign "yr_initialize" (void @-> (returning int))

(*
 * int yr_finalize(void)
 *
 *)
let yr_finalize =
    foreign "yr_finalize" (void @-> (returning int))

(*
 * void yr_finalize_thread(void)
 *
 *)
let yr_finalize_thread =
    foreign "yr_finalize_thread" (void @-> (returning void))

(*
 * int yr_compiler_create(YR_COMPILER **compiler)
 *
 *)
let yr_compiler_create =
    foreign "yr_compiler_create" ( ptr (ptr yrcompiler) @-> (returning int))

(*
 * void yr_compiler_destroy(YR_COMPILER *compiler)
 *
 *)
let yr_compiler_destroy =
    foreign "yr_compiler_destroy" ( ptr yrcompiler @-> (returning void))

(*
 * int yr_compiler_set_callback(YR_COMPILER *compiler,
 * YR_COMPILER_CALLBACK_FUNC callback, void *user_data)
 *)
let yr_compiler_set_callback =
    foreign "yr_compiler_set_callback" ( ptr yrcompiler @->
        funptr yrcompilercallback_t @-> ptr void @-> (returning int))
(*
 * int yr_compiler_add_file(YR_COMPILER *compiler, FILE *file, const char* namespace, const char *filename)
 *
 *)
let yr_compiler_add_file =
    foreign "yr_compiler_add_file" ( ptr yrcompiler @-> ptr int @-> string @-> string @-> (returning int))

(*
 * int yr_compiler_add_fd(YR_COMPILER *compiler, YR_FILE_DESCRIPTOR rules_fd, const char* namespace, const char* file_name)
 *
 *)
let yr_compiler_add_fd =
    foreign "yr_compiler_add_fd" ( ptr yrcompiler @-> int @-> string @-> string @-> (returning int))

(*
 * int yr_compiler_add_string(YR_COMPILER *compiler, const char *string, const char *namespace)
 *
 *)
let yr_compiler_add_string =
    foreign "yr_compiler_add_string" ( ptr yrcompiler @-> string @-> string @-> (returning int))

(*
 * int yr_compiler_get_rules(YR_COMPILER *compiler, const char *string, const char *namespace)
 *
 *)
let yr_compiler_get_rules =
    foreign "yr_compiler_get_rules" ( ptr yrcompiler @-> ptr (ptr yrrules) @-> (returning int))

(*
 * void yr_rules_destroy(YR_RULES *rules)
 *
 *)
let yr_rules_destroy =
    foreign "yr_rules_destroy" ( ptr yrrules @-> (returning void))

(*
 * int yr_rules_load(const char* filename, YR_RULES *rules)
 *
 *)
let yr_rules_load =
    foreign "yr_rules_load" ( string @-> ptr yrrules @-> (returning int))

(*
 * int yr_rules_save(YR_RULES *rules, const char *filename)
 *
 *)
let yr_rules_save =
    foreign "yr_rules_save" ( ptr yrrules @-> string @-> (returning int))

(*
 * int yr_compiler_define_integer_variable(YR_COMPILER *compiler, const char *identifier, int64_t value)
 *
 *)
let yr_compiler_define_integer_variable =
    foreign "yr_compiler_define_integer_variable" ( ptr yrcompiler @-> string @-> int64_t @-> (returning int))

(*
 * int yr_compiler_define_float_variable(YR_COMPILER *compiler, const char *identifier, double value)
 *
 *)
let yr_compiler_define_float_variable =
    foreign "yr_compiler_define_float_variable" ( ptr yrcompiler @-> string @-> float @-> (returning int))

(*
 * int yr_compiler_define_boolean_variable(YR_COMPILER *compiler, const char *identifier, int value)
 *
 *)
let yr_compiler_define_boolean_variable =
    foreign "yr_compiler_define_boolean_variable" ( ptr yrcompiler @-> string @-> int @-> (returning int))

(*
 * int yr_compiler_define_string_variable(YR_COMPILER *compiler, const char *identifier, const char* value)
 *
 *)
let yr_compiler_define_string_variable =
    foreign "yr_compiler_define_string_variable" ( ptr yrcompiler @-> string @-> string @-> (returning int))

(*
 * int yr_rules_scan_mem(YR_RULES *rules, const uint8_t *buf, size_t bufsize,
 * int flags, YR_CALLBACK_FUNC callback, void *user_data, int timeout)
 *)
let yr_rules_scan_mem =
    foreign "yr_rules_scan_mem" ( ptr yrrules @-> ptr uint8_t @-> size_t @->
        int @-> funptr yrcallback_t @-> ptr void @-> int @->
        (returning int))

(*
 * int yr_rules_scan_file(YR_RULES *rules, const char *filename,
 * int flags, YR_CALLBACK_FUNC callback, void *user_data, int timeout)
 *)
let yr_rules_scan_file =
    foreign "yr_rules_scan_file" ( ptr yrrules @-> string @->
        int @-> funptr yrcallback_t @-> ptr void @-> int @->
        (returning int))

(*
 * int yr_rules_scan_fd(YR_RULES *rules, YR_FILE_DESCRIPTOR fd,
 * int flags, YR_CALLBACK_FUNC callback, void *user_data, int timeout)
 *)
let yr_rules_scan_fd =
    foreign "yr_rules_scan_fd" ( ptr yrrules @-> int @->
        int @-> funptr yrcallback_t @-> ptr void @-> int @->
        (returning int))


(* --------------------------------------------------------------------------- *)

type errors =
    | Error_success
    | Error_insufficient_memory
    | Error_could_not_open_file
    | Error_could_not_map_file
    | Error_zero_length_file
    | Error_invalid_file
    | Error_corrupt_file
    | Error_unsupported_file_version
    | Error_too_many_scan_threads
    | Error_scan_timeout
    | Error_callback_error
    | Error_too_many_matches
    [@@deriving enum]

(* TODO: Add function for textual representation of errors *)

(* TODO: Propagate all errors up *)

(* --------------------------------------------------------------------------- *)

type messages =
    | Callback_msg_nothing
    | Callback_msg_rule_matching
    | Callback_msg_rule_not_matching
    | Callback_msg_scan_finished
    | Callback_msg_import_module
    | Callback_msg_module_imported
    [@@deriving enum]

(* --------------------------------------------------------------------------- *)

let yara_init () =
    let res = errors_of_enum (yr_initialize ()) in
    match res with
    | Some Error_success -> Ok ()
    | _ -> Or_error.error_string "Cannot initialize Yara"

let yara_deinit () =
    let res = errors_of_enum (yr_finalize ()) in
    match res with
    | Some Error_success -> Ok ()
    | _ -> Or_error.error_string "Cannot deinitialize Yara"

let yara_create () =
    let compiler = allocate (ptr yrcompiler) (from_voidp yrcompiler null) in
    let res = errors_of_enum (yr_compiler_create compiler) in
    match res with
    | Some Error_success ->
        let comp = !@ compiler in
        Ok comp
    | _ -> Or_error.error_string "Cannot create Yara compiler"

let yara_add_file compiler filename ns =
    let fd = Unix.openfile filename ~mode:[Unix.O_RDONLY;] ~perm:0o400 in
    let realfd = Unix.File_descr.to_int fd in
    let res = yr_compiler_add_fd compiler realfd ns filename in
    match errors_of_enum res with
    | Some Error_success -> Ok ()
    | _ -> Or_error.error_string "Cannot add the rules file to Yara compiler"

let yara_add_string compiler str ns =
    let res = errors_of_enum (yr_compiler_add_string compiler str ns) in
    match res with
    | Some Error_success -> Ok ()
    | _ -> Or_error.error_string "Cannot add the rules to Yara compiler"

(* Rules load and save *)
let yara_get_rules compiler =
    let rules = allocate (ptr yrrules) (from_voidp yrrules null) in
    let res = errors_of_enum (yr_compiler_get_rules compiler rules) in
    (* TODO: Print some rules information for debug purpose *)
    match res with
    | Some Error_success -> Ok (!@ rules)
    | _ -> Or_error.error_string "Cannot get any rules!"

(* Scan memory *)
let yara_scan_mem rules buf fn =
    (* We run "fn" as a callback *)
    (* Scan callback *)
    let callback msg msgdata _userdata =
        (* CALLBACK_MSG_RULE_MATCHING *)
        let rcvd = messages_of_enum msg in
        let _ = match rcvd with
            | Some Callback_msg_rule_matching -> (
                (* msgdata is YR_RULE in this case *)
                let rawrule = !@ (from_voidp yrrule msgdata) in
                let tags = match (getf rawrule yrrule_tags) with
                    | None -> ""
                    | Some tags_ptr -> coerce (ptr char) string tags_ptr
                in
                let rule = {
                    identifier = getf rawrule yrrule_identifier;
                    tags = tags;
                    (* Convert pointer to a list here *)
                    (*
                    metas = getf rawrule yrrule_metas;
                    strings = getf rawrule yrrule_strings;
                    ns = getf rawrule yrrule_ns;
                    *)
                } in
                Printf.printf "MATCHED: %s\n" rule.identifier;
                fn rule)
            | Some Callback_msg_rule_not_matching -> (
                (* msgdata is YR_RULE in this case *)
                let rawrule = !@ (from_voidp yrrule msgdata) in
                let rule = {
                    identifier = getf rawrule yrrule_identifier;
                    tags = "";
                } in
                Printf.printf "UNMATCHED: %s\n" rule.identifier;)
            | _ -> ()
        in
        (* TODO: At some point in the future return something more
        * meaningful than simply 0... maybe 42? *)
        0
    in
    let buflen = Unsigned.Size_t.of_int (String.length buf) in
    (* TODO: Remove unnecessary copy *)
    let bufp = CArray.of_list char (String.to_list buf) in
    let buffer = coerce (ptr char) (ptr uint8_t) (CArray.start bufp) in
    let result = yr_rules_scan_mem rules buffer buflen 0 callback null 0 in
    let res = errors_of_enum result in
    match res with
    | Some Error_success -> Ok ()
    | _ -> Or_error.error_string "Cannot perform the Yara scan"

(* Scan file *)
let yara_scan_file rules filename fn =
    (* Scan callback *)
    let callback msg msgdata _userdata =
        (* CALLBACK_MSG_RULE_MATCHING *)
        let rcvd = messages_of_enum msg in
        let _ = match rcvd with
            | Some Callback_msg_rule_matching -> (
                (* msgdata is YR_RULE in this case *)
                let rawrule = !@ (from_voidp yrrule msgdata) in
                let tags = match (getf rawrule yrrule_tags) with
                    | None -> ""
                    | Some tags_ptr -> coerce (ptr char) string tags_ptr
                in
                let rule = {
                    identifier = getf rawrule yrrule_identifier;
                    tags = tags;
                    (* Convert pointer to a list here *)
                    (*
                    metas = getf rawrule yrrule_metas;
                    strings = getf rawrule yrrule_strings;
                    ns = getf rawrule yrrule_ns;
                    *)
                } in
                Printf.printf "MATCHED: %s\n" rule.identifier;
                fn rule)
            | Some Callback_msg_rule_not_matching -> (
                (* msgdata is YR_RULE in this case *)
                let rawrule = !@ (from_voidp yrrule msgdata) in
                let rule = {
                    identifier = getf rawrule yrrule_identifier;
                    tags = "";
                } in
                Printf.printf "UNMATCHED: %s\n" rule.identifier;)
            | _ -> ()
        in
        (* TODO: At some point in the future return something more
        * meaningful than simply 0... maybe 42? *)
        0
    in
    let result = yr_rules_scan_file rules filename 0 callback null 0 in
    let res = errors_of_enum result in
    match res with
    | Some Error_success -> Ok ()
    | _ -> Or_error.error_string "Cannot perform the Yara scan"

(* TODO: Provide macro-like stuff, e.g. yr_rules_foreach, yr_rule_enable, ... *)
