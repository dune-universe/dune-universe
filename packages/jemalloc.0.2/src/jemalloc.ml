open Printf

let (!!) = Lazy.force

exception Invalid_property of string
let () = Callback.register_exception "Jemalloc_ctl.Invalid_property" (Invalid_property "")

external mallctl_bool : string -> bool option -> bool = "ml_je_mallctl_boolean"
external mallctl_int : string -> int option -> int = "ml_je_mallctl_int"
external mallctl_string : string -> string option -> string = "ml_je_mallctl_string"
external mallctl_unit : string -> unit = "ml_je_mallctl_unit"


let version () =
  let version = mallctl_string "version" None in
  let parse_numeric version numeric git =
    match String.split_on_char '.' numeric with
    | major :: minor :: _ ->
      begin try
          version, int_of_string major, int_of_string minor, git
        with _ -> version, 0, 0, ""
      end
    | _ -> version, 0, 0, ""
  in
  match String.split_on_char '-' version with
  | numeric :: _patch :: git :: _ -> parse_numeric version numeric git
  | numeric :: _ -> parse_numeric version numeric ""
  | _ -> version, 0, 0, ""

external get_all_arenas_prefix : unit -> int = "ml_je_all_arena"

let all_arena_prefix = Lazy.from_val begin sprintf "arena.%d" (get_all_arenas_prefix ()) end

let release_free_memory =
  let purge_all = lazy (!!all_arena_prefix ^ ".purge" ) in
  fun () -> mallctl_unit !!purge_all

let epoch =
  let epoch = ref 0 in
  fun () ->
    incr epoch;
    let _:int = mallctl_int "epoch" (Some !epoch) in
    ()

type memory_stats = {
  active: int;
  resident: int;
  allocated: int;
  mapped: int;
}

let get_stat name =
  mallctl_int ("stats." ^ name) None

let get_memory_stats () =
  epoch ();
  { active = get_stat "active"; resident = (try get_stat "resident" with _ -> 0); allocated = get_stat "allocated";  mapped = get_stat "mapped" }
