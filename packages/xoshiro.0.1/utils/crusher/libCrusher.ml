open TestU01

type battery = SmallCrush | Crush | BigCrush
let default_battery = SmallCrush
let battery = ref default_battery
let set_battery bat =
  battery :=
    match String.lowercase_ascii bat with
    | "smallcrush" -> SmallCrush
    | "crush" -> Crush
    | "bigcrush" -> BigCrush
    | _ -> raise (Arg.Bad (Format.sprintf "Unknown battery: %s" bat))

type verbosity = Verbose | Summary
let default_verbosity = Summary
let verbosity = ref default_verbosity
let set_verbosity lvl =
  verbosity :=
    match String.lowercase_ascii lvl with
    | "verbose" -> Verbose
    | "summary" -> Summary
    (* | "quiet" -> Quiet *)
    | _ -> raise (Arg.Bad (Format.sprintf "Unknown verbosity level: %s" lvl))

let ustring setter value =
  Arg.Unit (fun () -> setter value)

let spec =
  Arg.[
    "--battery",   String set_battery,   "BAT Sets battery to use (default: smallcrush)";
    "--smallcrush", ustring set_battery "smallcrush", " Alias for --battery=smallcrush";
    "--crush",      ustring set_battery "crush",      " Alias for --battery=crush";
    "--bigcrush",   ustring set_battery "bigcrush",   " Alias for --battery=bigcrush";

    "--verbosity", String set_verbosity, "LVL Sets verbosity level (default: summary)";
    "--verbose",   ustring set_verbosity "verbose", " Alias for --verbosity=verbose";
    "-v",          ustring set_verbosity "verbose", " Alias for --verbose";
  ] |> Arg.align

let anon_fun str =
  raise (Arg.Bad (Format.sprintf "Unexpected argument: %s" str))

let usage =
  Format.sprintf "Usage: %s [ARGUMENTS]

Supported batteries are: smallcrush, crush, bigcrush.
Supported verbosity levels are: verbose, summary.

ARGUMENTS can be:" Sys.argv.(0)

let parse_arguments () =
  Arg.parse spec anon_fun usage

let set_testu01_verbosity () =
  match !verbosity with
  | Summary -> Swrite.set_basic false
  | _ -> ()

let run_battery_on gen =
  match !battery with
  | SmallCrush -> Bbattery.small_crush gen
  | Crush -> Bbattery.crush gen
  | BigCrush -> Bbattery.big_crush gen

let nb_suspect_p_values = ref 0

let collect_suspect_p_values () =
  let suspectp = Probdist.Gofw.get_suspectp () in
  Bbattery.get_p_val ()
  |> Array.iter
    (fun pval ->
       if pval <= suspectp || 1. -. pval <= suspectp then
         incr nb_suspect_p_values)

let check_suspect_p_values_and_die () =
  if !nb_suspect_p_values > 0 then
    (
      Format.eprintf "There were %d suspect p-values. Exiting!@." !nb_suspect_p_values;
      exit 7
    );
  exit 0

let run ~name bits =
  parse_arguments ();
  set_testu01_verbosity ();
  let gen = Unif01.create_extern_gen_bits name bits in
  run_battery_on gen;
  collect_suspect_p_values ();
  check_suspect_p_values_and_die ()
