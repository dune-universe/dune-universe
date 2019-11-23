open Printf
open Lbvs_consent

module StringSet = BatSet.String

(* print on stdout only molecules whose inchikey is found in given file
   (one inchikey per line) *)

let accum acc x =
  acc := x :: !acc

let return acc =
  List.rev !acc

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let input_fn = ref "" in
  let keys_fn = ref "" in
  let inplace = ref false in
  let negate = ref false in
  let usage_str =
    sprintf "usage:\n%s -m molecules.sdf -k allowed_keys.txt [-v] [-i]"
      Sys.argv.(0) in
  Arg.parse
    ["-m", Arg.Set_string input_fn, "<filename.sdf> molecules";
     "-k", Arg.Set_string keys_fn, "<filename> inchikeys (one per line)";
     "-i", Arg.Set inplace, "modify the input file in-place";
     "-v", Arg.Set negate, "negate the matching of inchikeys"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_str;
  if not (BatString.ends_with !input_fn ".sdf") then
    failwith (sprintf "%s not a .sdf file" !input_fn);
  let inchikeys = MyUtils.lines_of_file !keys_fn in
  let allowed_keys = StringSet.of_list inchikeys in
  Log.info "%d unique keys" (StringSet.cardinal allowed_keys);
  let counter = ref 0 in
  let acc = ref [] in
  MyUtils.with_in_file !input_fn (fun input ->
      try
        while true do
          let mol = Sdf.read_one input in
          let inchikey = Sdf.get_inchikey mol in
          let key_match = StringSet.mem inchikey allowed_keys in
          if key_match then incr counter;
          if !negate then
            (if not key_match then accum acc mol)
          else
            (if key_match then accum acc mol)
        done
      with End_of_file ->
        Log.info "%d matches" !counter
    );
  let res = return acc in
  MyUtils.with_out_file (if !inplace then !input_fn else "/dev/stdout")
    (fun out -> List.iter (fprintf out "%s") res)

let () = main ()
