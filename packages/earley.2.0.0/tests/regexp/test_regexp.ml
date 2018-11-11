open Earley_core

let generated = "random_numbers.txt"

let _ =
  if not (Sys.file_exists generated) then
  let oc = open_out generated in
  for i = 1 to 100000 do
    Printf.fprintf oc " %i  %i \n" i (Random.int (1 lsl 29))
  done;
  close_out oc

let regexp ptr =
  let open Regexp in
  let num = Charset.range '0' '9' in
  let blank = List.fold_left Charset.add Charset.empty [' ';'\t';'\n';'\r'] in
  read_regexp (Str (Seq [Str (Set blank); Pls (Set num); Str (Set blank)]))

let test_rodolphe () =
  let time = Sys.time () in
  let ch = open_in generated in
  let ptr = ref "" in
  try
    while true do
      let line = input_line ch in
      let buf = Input.from_string line in
      ignore (regexp ptr buf 0);
    done;
    assert false
  with
  | Assert_failure _ as e -> raise e
  | _ ->
    close_in ch;
    let time' = Sys.time () in
    Printf.printf "rodolphe: %f\n%!" (time' -. time)

let regexp = Str.regexp "\\([ \n\t\r]*[0-9]+[ \n\t\r]*\\)*"

let test_str () =
  let time = Sys.time () in
  let ch = open_in generated in
  try
    while true do
      let line = input_line ch in
      assert (Str.string_match regexp line 0)
    done;
    assert false
  with
  | Assert_failure _ as e -> raise e
  | _ ->
    close_in ch;
    let time' = Sys.time () in
    Printf.printf "str: %f\n%!" (time' -. time)

let _ =
  test_str ();
  test_rodolphe ()
