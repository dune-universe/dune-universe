open Printf

module L = BatList
module Ht = Hashtbl
module String = BatString

let randomize_list l =
  L.shuffle ~state:(BatRandom.State.make_self_init ()) l

let with_out_file (fn: string) (f: out_channel -> 'a): 'a =
  let out = open_out_bin fn in
  let res = f out in
  close_out out;
  res

type mode = Head of int
          | Tail of int
          | Head_tail of int * int
          | Just_one of int
          | Between of int * int
          | Several of (int, unit) Ht.t

let is_substring super sub =
  try let _index = String.find super sub in true
  with Not_found -> false

let classify s =
  if String.starts_with s "+" then
    let n = int_of_string (String.lchop s) in
    Head n
  else if String.starts_with s "-" then
    let n = int_of_string (String.lchop s) in
    Tail n
  else if is_substring s ":" then
    let head, tail = String.split ~by:":" s in
    Head_tail (int_of_string head, int_of_string tail)
  else if is_substring s ".." then
    let start_i, stop_i = String.split ~by:".." s in
    Between (int_of_string start_i, int_of_string stop_i)
  else if String.contains s ',' then
    let strings = String.split_on_char ',' s in
    let line_nums = L.map int_of_string strings in
    let ht = Ht.create 11 in
    L.iter (fun i -> Ht.add ht i ()) line_nums;
    Several ht
  else
    Just_one (int_of_string s)

let main () =
  let usage_message =
    sprintf "usage:\n%s {--range|-r} {+n|-n|i|i..j|i:j|i,j[,...]} [-i FILE] \
             [--rand] [-v] (1 <= i [<= j] <= N; N = nb. lines in FILE)\n"
      Sys.argv.(0) in
  let argc = Array.length Sys.argv in
  if argc = 1 then
    (eprintf "%s" usage_message; exit 1);
  let rand_opt = ref false in
  let invert_opt = ref false in
  let range_opt = ref "" in
  let input_fn_opt = ref "/dev/stdin" in
  let (output_fn_opt: string option ref) = ref None in
  Arg.parse
    ["-v", Arg.Set invert_opt,
     "invert the selection of lines (like 'grep -v')";
     "--rand", Arg.Set rand_opt,
     "randomize selected lines before writing them out";
     "-i", Arg.Set_string input_fn_opt,
     "<filename> where to read lines from (default=stdin)";
     "-o", Arg.String (fun fn -> output_fn_opt := Some fn),
     "<filename> where to write lines to (default=stdout)";
     "--range", Arg.Set_string range_opt,
     "{+n|-n|i|i..j|i,j[,...]}: line selection policy; \
      (+n => top n lines; \
      -n => last n lines; \
      n => only line n; \
      i..j => lines i to j; \
      i:j => top i lines and last j lines; \
      i,j[,...] => only lines i,j,...";
     "-r", Arg.Set_string range_opt, "alias for --range"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let randomize = !rand_opt in
  let invert = !invert_opt in
  let nums_str = !range_opt in
  let input_fn = !input_fn_opt in
  let all_lines = L.of_enum (BatFile.lines_of input_fn) in
  let nb_lines = L.length all_lines in
  let selected_lines =
    match classify nums_str with
    | Head n ->
      if n > nb_lines then
        (eprintf "get_line: %d > %d\n" n nb_lines;
         exit 1);
      (if invert then L.drop else L.take)
        n all_lines
    | Tail n ->
      if n > nb_lines then
        (eprintf "get_line: %d > %d\n" n nb_lines;
         exit 1);
      (if invert then L.take else L.drop)
        (nb_lines - n) all_lines
    | Head_tail (h, t) ->
      (* whole file: head|body|tail *)
      let total = h + t in
      if total > nb_lines then
        (eprintf "get_line: %d + %d > %d\n" h t nb_lines;
         exit 1);
      let head, rest = L.takedrop h all_lines in
      let body, tail = L.takedrop (nb_lines - total) rest in
      if invert then (* inverted case *)
        body
      else (* standard case *)
        L.append head tail
    | Just_one i ->
      if i < 1 || i > nb_lines then
        (eprintf "get_line: %d < 1 || %d > %d\n" i i nb_lines;
         exit 1);
      if invert then
        let xs, rest = L.takedrop (i - 1) all_lines in
        L.append xs (L.drop 1 rest)
      else
        L.take 1 (L.drop (i - 1) all_lines)
    | Between (i, j) ->
      if i < 1 || j < i || j > nb_lines then
        (eprintf "get_line: %d < 1 || %d < %d || %d > %d\n" i j i j nb_lines;
         exit 1);
      if invert then
        let xs, rest = L.takedrop (i - 1) all_lines in
        L.append xs (L.drop ((j - i) + 1) rest)
      else
        L.take ((j - i) + 1) (L.drop (i - 1) all_lines)
    | Several ht ->
      let rev_res = ref [] in
      L.iteri (fun j line ->
          let i = j + 1 in
          if Ht.mem ht i then
            rev_res := line :: !rev_res
        ) all_lines;
      L.rev !rev_res in
  let to_output =
    if randomize then randomize_list selected_lines
    else selected_lines in
  (* there was a bug: if we open /dev/stdout, the file 'fn'
     is truncated in case stdout was redirected to 'fn'
     from the command line. *)
  match !output_fn_opt with
  | None -> L.iter (printf "%s\n") to_output (* bug correction *)
  | Some fn ->
    with_out_file fn (fun out ->
        L.iter (fprintf out "%s\n") to_output
      )

let () = main ()
