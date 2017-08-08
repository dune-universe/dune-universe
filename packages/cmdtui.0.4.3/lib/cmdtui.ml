type completion_spec = {
  desc: string option;
  choices_gen: (unit -> string list) option;
}

type completion = {
  desc: string option;
  choices: string list option;
}

(* optionally consumes an argument, and returns:
   remaining arguments, completion for last argument, and conversion function
*)
type 'a t = string list -> string list * completion_spec * (unit -> 'a)

let const v l = l, { desc = None; choices_gen = None }, fun () -> v

let ($) prev arg lst  =
  (* consume previous arguments *)
  let lst, completion, f = prev lst in
  (* apply current argument *)
  let lst, completion, v = match lst with
  | [] ->
      let lst,_,v = arg [] in lst,completion,v
  | lst -> arg lst
  in
  lst, completion, fun () -> (f ()) (v ())

let conv ?desc ?choices convert = function
| [] -> [], { desc; choices_gen = choices }, fun () -> invalid_arg "too few arguments"
| hd :: tl ->
    tl, { desc; choices_gen = choices }, fun () -> convert hd

let enum ~desc pairs =
  let choices () = List.rev_map fst pairs in
  let f arg =
    try List.assoc arg pairs
    with Not_found ->
      failwith (Printf.sprintf "Unknown enum: %S" arg)
  in
  conv ~desc ~choices f

let add_help display_help pairs =
  let descriptions =
    (("help", "display this help message") ::
     List.rev_map (fun (name, (doc, _)) -> name, doc) pairs) |>
    List.sort_uniq Pervasives.compare
  in
  ("help", ("", const display_help $ const descriptions)) :: pairs

let commands ?help pairs =
  let pairs = match help with
  | None -> pairs
  | Some f -> add_help f pairs in
  let choices () =  (List.rev_map fst pairs) in function
  | [] ->
      [], { desc = None; choices_gen = Some choices }, fun () -> invalid_arg "too few arguments"
  | hd :: tl ->
      try
        let _, f = List.assoc hd pairs in
        f tl
      with Not_found ->
        tl, { desc = None; choices_gen = Some choices }, fun () -> failwith (Printf.sprintf "Unknown command %S" hd)

let int = conv ~desc:"integer" int_of_string
let float = conv ~desc:"float" float_of_string
let string = conv ~desc:"string" (fun x -> x)

let eval spec argv =
  match spec (Array.to_list argv) with
  | [], _, r -> r ()
  | remaining, _, _ -> failwith ("Too many arguments, unused arguments: " ^ (String.concat " " remaining))

let completion spec argv =
  (*    Logs.debug (fun m -> m "completion on %a" Fmt.(Dump.array string) argv);*)
  match spec (Array.to_list argv) with
  | [], { desc; choices_gen = None }, _ ->
      Some { desc; choices = None }
  | [], { desc; choices_gen = Some choices }, _ ->
      let prefix = if Array.length argv > 0 then argv.(Array.length argv - 1) else "" in
      Some {
        desc;
        choices = Some (List.filter (fun cmd -> Astring.String.is_prefix ~affix:prefix cmd) (choices ()))
      }
  | _, _, _ ->
      None

(* TODO: re = Re_posix.compile_pat "\"([^\"\\\\]|\\\\.)*\"|[^ ]+" in Re.matches re *)
(* TODO: multiple spaces *)
let split s =
  Astring.String.fields ~is_sep:Astring.Char.Ascii.is_white s |>
  (*Astring.String.cuts ~sep:" " s*) Array.of_list
