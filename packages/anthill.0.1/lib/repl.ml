open Core
open Top
open Lwt

let display term text =
  let open LTerm_text in
  let out = eval [ S text ] in
  LTerm.fprintls term out

let make_prompt text =
  let open LTerm_text in
  React.S.const (eval [ S text ])

class read_line ~term ~history ~prompt = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (make_prompt prompt)
end

let print_instructions term = display term "
  Anagram: a letters
  Pattern: p letters
  Build: b letters
  Wildcards: . = blank, * = 0+ blanks, [abc] = any of a,b,c
  Set operations: & = and, | = or, - = diff
  Use () to combine expressions when using set operations.
  Example: eights in retinas? not starting with a vowel
    (a retinas.) - (p [aeiou]*)

  Hit Ctrl-D to exit
  ------------------------------------------------
  "

let display_result term env _expr ws =
  let wlist = Output.format_wordset env ws in
  Lwt_list.iter_s (display term) wlist

let show_exc term x =
  display term (Output.format_exception x)

let display_error term e =
  display term (Output.format_error e)

let run env term str =
  let open Env in
  try
    match Parser.parse str with
    | Ok expr -> begin
        let env', l = Eval.eval !env expr in
        env := {env' with op = Librepl.op_of_expr env' expr};
        display_result term !env expr l
      end
    | Error m -> display_error term m
  with
  | x -> show_exc term x

let rec loop term history env =
  match%lwt
    try%lwt
      let prompt = Librepl.prompt_of_op !env.Env.op in
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~prompt in
      let%lwt command = rl#run in
      return (Some command)
    with Sys.Break ->
      return None
  with
  | Some command ->
      let command_utf8= Zed_string.to_utf8 command in
      let%lwt () = run env term command_utf8 in
      LTerm_history.add history command;
      loop term history env
  | None ->
      loop term history env

let repl env =
  let%lwt () = LTerm_inputrc.load () in
  try%lwt
    let%lwt term = Lazy.force LTerm.stdout in
    let%lwt () = print_instructions term in
    loop term (LTerm_history.create []) env
  with LTerm_read_line.Interrupt ->
    return ()
