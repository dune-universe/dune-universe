open Core

let run () =
  let lines = In_channel.input_lines In_channel.stdin in
  let result_lines = Line_up_words_lib.Common.line_up_words lines in
  List.iter result_lines ~f:(printf "%s\n%!");
;;

let command = Command.basic_spec ~summary:"alignment for emacs's align-dwim"
  Command.Spec.empty
  run

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
