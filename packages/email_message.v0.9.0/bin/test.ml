open Core
open Email_message

let () =
  Command.basic ~summary:"E-mail parser tester"
    Command.Spec.(
      empty
      +> flag ~aliases:["i"] "input-file" (optional string)
           ~doc:"Message to parse"
      +> flag ~aliases:["o"] "output-file" (optional string)
           ~doc:"File to write the message")

    (fun input_file output_file () ->
       let main ifd ofd =
         let text = In_channel.input_all ifd in
         let email = Email.of_string text in
         let text' = Email.to_string email in
         Out_channel.output_string ofd text'
       in
       let main' ofd =
         match input_file with
         | Some fname -> In_channel.with_file fname
                           ~f:(fun ifd -> main ifd ofd)
         | None       -> main In_channel.stdin ofd
       in
       match output_file with
       | Some fname -> Out_channel.with_file fname ~f:main'
       | None       -> main' Out_channel.stdout
    ) |> Command.run
