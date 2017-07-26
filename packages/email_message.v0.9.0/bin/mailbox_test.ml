open Core
open Email_message

let () =
  Command.basic ~summary:"Mailbox parser tester"
    Command.Spec.(
      empty
      +> flag ~aliases:["i"] "input-file" (optional string)
           ~doc:"file Mailbox to parse."
      +> flag ~aliases:["o"] "output-file" (optional string)
           ~doc:"file File to write the mailbox to.")

    (fun input_file output_file () ->
       let main ifd ofd =
         let mailbox = Mailbox.With_seq.t_of_fd ifd in
         Mailbox.With_seq.iter_string mailbox
           ~f:(Out_channel.output_string ofd)
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
