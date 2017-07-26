open Core
open Email_message

let () =
  Command.basic ~summary:"Mailbox parser tester"
    Command.Spec.(empty
                  +> flag ~aliases:["i"] "input-file" (optional string)
                       ~doc:"file Mailbox to parse."
                 )

    (fun input_file () ->
       let main ifd =
         let mailbox = Mailbox.With_seq.t_of_fd ifd in
         let count = ref 0 in
         Mailbox.With_seq.iter_string mailbox ~f:(fun _ -> count := !count + 1);
         printf "%i\n%!" !count;
       in
       match input_file with
       | Some fname -> In_channel.with_file fname
                         ~f:(fun ifd -> main ifd)
       | None       -> main In_channel.stdin

    ) |> Command.run
