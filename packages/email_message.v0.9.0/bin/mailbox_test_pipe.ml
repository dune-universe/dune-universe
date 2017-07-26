open Async
open Core
open Email_message

let () =
  Command.basic ~summary:"Mailbox parser tester"
    Command.Spec.(empty
                  +> flag ~aliases:["i"] "input-file" (optional string)
                       ~doc:"file Mailbox to parse."
                  +> flag ~aliases:["o"] "output-file" (optional string)
                       ~doc:"file File to write the mailbox to.\nBROKEN, use > redirection instead"
                  +> flag ~aliases:["s"] "sexp" (no_arg)
                       ~doc:"Output S-expressions instead of strings")

    (fun input_file output_file write_sexps ->
       don't_wait_for (
         let main ifd ofd =
           ifd >>= fun ifd ->
           let ofd = Writer.create ofd in
           Mailbox.With_pipe.t_of_fd ifd >>= fun mailbox ->
           if write_sexps then
             Pipe.iter mailbox
               ~f:(fun message ->
                 let sexp = Mailbox.Message.sexp_of_t message in
                 Print.fprintf ofd "%s" (Sexplib.Pre_sexp.to_string_hum sexp);
                 Print.fprintf ofd "\n\n";
                 return ())
           else
             begin
               Pipe.iter mailbox
                 ~f:(fun message ->
                   Print.fprintf ofd "%s" (Mailbox.Message.to_string message);
                   return ())
             end
             >>= fun () ->
             Print.fprintf ofd "%!";
             Writer.close ofd

         in
         let main' ofd =
           match input_file with
           | Some fname -> main (Fd.of_in_channel_auto (In_channel.create fname)) ofd
           | None       -> main (return (Fd.stdin ())) ofd
         in
         match ignore output_file; None with
         | Some fname ->
           Fd.of_out_channel_auto (Out_channel.create fname)
           >>= fun ofd ->
           main' ofd
         | None       -> main' (Fd.stdout ())
           >>= fun () ->
           return (shutdown 0)
       );
       never_returns (Scheduler.go ())

    ) |> Command.run
