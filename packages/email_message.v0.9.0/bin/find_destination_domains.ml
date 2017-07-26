open Async
open Core
open Email_message

module Email_Address = struct
  type t = string
end

let x_js_compliance_to_hash astring =
  let _returnme = String.Table.create () ~size:4 in
  List.map ~f:(String.split ~on:' ') [astring]
  |> List.hd_exn
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(String.split ~on:'=')
  |> List.iter ~f:(function
    | [key; data] -> Hashtbl.add_multi _returnme ~key ~data
    | _ -> ()
  );
  _returnme

let destination_addresses message =
  Option.Monad_infix.(
    Email.last_header message.Mailbox.Message.email "x-js-compliance"
    >>= fun compliance_hdr ->
    Hashtbl.find (x_js_compliance_to_hash compliance_hdr) "recipient" )

let easy_get_header message header_name =
  let headers = Email.headers message.Mailbox.Message.email in
  match Headers.last headers header_name with
  | Some data -> data
  | None -> ""
;;

let print_all_header_names  message =
  let headers = Email.headers message.Mailbox.Message.email in
  let _foo = Headers.names headers in
  let sexp = List.sexp_of_t Headers.Name.sexp_of_t _foo in
  Sexp.to_string_hum ~indent:1 sexp


let () =
  Command.basic ~summary:"Mailbox parser tester"
    Command.Spec.(empty
                  +> flag ~aliases:["i"] "input-file" (optional string)
                       ~doc:"file Mailbox to parse."
                  +> flag ~aliases:["o"] "output-file" (optional string)
                       ~doc:"file File to write the mailbox to.\nBROKEN, use > redirection instead"
                  +> flag ~aliases:["s"] "sexp" (no_arg)
                       ~doc:"Output S-expressions instead of strings"
                  +> flag "domain" (optional string)
                       ~doc:"Domain Only print messages with this domain in the X-Js-Compliance/recipient field.")

    (fun input_file output_file write_sexps domain ->
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
                   begin
                     match domain with
                     | Some dest_domain ->
                       begin
                         match destination_addresses message with
                         | Some dests -> let matching_dests = List.filter
                                                                ~f:(fun s -> String.is_suffix ~suffix:dest_domain s) dests in
                           begin
                             match matching_dests with
                             | [] -> ()
                             | stuff -> Print.printf "Found matching dests: %s\n%!" (String.concat
                                                                                       ~sep:" | " stuff);
                           end

                         | None -> Print.printf "No dests?! Message-id: %s Headers: %s \n%!"
                                     (easy_get_header message "message-id")
                                     (print_all_header_names message);
                       end;
                     |None -> ()
                   end;

                   return ())
              (*
                    Print.fprintf ofd "%s" (Mailbox.Message.to_string message); *)
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
