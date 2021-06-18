open Core
open Async

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of \
   sitting by her sister on the bank, and of having nothing to do: once or \
   twice she had peeped into the book her sister was reading, but it had no \
   pictures or conversations in it, <and what is the use of a book,> thought \
   Alice <without pictures or conversations?> So she was considering in her \
   own mind (as well as she could, for the hot day made her feel very sleepy \
   and stupid), whether the pleasure of making a daisy-chain would be worth \
   the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very \
   remarkable in that; nor did Alice think it so very much out of the way to \
   hear the Rabbit say to itself, <Oh dear! Oh dear! I shall be late!> (when \
   she thought it over afterwards, it occurred to her that she ought to have \
   wondered at this, but at the time it all seemed quite natural); but when \
   the Rabbit actually took a watch out of its waistcoat-pocket, and looked at \
   it, and then hurried on, Alice started to her feet, for it flashed across \
   her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, \
   she ran across the field after it, and fortunately was just in time to see \
   it pop down a large rabbit-hole under the hedge. In another moment down \
   went Alice after it, never once considering how in the world she was to get \
   out again. The rabbit-hole went straight on like a tunnel for some way, and \
   then dipped suddenly down, so suddenly that Alice had not a moment to think \
   about stopping herself before she found herself falling down a very deep \
   well. Either the well was very deep, or she fell very slowly, for she had \
   plenty of time as she went down to look about her and to wonder what was \
   going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the \
   sides of the well, and noticed that they were filled with cupboards......"

let text = Base_bigstring.of_string text

[@@@part "simple_server"]

let run (sock : Fd.t) =
  let service (req, _body) =
    let target = Cohttp.Request.resource req in
    let resp =
      Cohttp.Response.make
        ~headers:
          (Cohttp.Header.of_list
             [ ("Content-Length", Int.to_string (Base_bigstring.length text)) ])
        ~status:`OK ()
    in
    match target with
    | "/delay" ->
        let%map () = after Time.Span.millisecond in
        (resp, `Bigstring text)
    | _ -> return (resp, `Bigstring text)
  in
  let conn =
    H1_async.create sock ~read_buffer_size:(10 * 1024)
      ~write_buffer_size:(10 * 1024)
  in
  H1_async.run conn service

[@@@part "simple_server"]

let log_exn _ exn = Log.Global.error "%s" (Exn.to_string exn)

let run ~port ~max_accepts_per_batch =
  let (server : (Socket.Address.Inet.t, int) Tcp.Server.t) =
    Tcp.Server.create_sock_inet ~backlog:11_000 ~max_accepts_per_batch
      ~on_handler_error:(`Call log_exn) (Tcp.Where_to_listen.of_port port)
      (fun _addr sock ->
        let fd = Socket.fd sock in
        run fd)
  in
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5) >>| fun () ->
      Log.Global.printf "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()

let () =
  Command.async ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open port =
        flag "-port"
          (optional_with_default 8080 int)
          ~doc:" Port to listen on (default 8080)"
      and max_accepts_per_batch =
        flag "-accepts"
          (optional_with_default 1 int)
          ~doc:" Max connections per accept call"
      in
      fun () -> run ~port ~max_accepts_per_batch)
  |> Command.run
