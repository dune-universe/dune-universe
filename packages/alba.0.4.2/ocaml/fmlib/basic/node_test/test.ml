open Fmlib
open Common

(*module Io   = Fmlib_node.Node_io

let _ =
  Io.(
    let rec step () =
      getchar >>= function
      | None -> exit 1
      | Some c ->
         putchar c >>= function
         | None -> return ()
         | Some () -> step ()
    in
    execute @@ step ())
 *)

module Io = Fmlib_node.Node_io.IO

module Out = Fmlib.Io.Output (Io)

module Pretty = Pretty_printer.Pretty (Out)


let width = 80

let pp =
  let open Pretty in
  string "Hello" <+> cut <+> string "world!" <+> cut
  <+> fill_paragraph
        "This is a paragraph which is justified left and right. It must be \
         very very long and must split over several lines. Lorem ipsum \
         blablabla and more to come. More and more and more and more and more \
         blablabla and more to come. More and more and more and more and more."
  <+> cut

module WStr = Io.Write (String_reader)
let _ =
  Io.(
    Process.execute (
        Stdout.line "Hello world" >>= fun _ ->
        WStr.write Io.stdout (String_reader.of_string "Hello 2\n") >>= fun _ ->
        Process.command_line >>= fun arr ->
        Array.fold_left
          (fun pr str ->
            pr >>= fun _ -> Stdout.line str)
          (return ())
          arr
        >>= fun _ ->
        Out.run Io.File.stdout (Pretty.run 0 width width pp)
      )
  )
