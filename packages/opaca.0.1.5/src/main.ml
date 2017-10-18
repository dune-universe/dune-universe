

module Cmds: sig val parse : unit -> unit end = struct
              
  let disp_first () =
    match String.lowercase_ascii Sys.argv.(1) with
    | "help" | "-help" | "-h" -> Help.print ()
    | "new"                   -> New.scaffold ()
    | _ -> print_endline @@ "invalid command: " ^ Sys.argv.(1); Help.print ()

  let parse () =
    if Array.length Sys.argv > 1 then
      disp_first ()
    else
      Help.print ()

end

let () = Cmds.parse ();;
