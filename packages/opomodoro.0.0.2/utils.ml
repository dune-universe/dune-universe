open Lwt.Infix

let rec get_char () =
	Lwt_io.(read_char stdin) >>= function
	| ' '  -> Lwt.return `Pause
	| 'r' -> Lwt.return `Reset
	| 'b' -> Lwt.return `BeginBreak
	| _ ->
			print_endline "\nControl with either 'r', 'b', or space.";
			get_char ()

let clock_tick () =
	Lwt_unix.sleep 1. >|= fun () ->
	`Tick
