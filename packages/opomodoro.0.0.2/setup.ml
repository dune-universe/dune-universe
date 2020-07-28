(* Store original attributes *)
let attr_orig =
	Unix.tcgetattr Unix.stdin

(* Modify stdin so that keystrokes are processed immediately *)
(* Also ensure key input is not echoed *)
let setup_stdin () =
	let attr =
		{ attr_orig with c_icanon = false
		; c_echo = false
		}
	in
	Unix.(tcsetattr stdin TCSANOW attr)

let handle_signal =
	(* Restore stdin tcattrs and exit *)
	Sys.Signal_handle ( fun _ ->
		Unix.(tcsetattr stdin TCSANOW attr_orig); (* Reset attributes *)
		ANSITerminal.(erase Screen);
		exit 0
	)

let setup () =
	setup_stdin ();
	Sys.(set_signal sigint handle_signal); (* Catch ctrl-c *)
	Sys.(set_signal sigterm handle_signal) (* Catch SIGTERM *)
