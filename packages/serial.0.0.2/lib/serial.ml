open Serial_intf
open Lwt.Infix

module Make (T : Serial_config_type) = struct
	let port = T.port
	let baud_rate = T.baud_rate

	module Private = struct

		let fd = Lwt_main.run begin
			Lwt_unix.openfile port [Unix.O_RDWR; Unix.O_NONBLOCK] 0o000
		end

		let in_channel = Lwt_io.of_fd fd ~mode:Lwt_io.input
		let out_channel  = Lwt_io.of_fd fd ~mode:Lwt_io.output
	end

	let set_baud_rate baud_rate =
		Lwt_unix.tcgetattr Private.fd >>= fun attr ->
		Lwt_unix.tcsetattr Private.fd Unix.TCSANOW
			{ attr with c_ibaud = baud_rate
			; c_obaud = baud_rate
			; c_echo = false
			; c_icanon = false
			}

	(* Initialize with desired baud rate *)
	let () = Lwt_main.run begin
		set_baud_rate baud_rate
	end

	let read_line () =
		Lwt_io.read_line Private.in_channel

	let write_line l =
		Lwt_io.fprintl Private.out_channel l

	let wait_for_line to_wait_for =
		let rec loop = function
		| Some line when line = to_wait_for ->
				Lwt.return ()
		| _ ->
			read_line () >>= fun line ->
			loop (Some line)
		in
		loop None

	let rec io_loop until =
		let read_to_stdin () =
			read_line () >>= fun line ->
			Lwt_io.printl line >>= fun () ->
			Lwt.return `Continue
		in

		let write_from_stdin () =
			Lwt_io.(read_line stdin) >>= function
				| line when Some line <> until ->
						write_line line >>= fun () ->
						Lwt.return `Continue
				| line when Some line = until -> Lwt.return `Terminate
				| _ -> assert false
		in

		Lwt.pick [read_to_stdin (); write_from_stdin ()] >>= function
		| `Continue -> io_loop until
		| `Terminate -> Lwt.return ()

end
