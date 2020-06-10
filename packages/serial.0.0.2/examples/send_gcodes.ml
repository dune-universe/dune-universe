open Lwt.Infix

module Serial_config = struct
	let port = "/dev/ttyUSB0"
	let baud_rate = 115200
end

module Serial0 = Serial.Make(Serial_config)

let () =
	let send_command c =
		Serial0.write_line c >>= fun () ->
		Serial0.wait_for_line "ok" >>= fun () ->
		Lwt_io.printlf "ok received for %S" c
	in

	let demo =
		let commands =
			[ "G28"; "G0 Z60"; "M81"; "G4 S1"
			; "G0 Z50"; "G4 P200"; "G0 Z40"
			]
		in

		Lwt_io.printl "Starting demo...." >>= fun () ->
		Lwt_list.iter_s send_command commands >>= fun () ->
		Lwt_io.printl "Commands sent."
	in

	Lwt_main.run
		demo
