type pixel =
	| F (* filled *)
	| C (* cleared *)

type icon = pixel list

let get_char_icon = function
	| '0' ->
		[ F; F; F; F; F
		; F; C; C; C; F
		; F; C; C; C; F
		; F; C; C; C; F
		; F; F; F; F; F
		]
	| '1' ->
		[ C; C; C; F; F
		; C; C; C; F; F
		; C; C; C; F; F
		; C; C; C; F; F
		; C; C; C; F; F
		]
	| '2' ->
		[ F; F; F; F; F
		; C; C; C; C; F
		; F; F; F; F; F
		; F; C; C; C; C
		; F; F; F; F; F
		]
	| '3' ->
		[ F; F; F; F; F
		; C; C; C; C; F
		; F; F; F; F; F
		; C; C; C; C; F
		; F; F; F; F; F
		]
	| '4' ->
		[ F; C; C; C; F
		; F; C; C; C; F
		; F; F; F; F; F
		; C; C; C; C; F
		; C; C; C; C; F
		]
	| '5' ->
		[ F; F; F; F; F
		; F; C; C; C; C
		; F; F; F; F; F
		; C; C; C; C; F
		; F; F; F; F; F
		]
	| '6' ->
		[ F; F; F; F; F
		; F; C; C; C; C
		; F; F; F; F; F
		; F; C; C; C; F
		; F; F; F; F; F
		]
	| '7' ->
		[ F; F; F; F; F
		; C; C; C; C; F
		; C; C; C; C; F
		; C; C; C; C; F
		; C; C; C; C; F
		]
	| '8' ->
		[ F; F; F; F; F
		; F; C; C; C; F
		; F; F; F; F; F
		; F; C; C; C; F
		; F; F; F; F; F
		]
	| '9' ->
		[ F; F; F; F; F
		; F; C; C; C; F
		; F; F; F; F; F
		; C; C; C; C; F
		; F; F; F; F; F
		]
	| ':' ->
		[ C; C; C; C; C
		; C; C; F; C; C
		; C; C; C; C; C
		; C; C; F; C; C
		; C; C; C; C; C
		]
	| _ -> failwith "Don't have an icon for that"

let get_pixel icon i =
	List.nth icon i

let get_pixel_style = function
	| C -> Args.clock_background
	| F -> Args.clock_color

let print_pixel x y pixel =
	let style = get_pixel_style pixel in
	ANSITerminal.set_cursor x y;
	ANSITerminal.print_string style " "

let print_char start_x start_y c =
	let icon = get_char_icon c in
	(* print every pixel in the icon *)
	for i = 0 to List.length icon - 1 do
		let x = (i mod Args.icon_size_x) + start_x in
		let y = (i / Args.icon_size_y) + start_y in
		let pixel = get_pixel icon i in
		print_pixel x y pixel
	done

let get_screen_mid () =
	let size_x, size_y = ANSITerminal.size () in
	(size_x / 2 + 1), (size_y / 2 + 1)

let draw_clock timestring =
	ANSITerminal.save_cursor ();
	ANSITerminal.(erase Screen);
	let mid_x, mid_y = get_screen_mid () in

	(* Print every char in the timestring *)
	String.iteri (fun i c ->
		let char_x = (i * Args.digit_width) + mid_x - Args.half_clock_width in
		let char_y = mid_y - Args.half_clock_height in
		print_char char_x char_y c
	) timestring;

	ANSITerminal.restore_cursor ()

let to_timestring time_s =
	let minutes, seconds = time_s / 60, time_s mod 60 in
	let padding = if seconds < 10 then "0" else "" in
	Printf.sprintf "%i:%s%i" minutes padding seconds

let display_time time_s =
	let to_print = to_timestring time_s in

	match ANSITerminal.size () with
	| x, y when x < Args.clock_width || y < Args.icon_size_y ->
			print_endline to_print (* Not enough space -- just print timestring *)
	| _ -> draw_clock to_print
