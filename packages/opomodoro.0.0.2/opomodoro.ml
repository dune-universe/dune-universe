type status =
	| Paused
	| CountDown
	| CountUp

type t =
	{ time_s: int
	; status: status
	}

let timer_initial =
	{ time_s = Args.work_time_s
	; status = CountDown
	}

let advance_state timer = match timer.status with
	| CountDown ->
		(
			match timer.time_s with
			| s when s <= 1 -> {time_s = 0; status = CountUp}
			| s -> {timer with time_s = s - 1}
		)
	| CountUp ->
		(
			match timer.time_s with
			| s when s >= Args.break_time_s -> {timer_initial with status = Paused}
			| s -> {timer with time_s = s + 1}
		)

	| Paused -> timer

let rec loop timer =
	let timer : t = advance_state timer in

	Printer.display_time timer.time_s;
	if timer.status = Paused then print_endline "<paused>" else ();

	let action = Lwt_main.run begin
		(* Get the result of whatever occurs first, cancel the other *)
		Lwt.pick
		[ Utils.get_char ()
		; Utils.clock_tick ()
		]
	end in

	match action with
	| `Tick -> loop timer
	| `BeginBreak -> loop {time_s = 0; status = CountUp}
	| `Pause when timer.status = Paused -> loop {timer with status = CountDown}
	| `Pause when timer.status = CountDown -> loop {timer with status = Paused}
	| `Pause -> loop timer
	| `Reset -> loop {timer_initial with status = Paused}

let () =
	Setup.setup ();
	loop timer_initial
