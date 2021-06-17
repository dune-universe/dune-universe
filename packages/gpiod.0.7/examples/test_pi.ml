open Printf

exception InternalError of string

let open_line chip nr =
  match Gpiod.chip_get_line (Some chip) nr with
  | Some v -> v
  | None -> raise (InternalError "Could not open a line")
;;

let print_gpiod_version () =
  let s = Gpiod.version_string () in
  match s with
  | Some s -> printf "done %s\n" s
  | None -> raise (InternalError "version_string failed")
;;

let gpio_test_pi () =
  printf "preparing call\n";
  print_gpiod_version ();
  let chip =
    match Gpiod.chip_open_by_name (Some "gpiochip0") with
    | Some v -> v
    | None -> raise (InternalError "Could not open gpiochip0")
  in
  printf "Opening lines\n";
  let lineRed = open_line chip 24 in
  let lineYellow = open_line chip 25 in
  printf "Finished setting up\n";
  let _ = Gpiod.line_request_output (Some lineRed) (Some "example1") 0 in
  let _ = Gpiod.line_request_output (Some lineYellow) (Some "example1") 0 in
  let _ = Gpiod.line_set_value (Some lineRed) 1 in
  let _ = Gpiod.line_set_value (Some lineYellow) 1 in
  ()
;;

let () = gpio_test_pi ()
