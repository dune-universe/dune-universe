open Core
open Async
open Async_extended.Std

let rec fib n =
  if n <= 1 then 1
  else fib (n - 1) + fib (n - 2)
;;

let print_fib n fib_n =
  printf "Fibonacci number %i: %i\n" n fib_n
;;

let print_error e =
  printf !"%{Error.to_string_hum}\n" e
;;

let fork_and_calc_fib n =
  Run_in_fork.run_in_fork
    ~bin_t:Int.bin_t
    ~f:(fun () -> fib n)
    ()
  >>| function
  | Error e -> print_error e
  | Ok result -> print_fib n result
;;

module Result = struct
  type t = int * int [@@deriving bin_io]
end

let fork_and_calc_fib_multi n c =
  Run_in_fork.run_in_fork_multiple
    ~bin_t:Result.bin_t
    ~f:(fun ~write ->
      List.init n ~f:(fun i -> i + c)
      |> List.iter ~f:(fun i -> write (i, fib i)))
    ()
;;

let fork_and_calc_fib_multi_to_pipe_close n =
  fork_and_calc_fib_multi n 40
  |> Pipe.iter_without_pushback ~f:(function
    | Part (input,result) -> print_fib input result
    | Err e               -> print_error e
    | Done                -> printf "Successfully completed.\n")
;;

let fork_and_calc_fib_multi_close_pipe_early n =
  let reader = fork_and_calc_fib_multi n 40 in
  Pipe.iter_without_pushback reader ~f:(function
    | Part (input,result) ->
      print_fib input result;
      if input = 44 then begin
        printf "Closing pipe reader...\n";
        Pipe.close_read reader;
      end
    | Err e -> print_error e
    | Done -> failwith "Unexpectedly completed.\n")
;;

let main () =
  Clock.every (Time.Span.of_sec 1.) (fun () ->
    printf !"Tick at %{Time}\n" (Time.now ()));
  printf "Testing [run_in_fork]...\n\n";
  fork_and_calc_fib 47
  >>= fun () ->
  printf "Done.\n\n\n";
  printf "Testing [run_in_fork_multiple]...\n\n";
  fork_and_calc_fib_multi_to_pipe_close 6
  >>= fun () ->
  printf "Done.\n\n\n";
  printf "Testing [run_in_fork_multiple] with early pipe close...\n\n";
  fork_and_calc_fib_multi_close_pipe_early 6
  >>| fun () ->
  printf "Done.\n";
  Shutdown.shutdown 0
;;

let () =
  Command.run
    (Command.async_spec
       Command.Spec.empty
       ~summary:"Do long running fibonacci calcs in a forked process, while \n\
                 ticking every second to show we never block the async thread"
       main)
