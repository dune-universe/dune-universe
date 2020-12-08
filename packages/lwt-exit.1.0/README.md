# Lwt-exit

Lwt-exit is a somewhat opinionated library for handling signals and clean exit
in programs that use Lwt for concurrency.


## Installation

Lwt-exit is available through opam:

    opam install lwt-exit


## Use

There are two sides to Lwt-exit: the cleaning up of resources when the program
exits and the handling of signals whilst the program is running. These two sides
are somewhat indepenedet, but note that signal handling can trigger an exit
which triggers the cleaning up of resources.

You can set up resource clean-up as follows:

    let output_socket = .. (* allocate resource *) in
    let _ =
       Lwt_exit.register_clean_up_callback
          ~loc:__LOC__ (* used if needed for error reporting *)
          (fun exit_code ->
             .. (* clean-up resource *)
          )
    in

You can trigger an exit from within your own program. This is similar to using
`Stdlib.exit` but it will ensure the clean-up callbacks are executed.

    match message with
    | `Payload m -> process m
    | `Stop -> Lwt_exit.exit_and_raise 0 (* exit-code 0 *)
    | `Abort -> Lwt_exit.exit_and_raise 1 (* exit-code 1 *)

In addition, you can set up signal handlers which may trigger exits on their
own.

    let signal_setup = make_signal_setup ~soft:[..] ~hard:[]

Finally, you must wrap the main promise of your program:

    let () = Lwt_main.run @@ Lwt_exit.wrap_and_exit ~signal_setup @@ ..

Check the documentation inline in the mli (`src/lwt_exit.mli`) or
[online](https://nomadic-labs.gitlab.io/lwt-exit/) for more information,
advanced uses and additional examples.

## Historical notes

The previous versions of Lwt-exit were developped as part of the
[Tezos project](https://gitlab.com/tezos/tezos) and is now used as an external
depenedency.
