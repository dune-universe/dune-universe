## v0.11

Removed [Ref] module.

## 113.43.00

- Added `Pipe.create_reader`, which is like `Pipe.init`, but has different
  behavior w.r.t. `f` raising.  It forces the caller to choose the behavior
  on raise to avoid subtle bugs related to closing the reading end.
  The comment recommends `~close_on_exception:false`.

    val create_reader
      :  close_on_exception:bool
      -> ('a Writer.t -> unit Deferred.t)
      -> 'a Reader.t

  `Pipe.init_reader` was renamed to `Pipe.create_writer` so that the names are
  consistent.  Its behaviour was not changed: it also closes the pipe if `f`
  raises, but here this is probably what you want, so that the writer is notified
  that the reader is dead.

  Changed `Pipe.of_sequence`, which is implemented with `create_reader`,
  to use `~close_on_exception:false`.  This seems like an improvement, because
  it won't treat raising as end of sequence.

  Changed `Pipe.unfold`, which is implemented with `create_reader`, to use
  the `~close_on_exception:false`.  This shouldn't be a problem, because
  `unfold` is barely used.  And it was never specified what `f` raising
  means.  It seems fine to not treat raising as end-of-sequence, because
  `unfold` already has a way to express end-of-sequence.

  Here's some more explanation of motivation for the change.  In the
  current world, `Pipe.init` looks like this:

    let init f =
      let r, w = create () in
      don't_wait_for (Monitor.protect (fun () -> f w)
                        ~finally:(fun () -> close w; Deferred.unit));
      r
    ;;

  This means that if `f` raises, then the pipe is both closed and the
  exception is sent to the monitor active when `init` was called.

  If you have something (like `Pipe.fold_without_pushback`) consuming
  the pipe, the exception being delivered can race against the fold
  finishing, and you could end up missing it. Moreover, the race seems
  to reliably go in the direction you don't want:

    $ cat pipes.ml
    #!/j/office/app/jane-script/prod/113.34/jane-script run
    open Core.Std
    open Async.Std

    let main () =
      Monitor.try_with_or_error (fun () ->
        Pipe.init (fun _writer -> assert false)
        |> Pipe.fold_without_pushback ~init:() ~f:(fun () () -> ())
      )
      >>= fun res ->
      printf !"Result: %{sexp:unit Or_error.t}\n" res;
      exit 0

    let () =
      don't_wait_for (main ());
      never_returns (Scheduler.go ())

    $ ./pipes.ml
    Result: (Ok ())
    2015-10-21 15:42:22.494737+01:00 Error Exception raised to Monitor.try_with that already returned
      (monitor.ml.Error_
      ((exn "Assert_failure /home/toto/pipes.ml:7:30")
    `snip`

- Deprecated `Pipe.init` and replaced its uses with the semantically
  equivalent:

    Pipe.create_reader ~close_on_exception:true

- Add a generic validate command that validates config files against their type

- Add a convenience function to Delimited to read off a string

- Add `Mailbox.sexp_of_t`. It shows elements in the order they were added,
  ie like `peek { select = always_select }` would return.

- The requirement that the binary path you give to `Command_rpc` is
  slightly inconvenient and seems not very useful. Let's remove it.

- Remove the ~update argument from State_rpc.dispatch.

  Before this feature, the dispatch method for State_rpcs looked like this:

    val dispatch
      :  ('query, 'state, 'update, 'error) t
      -> Connection.t
      -> 'query
      -> update : ('state -> 'update -> 'state)
      -> ( 'state * ('state * 'update) Pipe.Reader.t * Metadata.t
         , 'error
         ) Result.t Or_error.t Deferred.t

  There are a couple of cases where having the `update` method there can be
  cumbersome:
  - sometimes the type of the state that it sent over the wire is different
  from the type that is naturally maintained on the client side.
  - sometimes the update method needs to perform some Async computation to
  return a new state.

  It feels like the method is trying to be helpful, but has more of a tendency
  of getting in the way. This feature removes this method.

  The old API may be reinstated by using a `Pipe.fold_map` as below:

    State_rpc.dispatch rpc connection query
    >>|? fun (state, update_pipe, pipe_metadata) ->
    state,
    Pipe.fold_map pipe ~init:state ~f:(fun state update ->
      let new_state = update_state state update in
      new_state, (new_state, update)),
    pipe_metadata

## 113.33.00

- Create library to fork and do calculations in child process.

- `Command_rpc.with_close` returns an `Or_error.t Deferred.t`, and indeed if it
  fails to execute the binary, it will return an `Error _` rather than raising.
  However, if it successfully executes the binary but then the handshake fails,
  it raises an exception. Successfully executing the binary but then having the
  handshake fail is I think a reasonable thing to want to catch, as it's what
  you'll get if you are starting the command rpc slave via ssh and there's some
  problem on the remote side.

  I haven't thought very hard about what happens if the subprocess dies after
  successful handshake and/or the implications of the Writer's monitor being the
  monitor in effect when `with_close` is called. I think heartbeats will kill the
  connection before this becomes an issue.

- Remove `Async_extended.Deprecated_async_bench`

  It's causing problems for the open-source release on platforms that don't have
  `Posix_clock.gettime`:

      https://github.com/janestreet/async_extended/issues/1

  Seems that a module with Deprecated in the name and no uses should just be binned.

- add a simple tcp proxy that is useful in testing for simulating network issues

## 113.24.00

- Make LTL predicates comparable by tagging and id to each one. Fixes a
  functional comparison bug.

- Switched to PPX.

- Add an mli for `async_extended/src/reader_ext.ml` and remove a
  couple of unused functions (notably `Reader_ext` had its own version of
  `Reader.read_char`).

- Add async-friendly color print

- `Ltl.eval` should close the pipe after it is done with it.

- Deleted `Async_extended.Cml`.

- Remove `Async_extended.Std.Gzip` and redirect references to `Async_gzip.Std.Gzip`.

- Update `Command_rpc.Connection` to check the program before exec'ing it.
  The filename must now be absolute, exist, and be executable.  Previously
  errors with nonexistent or nonexecutable files would only be found out
  after forking.

- Change `Command_rpc.Command` to use `Versioned_rpc.Callee_converts` instead of
  `Versioned_rpc.Both_convert` so that commands can be constructed without client-side
  conversions.  Clients remain free to use conversions or not, as appropriate.

  Removed `val rpcs` from `Callee_converts` interfaces because nothing appears to use it,
  and `Both_convert` does not provide it.  Now `Both_convert.S` can be supplied to satisfy
  `Callee_converts.S`.

- Add simple example of Command_rpc

- Add `Deferred_cache`.

- Fixing a couple of issues noticed in `Command_rpc`:

  - If `propagate_stderr` is false, the child's stderr is now drained instead of
    ignored.

  - When connections are closed, stderr is now closed as well, which prevents
    a file descriptor leak if the child process is unresponsive.

## 113.00.00

- Added a more raw interface to `Delimited.Csv`.

## 112.35.00

- Added `Ltl` module, an implementation of linear temporal logic, which
  can be used to run online queries on sequences of states.
- Added `Interactive.Job`, for printing start/done messages for multiple
  simultaneous jobs.
- Made `Any_error` be `Applicative`.
- Added `Command_rpc` support for `Versioned_rpc.Both_convert.Plain`.

## 112.24.00

- Fixed misspelling in `Command_rpc.Connection`, renaming `propogate_stderr` as
  `propagate_stderr`.

## 112.17.00

- Added `Interactive` module for terminal interaction with users by
  command-line executables.

  `Interactive` was previously in `Iron_common`.
- In `Process`, added an `?env` argument to some functions.
- Allowed `Command_rpc` implementations to access the
  `Rpc.Connection.t` used to invoke them.

  There is an option to invoke `Command_rpc` implementations via sexp
  communication instead of `Rpc`, so implementations are given a value
  of a variant type `Sexp | Bin_io of Rpc.Connection.t`.
- Added `Resource` module, which abstracts the idea of acquiring and
  releasing a handle to a resource.

## 112.06.00

- Unwound a recent change to `Mailbox` where one invocation of `receive`
  would put aside items, preventing other invocations from noticing
  them.
- Added `Delimited.Row.nth_conv_exn`, as a counterpart to
  `get_conv_exn`.
- Fixed `File_updates` handling of identical mtimes.

## 112.01.00

- Clarified an error in `Rpc_proxy`.
