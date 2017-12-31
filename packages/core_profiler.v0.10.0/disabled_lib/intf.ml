(** For a tutorial on using [Core_profiler] please look at:
    [http://docs/programming/performance/core_profiler.html]

    This interface file defines the [Profiler_intf] interface, which has three
    implementations:

    (1) By opening [Core_profiler_disabled.Std] you get an implementation of the interface
    where the profiling functions are no-ops.

    (2) By opening [Core_profiler.Std_online] you get an implementation where the
    profiling stats like mean, stddev etc are maintained online, i.e. in-process with the
    process that is being profiled.  Running the online profiler causes the program to
    print a table of stats periodically to stdout as your program runs.

    (3) By opening [Core_profiler.Std_offline] you get the implementation where the
    profiling data is held in-memory in a buffer and is written out to a file at process
    exit and the stats can be analyzed offline.  For most programs this is the preferred
    approach to profiling.

    Broadly, there are two ways to collect metrics using [Core_profiler]:

    (1) One can collect time-stamped metrics at various points in your programs using
    [Probe]s.  The metrics are integers that represent some application specific quantity,
    for example the length of some list, the number of allocated words on the heap etc.

    (2) Alternately, one can use [Timer]s to collect time stamps without recording any
    associated metric.  [Timer]s are a special kind of [Probe] that are useful when one
    only wants to measure *when something happened* and there is no associated quantity to
    measure.  [Timer]s are strictly less general than [Probe]s, but are slightly more
    efficient. *)

open! Core

module type Probe = sig
  type t
  type probe = t

  (** The [*_args] below are instantiated differently for [Timer]s and [Probe]s. See
      [Profiler_intf] below. *)
  type 'a create_args
  type 'a record_args

  (** Create a timer or probe that isn't in a group. *)
  val create : name:string -> t create_args

  (** [record] a particular data sample. *)
  val record : t record_args

  (** A [Group] provides a way of grouping multiple probes (or timers).  Once grouped, one
      can measure stats between members of a group -- i.e. the time it takes to get from
      one probe to the other in the group, or the change in a metric between two probes in
      a group.

      [Core_profiler] supports a path query syntax where one can ask for stats about
      *paths* in the time series of data.  The idea of a group is best explained by an
      example, which you can find in the tutorial. *)
  module Group : sig
    type t

    val create : name:string -> t create_args

    (** [sources] should be a list of probes, specifying the edges that we care about.
        (i.e., we care about (s, this) for all s in [sources]).  When using online
        profiling, if no sources are specified no, stats will be collected.

        For offline profiling sources are less relevant.  All the probe information is
        collected in the output and sources provides a default configuration to the
        profiler_tool.  If no sources are specified, the offline tool will fall back to
        the default of "all two-probe direct paths" of a group. *)
    val add_probe : t -> ?sources:probe array -> name:string -> unit -> probe

    (** Resetting a group avoids path/delta calculation across the reset.  This shouldn't
        be necessary in simple cases if you specify the edges you care about, via
        [sources] or otherwise.  In more complex cases with cycles, you will need to call
        this at the start or end of the function you are instrumenting. *)
    val reset : t -> unit
  end
end

type 'a timer_create_args = 'a
type 'a timer_record_args = 'a -> unit

type 'a probe_create_args = units:Profiler_units.t -> 'a
type 'a probe_record_args = 'a -> int -> unit

(** All three profilers -- the disabled one, the online one and the offline one --
    implement [Profiler_intf]. *)
module type Profiler_intf = sig

  module Profiler : sig
    (** [is_enabled] can be used to "guard" expensive computations done while recording a
        metric.  For example:

        {[
          Probe.record len <some computation>
        ]}

        If <some computation> is just a variable reference it is free (when using
        [Core_profiler_disabled]).  However, if it involves some actual work, it is better
        to write:

        {[
          if Profiler.is_enabled then Probe.record len <some computation>
        ]}

        When using online or offline profiling, the boolean is constant [true] and with
        disabled profiling, the boolean is [false]. *)
    val is_enabled : bool

    (** [configure] lets one set various profiler parameters programmatically.

        - [don't_require_core_profiler_env] : To protect against Core_profiler being
          enabled in production, it will check the environment variable [CORE_PROFILER]
          whenever you try to create the first [Timer] or [Probe].  Setting
          [don't_require_core_profiler_env] disables raising an exception if the
          [CORE_PROFILER] environment variable is not set.

          You need to call this before any [Timer] or [Probe] has been created.  If you
          set [don't_require_core_profiler_env] after a [Timer] or [Probe] has been
          created, then it will raise an exception if the value you are trying to set
          disagrees with that which was read from the environment.

        - [offline_profiler_data_file] : This specifies the name of the data file to use.
          By default this is "profiler.dat".

        - [online_print_time_interval_secs] : This is the rate at which stats should be
          printed by the online profiler.  Stats may not be printed at this rate is one
          does not call [at] or [safe_to_delay] periodically.

        - [online_print_by_default] : Setting this to [false] disables printing stats
          every time interval.  One can print stats by explicitly calling [dump_stats].

        The environment variable [CORE_PROFILER] can be used to configure the
        app. Also see [core_profiler_env_help_string] below. *)
    val configure
      :  ?don't_require_core_profiler_env : unit
      -> ?offline_profiler_data_file      : string
      -> ?online_print_time_interval_secs : int
      -> ?online_print_by_default         : bool
      -> unit
      -> unit

    (** There are several slow operations that may happen occasionally when calling
        [record]: allocation, [Time_stamp_counter] calibration, etc.  [safe_to_delay]
        checks if they will be necessary soon, and does them in advance.  If possible,
        call this (fairly regularly) from a time-insensitive point in code (or at least,
        outside any deltas / groups) to reduce the number of spurious jumps in time
        deltas.  If you know for certain that you will be using [Core_profiler], you also
        probably want to call this at startup, to perform the first allocation. *)
    val safe_to_delay : unit -> unit

    (** In the online profiler, [dump_stats] prints a table of stats -- this is the same
        table that is printed periodically and this function gives the user the option to
        disable the automatic printing and take control of the printing process.

        In the offline profiler, [dump_stats] writes out all the collected stats so
        far.  This normally happens [at_exit] and this function lets the programmer dump
        the stats earlier. *)
    val dump_stats : unit -> unit
  end

  (** A [Timer] contains only a time stamp and no extra information; however, it is useful
      because (in [Offline]) the current time is recorded when measurements are made. *)
  module Timer : Probe
    with type 'a create_args := 'a timer_create_args
     and type 'a record_args := 'a timer_record_args

  (** A [Probe] records some integer value that is passed to [at] along with a
      timestamp. *)
  module Probe : Probe
    with type 'a create_args := 'a probe_create_args
     and type 'a record_args := 'a probe_record_args

  (** [Delta_probe] is an optimized two-probe group to track changes to some counter. *)
  module Delta_probe : sig
    type t
    type state

    val create : name:string -> units:Profiler_units.t -> t

    (** To measure changes in a value, one can call [start] followed by a call [stop]
        after some time.  The call to [stop] will record the delta.  Calls to
        [start]/[stop] must be interleaved for each [t].

        Calling [pause] in place of [stop] causes [t] to accumulate, but not record, the
        delta.  [start] and [pause] can then be interleaved multiple times.  Afterwards,
        calling [record] will record the sum of the deltas between each [start]/[pause],
        and reset [t].

        Valid sequences should satisfy this regular expression:

        {v
          start;(pause;start;)*((pause;record;)|stop;)
        v}

        Calling these functions out of order will cause bad data to be recorded. This API
        does not raise exceptions, so one will not be warned of errors.

        For each [t], there are two valid sequences of calls.  The first is calling
        [start] then [stop].  The second is calling [start] then [pause] an arbitrary
        number of times, and ending with [record]. *)
    val start : t -> int -> unit
    val stop  : t -> int -> unit
    val pause : t -> int -> unit
    val record: t -> unit

    (** These are non-stateful and can be used in Async, wherein multiple jobs might call
        [stateless_start] before the corresponding [stop_async] is called.  One can use
        [stateless_start] and [stateless_stop] to wrap async functions roughly like the
        following.  This function cannot be provided as part of the [Core_profiler]
        library because we'd like the library to be usable in [Async] and hence now depend
        on it.

        {[
          let wrap_async t f x =
            let state = stateless_start t (Gc.minor_words ()) in
            try_with ~run:`Now (fun () -> f x)
            >>= fun res ->
            stateless_stop t state (Gc.minor_words ());
            match res with
            | Ok x -> return x
            | Error ex -> Exn.reraise ex "Core_profiler wrap_async"
        ]}

        The stateless API does not support pausing.  This is because state would require
        memory allocation if it supported accumulating the counter. *)
    val stateless_start : t          -> int -> state
    val stateless_stop  : t -> state -> int -> unit
  end

  (** [Delta_timer] is an optimized two-probe group to track time differences between
      calls to [start] and [stop]. *)
  module Delta_timer : sig
    type t
    type state

    val create : name:string -> t

    val start : t -> unit
    val stop  : t -> unit
    val pause : t -> unit
    val record: t -> unit

    val stateless_start : t -> state
    val stateless_stop  : t -> state -> unit

    (** Typically partially applied (the first two arguments) to produce a 'wrapped'
        function.  This behaves like the identity function on functions, except it times
        the inner function. *)
    val wrap_sync  : t -> ('a -> 'b)                    -> 'a -> 'b
    val wrap_sync2 : t -> ('a -> 'b -> 'c)              -> 'a -> 'b -> 'c
    val wrap_sync3 : t -> ('a -> 'b -> 'c -> 'd)        -> 'a -> 'b -> 'c -> 'd
    val wrap_sync4 : t -> ('a -> 'b -> 'c -> 'd -> 'e)  -> 'a -> 'b -> 'c -> 'd -> 'e
  end
end

let core_profiler_env_help_string = "
    Assign a value to environment variable CORE_PROFILER in order to proceed,
    or replace references to [Core_profiler] with [Core_profiler_disabled].

    The environment variable [CORE_PROFILER] must be set to run a program that uses the
    [Core_profiler] library.  This check is meant to protect us from accidentally
    deploying binaries with profiling into production.  The variable can contain zero or
    more name-value pairs.

    Syntax:
      CORE_PROFILER=[name=value(,name=value)+] <command to run>

    i.e. commas are separators and invalid names will simply be ignored.

    The valid names are:

    OUTPUT_FILE=<filename>
    This determines the output filename for the offline profiler.

    PRINT_INTERVAL=<int>
    This determines the integer number of seconds between outputing online stats
    summaries. Setting print interval does not affect the offline profiler.

    PRINT_ENABLED=<true|false>
    Setting this to false disables printing in the online profiler. One can use this to
    control printing by calling the [Profiler.dump_stats] function.

    Example:
    CORE_PROFILER=PRINT_INTERVAL=3 myprog.exe

    Example:
    CORE_PROFILER= myprog.exe
"


