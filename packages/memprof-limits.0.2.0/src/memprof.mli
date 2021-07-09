(** Use this reimplementation of the Memprof interface if you need to
    profile a program that uses memprof-limits.

    Note: the expectancy (1/[sampling_rate]) provided in
    [Memprof.start] is rounded to the nearest integer for the accuracy
    of allocation limits accounting. *)

include module type of Stdlib.Gc.Memprof
