(** [Core_profiler_disabled] is the library in the base projection that exposes the same
    profiling interface as [Core_profiler].

    The profiling functions exposed by this library do not collect any profiling metrics
    and do not have any runtime performance on the programs they are embedded in.  This
    allows one to ship productions applications that include profiling code, but with the
    profiling turned off.
*)
module Profiler_units = Profiler_units

include Disabled
