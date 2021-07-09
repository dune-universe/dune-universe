(** Open {!Resource_bind} to enable the [let&] binder for resources. *)

val ( let& ) : (scope:('a -> 'b) -> 'b) -> ('a -> 'b) -> 'b
(** RAII-style notation for resources cleaned-up at the end of
    scope. Example:
    {[open Memprof_limits.Resource_bind

    let with_my_resource = Memprof_limits.Masking.with_resource ~acquire ~release

    let () =
      let& resource = with_my_resource x in
      …]} *)
