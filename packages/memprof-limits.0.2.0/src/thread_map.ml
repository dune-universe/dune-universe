include Thread_map_core

let with_value tls ~value ~scope =
  let old_value = get tls in
  (* FIXME: needs proper masking here as there is a race between
     resources and asynchronous exceptions. For now, it is
     exception-safe only for exceptions arising from Memprof_callbacks. *)
  Masking.with_resource
    ~acquire:(fun () -> set tls (Some value)) ()
    ~scope
    ~release:(fun () -> set tls old_value)
