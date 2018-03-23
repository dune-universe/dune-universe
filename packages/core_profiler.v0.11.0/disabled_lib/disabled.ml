open! Core

module Profiler = struct
  let is_enabled = false

  let safe_to_delay () = ()
  let dump_stats () = ()

  let configure
      ?don't_require_core_profiler_env:_
      ?offline_profiler_data_file:_
      ?online_print_time_interval_secs:_
      ?online_print_by_default:_
      () = ()
end


module Timer = struct
  type t = unit
  type probe = t

  let create ~name:_ = ()
  let record _id = ()

  module Group = struct
    type t = unit

    let create ~name:_ = ()
    let add_probe _group ?sources:_ ~name:_ () = ()
    let reset _group = ()
  end
end

module Probe = struct
  type t = unit
  type probe = t

  let create ~name:_ ~units:_ = ()
  let record _id _value = ()

  module Group = struct
    type t = unit

    let create ~name:_ ~units:_ = ()
    let add_probe _group ?sources:_ ~name:_ () = ()
    let reset _group = ()
  end
end

module Delta_timer = struct
  type state = unit
  type t = unit

  let create ~name:_ = ()

  let stateless_start _t = ()
  let stateless_stop  _t _state = ()

  let start _t = ()
  let stop _t = ()
  let pause _t = ()
  let record _t = ()

  let wrap_sync  _t f x       = f x
  let wrap_sync2 _t f x y     = f x y
  let wrap_sync3 _t f x y z   = f x y z
  let wrap_sync4 _t f x y z w = f x y z w

  (* let wrap_async _t f x = f x *)
end

module Delta_probe = struct
  type state = unit
  type t = unit

  let create ~name:_ ~units:_ = ()
  let stateless_start _t _value = ()
  let stateless_stop  _t _state _value = ()

  let start _t _value = ()
  let stop  _t _value = ()
  let pause _t _value = ()
  let record _t = ()
end
