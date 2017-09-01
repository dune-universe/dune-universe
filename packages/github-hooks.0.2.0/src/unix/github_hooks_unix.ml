module Time = struct
  type t = float
  let min = 0.
  let now = Unix.gettimeofday
end

module Server = struct
  include Cohttp_lwt_unix.Server
  type mode = Conduit_lwt_unix.server
  let create mode t = create ~mode:(mode :> mode) t
end

module Make = Github_hooks.Make(Time)(Github)(Server)
