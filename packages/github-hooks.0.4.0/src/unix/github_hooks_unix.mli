open Github_hooks

module Time: TIME

module Server: SERVER with type mode = Conduit_lwt_unix.server
  [@@ warning "-34"]

module Make (Conf: CONFIGURATION): HOOKS with type token = Github.Token.t
