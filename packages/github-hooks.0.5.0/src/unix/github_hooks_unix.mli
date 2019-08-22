open Github_hooks

module Time: TIME

module Server: SERVER with
  type tcp_config = Conduit_lwt_unix.tcp_config and
  type mode = Conduit_lwt_unix.server

module Make (Conf: CONFIGURATION): HOOKS with type token = Github.Token.t
