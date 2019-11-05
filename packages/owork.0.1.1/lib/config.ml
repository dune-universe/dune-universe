(** A type to store the user-provided arguments. *)
type t =
  { work_duration: Duration.t
  ; short_break_duration: Duration.t
  ; long_break_duration: Duration.t
  ; number_work_sessions: int
  ; notify_script: string
  ; socket_file: string }
[@@deriving make, show]
