val notify : Config.t -> State.t -> unit
(** [notify config state] either calls the user specified notify-script in the config or uses notify-send. It calls the user script with the state as a string. The call is asynchronous. *)
