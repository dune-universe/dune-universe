type t = Github_branch of Branch.t | Github_PR of Pull_request.t

val switch_target : t -> Github_client.t -> (string, [ `Unknown ]) result

val switch_description : t -> Github_client.t -> string

val global_switch_name : t -> Switch_name.t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val parse : string -> (t, [ `Unknown ]) result
