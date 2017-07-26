open Wamp

val msg_of_msgpck : Msgpck.t -> (Msgpck.t msg, string) Result.result
val msg_to_msgpck : Msgpck.t msg -> Msgpck.t

val hello : Uri.t -> role list -> Msgpck.t msg
val subscribe : ?reqid:int -> ?options: Msgpck.t dict -> Uri.t -> int * Msgpck.t msg
