open Wamp

val msg_of_yojson : Yojson.Safe.json -> (Yojson.Safe.json msg, string) Result.result
val msg_to_yojson : Yojson.Safe.json msg -> Yojson.Safe.json

val hello : Uri.t -> role list -> Yojson.Safe.json msg
val subscribe : ?reqid:int -> ?options: Yojson.Safe.json dict -> Uri.t -> int * Yojson.Safe.json msg
