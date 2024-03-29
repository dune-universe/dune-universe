open Async
include module type of Guild_id_t

val ban_user : id:Snowflake.t -> ?reason:string -> ?days:int -> t -> unit Deferred.Or_error.t
val create_emoji : name:string -> image:string -> t -> Emoji.t Deferred.Or_error.t
val create_role :
    name:string ->
    ?colour:int ->
    ?permissions:int ->
    ?hoist:bool ->
    ?mentionable:bool ->
    t ->
    Role_t.t Deferred.Or_error.t
val create_channel : mode:[ `Text | `Voice | `Category ] -> name:string -> t -> Channel_t.t Deferred.Or_error.t
val delete : t -> unit Deferred.Or_error.t
val get_ban : id:Snowflake.t -> t -> Ban_t.t Deferred.Or_error.t
val get_bans : t -> Ban_t.t list Deferred.Or_error.t
val get_emoji : id:Snowflake.t -> t -> Emoji.t Deferred.Or_error.t
val get_invites : t -> Yojson.Safe.json Deferred.Or_error.t
val get_prune_count : days:int -> t -> int Deferred.Or_error.t
val get_webhooks : t -> Yojson.Safe.json Deferred.Or_error.t
val kick_user : id:Snowflake.t -> ?reason:string -> t -> unit Deferred.Or_error.t
val leave : t -> unit Deferred.Or_error.t
val list_voice_regions : t -> Yojson.Safe.json Deferred.Or_error.t
val prune : days:int -> t -> int Deferred.Or_error.t
val request_members : t -> Member_t.t list Deferred.Or_error.t
val set_afk_channel : id:Snowflake.t -> t -> Guild_t.t Deferred.Or_error.t
val set_afk_timeout : timeout:int -> t -> Guild_t.t Deferred.Or_error.t
val set_name : name:string -> t -> Guild_t.t Deferred.Or_error.t
val set_icon : icon:string -> t -> Guild_t.t Deferred.Or_error.t
val unban_user : id:Snowflake.t -> ?reason:string -> t -> unit Deferred.Or_error.t