module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

module Auth (Keys : Auth_key) : sig
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs | Users
end

module Response_headers : sig
  type t
  val content_type : t -> string option
  val date : t -> string option
  val etag : t -> string option
  val x_ms_activity_id : t -> string option
  val x_ms_alt_content_path : t -> string option
  val x_ms_continuation : t -> string option
  val x_ms_item_count : t -> string option
  val x_ms_request_charge : t -> string option
  val x_ms_resource_quota : t -> string option
  val x_ms_resource_usage : t -> string option
  val x_ms_retry_after_ms : t -> string option
  val x_ms_schemaversion : t -> string option
  val x_ms_serviceversion : t -> string option
  val x_ms_session_token : t -> string option
end

module Database (Auth_key : Auth_key) : sig

  val get_code : Cohttp.Response.t -> int
  val list_databases : unit -> (int * Json_converter_t.list_databases) Lwt.t
  val create : string -> (int * Json_converter_t.database option) Lwt.t
  val create_if_not_exists : string -> (int * Json_converter_t.database option) Lwt.t
  val get : string -> (int * Json_converter_t.database option) Lwt.t
  val delete : string -> int Lwt.t
  module Collection :
  sig
    val list : string -> (int * Json_converter_t.list_collections) Lwt.t
    val create :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      string ->
      string ->
      (int * Json_converter_t.collection option) Lwt.t
    val create_if_not_exists :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      string ->
      string ->
      (int * Json_converter_t.collection option) Lwt.t
    val get :
      string -> string -> (int * Json_converter_t.collection option) Lwt.t
    val delete : string -> string -> int Lwt.t
    module Document :
    sig
      type indexing_directive = Include | Exclude
      val create :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        string ->
        string ->
        string ->
        (int * Json_converter_t.collection option) Lwt.t
      type list_result_meta_data = {
        rid: string;
        self: string;
        etag: string;
        ts: int;
        attachments: string;
      }
      type list_result = {
          rid: string;
          documents: (string * list_result_meta_data) list;
          count: int;
        }
      val list :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?a_im:bool ->
        ?if_none_match:string ->
        ?partition_key_range_id:string ->
        string -> string -> (int * Response_headers.t * list_result option) Lwt.t
      type consistency_level = Strong | Bounded | Session | Eventual
      val string_of_consistency_level : consistency_level -> string
      val get :
        ?if_none_match:string ->
        ?partition_key:string ->
        ?consistency_level:consistency_level ->
        ?session_token:string ->
        string -> string -> string -> (int * string) Lwt.t
      val replace :
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?if_match:string ->
        string ->
        string -> string -> string -> (int * Cohttp_lwt.Body.t) Lwt.t
      val delete :
         ?partition_key:string ->
         string -> string -> string -> int Lwt.t
      val query :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?is_partition:bool ->
        string ->
        string -> Json_converter_t.query -> (int * Response_headers.t * list_result option) Lwt.t
    end
  end

  module User : sig
    val create : string -> string -> (int * Json_converter_t.user option) Lwt.t
    val list : string -> (int * Json_converter_t.list_users) Lwt.t
    val get : string -> string -> (int * Json_converter_t.user option) Lwt.t
    val replace : string -> string -> string -> (int * Json_converter_t.user option) Lwt.t
    (* [replace dbname oldname newname] will replace the user name from oldname to newname *)
    val delete : string -> string -> int Lwt.t
  end
end
