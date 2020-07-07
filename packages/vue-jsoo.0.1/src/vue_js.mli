open Mjs

type 'a vue = 'a t

(** Functor to create a vue with typechek of input data *)
module Make(S: sig
    (** data class type of the vue *)
    type data

    (** output class type : data + computed + ... *)
    type all

    (** id of the container (without the `#`) *)
    val id : string
  end) : sig

  include Vue_component.Tables_S with type all = S.all

  (** create the vue object from previoulsy added data/methods/watchers/computed *)
  val init :
    ?export:bool ->
    ?data:S.data t ->
    ?show:bool ->
    ?suffix:string ->
    ?router:Vue_router.router ->
    ?store:top Vuex.store ->
    unit -> all vue

  (** get the vue instance with the reference *)
  val app : unit -> all vue

end

(** generic maker *)
val make :
  ?computed:('all t -> any optdef) table_cons ->
  ?watch:('all t -> any -> any -> any) table_cons ->
  ?methods:any table_cons ->
  ?data:'data t ->
  ?components:(top, top) Vue_component.Internal.component_arg t table_cons ->
  ?router:Vue_router.router ->
  ?store:top Vuex.store ->
  string ->
  'all vue

(** get a $ prop element of the vue *)
val get_prop : 'a vue -> string -> 'b

(** get an element of $ref *)
val get_ref : 'a vue -> string -> 'b

(** get the router of the vue *)
val get_router : 'a vue -> Vue_router.router

(** get the store of the vue *)
val get_store : 'a vue -> 'b Vuex.store

(** get the current route *)
val get_route : 'a vue -> ('b, 'c) Vue_router.Internal.route t


(** SPA Functor *)
module SPA(S : sig
    type data
    type state
    type all
    val name : string
    val template : string
    val props : string list
  end) : sig

  type data = S.data
  include Vuex.Module_S with type state = S.state
  include Vue_component.Tables_S with type all = S.all
  include Vue_router.Tables_S

  val init :
      ?getters:(string * (state t -> any)) list ->
      ?mutations:(string * (state t -> any -> unit)) list ->
      ?actions:(string * (state Vuex.action_input -> any -> unit)) list ->
      ?modules:(string * top Vuex.module_obj) list ->
      ?computed:(string * (all t -> any optdef)) list ->
      ?methods:(string * any) list ->
      ?watch:(string * (all t -> any -> any -> any)) list ->
      ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
      ?children:(top, top) Vue_router.Internal.route t list ->
      ?hook:(Vue_router.push_obj -> Vue_router.push_obj -> (unit -> unit) -> unit) ->
      ?path:string ->
      data:data t -> state:state t -> unit ->
      string * state Vuex.module_obj * (data, all) Vue_router.Internal.route t

  val store : unit -> state Vuex.module_obj
  val route : unit -> (data, all) Vue_router.Internal.route t
end

(** Root Functor *)
module Root(S : sig
    type data
    type state
    type all
    val id : string
  end) : sig

  type data = S.data
  include Vuex.Make_S with type state = S.state
  include Vue_component.Tables_S with type all = S.all
  include Vue_router.Tables_S

  val init :
      ?getters:(string * (state t -> any)) list ->
      ?mutations:(string * (state t -> any -> unit)) list ->
      ?actions:(string * (state Vuex.action_input -> any -> unit)) list ->
      ?modules:(string * top Vuex.module_obj) list ->
      ?computed:(string * (all t -> any optdef)) list ->
      ?methods:(string * any) list ->
      ?watch:(string * (all t -> any -> any -> any)) list ->
      ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
      ?routes:(top, top) Vue_router.Internal.route t list ->
      ?strict:bool -> ?devtools:bool -> ?plugins:any list -> ?mode:string ->
      ?show:bool -> ?export:bool -> ?suffix:string ->
      data:data t -> state:state t -> unit ->
      all vue

  val router : unit -> Vue_router.router
  val app : unit -> all vue
end
