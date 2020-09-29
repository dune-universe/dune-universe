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

  type data = S.data
  include Vue_component.Tables_S with type all = S.all

  (** create the vue object from previoulsy added data/methods/watchers/computed *)
  val init :
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
    ?export:bool ->
    ?data:data t ->
    ?show:bool ->
    ?suffix:string ->
    ?router:Vue_router.router t ->
    ?store:top Vuex.store t ->
    unit -> all vue

  (** get the vue instance with the reference *)
  val app : unit -> all vue

end

(** Functor to create a vue with typechek of input data and routes *)
module Router(S: sig
    (** data class type of the vue *)
    type data

    (** output class type : data + computed + ... *)
    type all

    (** id of the container (without the `#`) *)
    val id : string
  end) : sig

  type data = S.data
  include Vue_component.Tables_S with type all = S.all
  include Vue_router.Tables_S

  (** create the vue object from previoulsy added data/methods/watchers/computed *)
  val init :
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
    ?routes:(top, top) Vue_router.route t list ->
    ?show:bool ->
    ?export:bool ->
    ?suffix:string ->
    ?data:data t ->
    ?mode:string ->
    ?store:top Vuex.store t ->
    unit -> all vue

  (** get the vue instance with the reference *)
  val app : unit -> all vue

  (** get the router instance with the reference *)
  val router : unit -> Vue_router.router t

end

(** generic maker *)
val make :
  ?computed:('all t -> any optdef) table_cons ->
  ?watch:('all t -> any -> any -> any) table_cons ->
  ?methods:any table_cons ->
  ?data:'data t ->
  ?lifecycle:(string * ('all t -> unit)) list ->
  ?error_captured:(any -> any -> string -> bool option) ->
  ?directives:(string * (string * (
      Js_of_ocaml.Dom.element t ->
      Vue_component.Internal.binding t -> Vue_component.Internal.vnode ->
      Vue_component.Internal.vnode -> unit)) list) list ->
  ?filters:(string * (any -> any)) list ->
  ?components:(top, top) Vue_component.Internal.component_arg t Mjs.table_cons ->
  ?delimiters:string * string ->
  ?functional:bool ->
  ?model:string option * string option ->
  ?inherit_attrs:bool ->
  ?comments:bool ->
  ?router:Vue_router.router t ->
  ?store:'state Vuex.store t -> string -> 'all vue

(** set a global $ element of the vue *)
val set_global : string -> 'a -> unit

(** get a $ prop element of the vue *)
val get_prop : 'a vue -> string -> 'b

(** get an element of $ref *)
val get_ref : 'a vue -> string -> 'b

(** get the router of the vue *)
val get_router : 'a vue -> Vue_router.router t

(** get the store of the vue *)
val get_store : 'a vue -> 'b Vuex.store t

(** get the current route *)
val get_route : 'a vue -> ('b, 'c) Vue_router.route t

(** emit events *)
val emit0 : 'a vue -> string -> unit
val emit1 : 'a vue -> string -> 'b -> unit

(** SPA Functor *)
module SPA(S : sig
    type data
    type state
    type getters
    type all
    val name : string
    val template : string
    val props : string list
  end) : sig

  type data = S.data
  include Vuex.Module_S with type state = S.state and type getters = S.getters
  include Vue_component.Tables_S with type all = S.all
  include Vue_router.Tables_S

  val add_2way : ?prefix:string -> string -> unit

  val init :
    ?getters:(string * (state t -> any)) list ->
    ?mutations:(string * (state t -> any -> unit)) list ->
    ?actions:(string * ((state, getters) Vuex.action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) Vuex.module_obj t) list ->
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
    ?children:(top, top) Vue_router.Internal.route t list ->
    ?hook:(Vue_nav.push_obj -> Vue_nav.push_obj -> all Vue_nav.next) ->
    ?path:string ->
    ?data:data t -> ?state:state t ->
    ?state_to_computed:string list ->
    ?two_way:string list -> unit ->
    string * (state, getters) Vuex.module_obj t * (data, all) Vue_router.Internal.route t

  val make :
    ?getters:(string * (state t -> any)) list ->
    ?mutations:(string * (state t -> any -> unit)) list ->
    ?actions:(string * ((state, getters) Vuex.action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) Vuex.module_obj t) list ->
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
    ?children:(top, top) Vue_router.Internal.route t list ->
    ?hook:(Vue_nav.push_obj -> Vue_nav.push_obj -> all Vue_nav.next) ->
    ?path:string ->
    ?data:data t ->
    ?state:state t ->
    ?state_to_computed:string list ->
    ?two_way:string list -> unit ->
    string * (top, top) Vuex.module_obj t * (top, top) Vue_router.route t

  val store : unit -> (state, getters) Vuex.module_obj t
  val route : unit -> (data, all) Vue_router.route t
end

(** Root Functor *)
module Root(S : sig
    type data
    type state
    type getters
    type all
    val id : string
  end) : sig

  type data = S.data
  include Vuex.Make_S with type state = S.state and type getters = S.getters
  include Vue_component.Tables_S with type all = S.all
  include Vue_router.Tables_S

  val add_spa :
    string * (top, top) Vuex.module_obj t * (top, top) Vue_router.route t ->
    unit

  val add_2way : ?prefix:string -> string -> unit

  val init :
    ?getters:(string * (state t -> any)) list ->
    ?mutations:(string * (state t -> any -> unit)) list ->
    ?actions:(string * ((state, getters) Vuex.action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) Vuex.module_obj t) list ->
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
    ?routes:(top, top) Vue_router.route t list ->
    ?strict:bool -> ?devtools:bool -> ?plugins:any list -> ?mode:string ->
    ?show:bool -> ?export:bool -> ?suffix:string ->
    ?data:data t -> ?state:state t ->
    ?state_to_computed:string list -> ?two_way:string list -> unit ->
    all vue

  val router : unit -> Vue_router.router t
  val app : unit -> all vue
end
