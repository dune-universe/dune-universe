open Mjs
open Vue_nav

(** redirect to path or to named route *)
type redirect = Rd of string | Rd_name of string

(** props type : boolean, object or function *)
type props = PrBool of bool | PrObj of any | PrFn of (push_obj -> any)

(** Internal JS module *)
module Internal : sig

  (** route javascript object *)
  class type ['a, 'b] route = object
    method path : js_string t readonly_prop
    method component : ('a, 'b) Vue_component.Internal.component_arg t optdef readonly_prop
    method children : ('a, 'b) route t js_array t optdef readonly_prop
    method name : js_string t optdef readonly_prop
    method components : ('a, 'b) Vue_component.Internal.component_arg table optdef readonly_prop
    method redirect : any optdef readonly_prop
    method alias : js_string t optdef readonly_prop
    method props : any optdef readonly_prop
    method beforeEnter : (push_args t -> push_args t -> (any -> unit) -> unit) callback optdef readonly_prop
  end

  (** route name javascript object *)
  class type name = object
    method name : js_string t readonly_prop
  end

  val to_redirect : redirect -> any

  (** route object *)
  type ('a, 'b) route_ml = {
    path : string;
    component : ('a, 'b) Vue_component.component_args option;
    children : (top, top) route t list option;
    name : string option;
    components : ('a, 'b) Vue_component.component_args table_cons option;
    redirect : redirect option;
    alias : string option;
    props : props option;
    hook : (push_obj -> push_obj -> 'b next) option;
  }

  val make_route_base : ('a, 'b) route_ml -> ('a ,'b) route t

  val make_route :
    ?component: ('a, 'b) Vue_component.component_args ->
    ?children: (top, top) route t list ->
    ?name: string ->
    ?components: ('a, 'b) Vue_component.component_args table_cons ->
    ?redirect: redirect ->
    ?alias: string ->
    ?props: props ->
    ?hook: (push_obj -> push_obj -> 'b next) -> string ->
    ('a, 'b) route t

  class type router_args = object
    method routes : (top, top) route t js_array t readonly_prop
    method mode : js_string t optdef readonly_prop
  end

  class type router = object
    method push : js_string t -> unit meth
    method push_obj : push_args t -> unit meth
    method replace : js_string t -> unit meth
    method replace_obj : push_args t -> unit meth
    method go : int -> unit meth
    method beforeEach : (push_args t -> push_args t -> (any -> unit) -> unit) callback -> unit meth
    method beforeResolve : (push_args t -> push_args t -> (any -> unit) -> unit) callback -> unit meth
    method afterEach : (push_args t -> push_args t -> (any -> unit) -> unit) callback -> unit meth
  end

  val make_args_base : ?mode:string -> (top, top) route t list -> router_args t

  val make_args : ?mode:string -> (top, top) route_ml list -> router_args t

  val vue_router_cs : (router_args t -> router t) constr

  val make_base : ?mode:string -> (top, top) route t list -> router t

end

(** route class type *)
class type ['data, 'all] route = ['data, 'all] Internal.route

(** router class type *)
class type router = Internal.router

(** Call router methods *)

(** push to new route *)
val push : router t -> push_arg -> unit

(** replace to new route *)
val replace : router t -> push_arg -> unit

(** go to n next route *)
val go : router t -> int -> unit

(** register hook before each navigation *)
val before_hook : router t -> (push_obj -> push_obj -> (any -> unit) -> unit) -> unit

(** register hook before each navigation confirmation *)
val resolve_hook : router t -> (push_obj -> push_obj -> (any -> unit) -> unit) -> unit

(** register hook after each navigation *)
val after_hook : router t -> (push_obj -> push_obj -> (any -> unit) -> unit) -> unit

(** route ML object *)
type ('a, 'b) route_ml = ('a, 'b) Internal.route_ml = {
  path : string;
  component : ('a, 'b) Vue_component.component_args option;
  children : (top, top) route t list option;
  name : string option;
  components : ('a, 'b) Vue_component.component_args table_cons option;
  redirect : redirect option;
  alias : string option;
  props : props option;
  hook : (push_obj -> push_obj -> 'b next) option;
}

(** empty route with path *)
val empty : string -> ('a, 'b) route_ml

(** make router from route list *)
val make: ?mode:string -> (top, top) route_ml list -> router t

module type Tables_S = sig
  val routes_t : (top, top) route t list ref
  val add_route : ('a, 'b) route t -> unit
  val merge_routes : ?routes:(top, top) route t list -> unit -> unit
end

(** Functor to fill routes objects *)
module Tables : Tables_S

(** functor to create a route with a component with typecheck of input data *)
module Route(S : sig
    (** component name *)
    val name : string

    (** template *)
    val template : string option

    (** props *)
    val props : Vue_component.props_options option * props option

    (** class type for vue data *)
    type data

    (** class type for output : data + computed + props (+methods?) *)
    type all
  end) : sig

  type data = S.data
  include Vue_component.Tables_S with type all = S.all
  include Tables_S

  (** make the route combining previously added methods/watch/computed *)
  val make : ?export:bool ->
    ?data:(all t -> S.data t) ->
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Vue_component.Internal.component_arg t) list ->
    ?children:(top, top) route t list ->
    ?path:string -> ?hook:(Vue_nav.push_obj -> Vue_nav.push_obj -> all Vue_nav.next) ->
    unit -> (data, all) route t

  (** Unsafe functions *)

  (** unsafe reference to component object *)
  val get : unit -> (data, all) route t
end
