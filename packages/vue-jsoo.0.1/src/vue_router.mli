open Mjs

(** Create router object *)

(** redirect to path or to named route *)
type redirect = Rd of string | Rd_name of string

(** route argument object for the router methods *)
type push_obj = {
  pu_path : string option;
  pu_name : string option;
  params : (string * string) list option;
  query : (string * string) list option;
}

(** empty route object argument *)
val empty_arg : push_obj

(** props argument can be a bool, and object or a function *)
type props = PrBool of bool | PrObj of any | PrFn of (push_obj -> any)

(** Internal module *)
module Internal : sig

  class type push_args = object
    method path : js_string t optdef readonly_prop
    method name : js_string t optdef readonly_prop
    method params : js_string t table optdef readonly_prop
    method query : js_string t table optdef readonly_prop
  end

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
    method beforeEnter : (push_args t -> push_args t -> (unit -> unit) -> unit) callback optdef readonly_prop
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
    hook : (push_obj -> push_obj -> (unit -> unit) -> unit) option;
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
    ?hook: (push_obj -> push_obj -> (unit -> unit) -> unit) -> string ->
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
    method beforeEach : (push_args t -> push_args t -> (unit -> unit) -> unit) callback -> unit meth
    method beforeResolve : (push_args t -> push_args t -> (unit -> unit) -> unit) callback -> unit meth
    method afterEach : (push_args t -> push_args t -> (unit -> unit) -> unit) callback -> unit meth
  end

  val make_args_base : ?mode:string -> (top, top) route t list -> router_args t

  val make_args : ?mode:string -> (top, top) route_ml list -> router_args t

  val vue_router_cs : (router_args t -> router t) constr

  val make_base : ?mode:string -> (top, top) route t list -> router t

  val of_push_obj : push_obj -> push_args t

  val to_push_obj : push_args t -> push_obj

end

(** router class type *)
type router = Internal.router t

(** Call router methods *)

(** route argument can be a string or an object *)
type push_arg = PStr of string | PObj of push_obj

(** push to new route *)
val push : router -> push_arg -> unit

(** replace to new route *)
val replace : router -> push_arg -> unit

(** go to n next route *)
val go : router -> int -> unit

(** register hook before each navigation *)
val before_hook : router -> (push_obj -> push_obj -> (unit -> unit) -> unit) -> unit

(** register hook before each navigation confirmation *)
val resolve_hook : router -> (push_obj -> push_obj -> (unit -> unit) -> unit) -> unit

(** register hook after each navigation *)
val after_hook : router -> (push_obj -> push_obj -> (unit -> unit) -> unit) -> unit

(** route object *)
type ('a, 'b) route_ml = ('a, 'b) Internal.route_ml = {
  path : string;
  component : ('a, 'b) Vue_component.component_args option;
  children : (top, top) Internal.route t list option;
  name : string option;
  components : ('a, 'b) Vue_component.component_args table_cons option;
  redirect : redirect option;
  alias : string option;
  props : props option;
  hook : (push_obj -> push_obj -> (unit -> unit) -> unit) option;
}

(** empty route with path *)
val empty : string -> ('a, 'b) route_ml

(** make router from route list *)
val make: ?mode:string -> (top, top) route_ml list -> router

module type Tables_S = sig
  val routes_t : (top, top) Internal.route t list ref
  val add_route : (top, top) Internal.route t -> unit
  val merge_routes : ?routes:(top, top) Internal.route t list -> unit -> unit
end

module Tables : Tables_S
