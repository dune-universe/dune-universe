open Mjs

(** input for actions *)
type ('state, 'getters) action_input = {
  state : 'state t;
  getters : 'getters t;
  commit : ?payload:any -> string -> unit;
  dispatch : ?payload:any -> string -> unit_promise t;
  root_state : any option;
  root_getters : any option;
}

(** mutation type for subscribe *)
type 'payload mutation = {
  name : string;
  payload : 'payload;
}

(** internal JS module *)
module Internal : sig

  class type mutation_js = object
    method type_ : js_string t readonly_prop
    method payload : 'payload readonly_prop
  end

    class type ['state, 'getters] action_input_js = object
    method state : 'state t readonly_prop
    method getters : 'getters t readonly_prop
    method commit : js_string t -> 'payload optdef -> unit meth
    method dispatch : js_string t -> 'payload optdef -> unit_promise t meth
  end

  class type ['state, 'getters] action_input_module = object
    inherit ['state, 'getters] action_input_js
    method rootState : any readonly_prop
    method rootGetters : any readonly_prop
  end

  class type ['state] cs_options_base = object
    method mutations : ('state t -> any -> unit) table optdef readonly_prop
    method getters : ('state t -> any) table optdef readonly_prop
  end

  class type ['state, 'getters] dynamic_cs_options = object
    method state : (unit -> 'state t) readonly_prop
    method actions : (('state, 'getters) action_input_module t -> any -> unit) table optdef readonly_prop
    inherit ['state] cs_options_base
    method modules : ('state, 'getters) dynamic_cs_options t table optdef readonly_prop
    method namespaced : bool t optdef readonly_prop
  end

  class type ['state, 'getters] cs_options = object
    method state : 'state t readonly_prop
    method actions : (('state, 'getters) action_input_js t -> any -> unit) table optdef readonly_prop
    inherit ['state] cs_options_base
    method plugins : any js_array t optdef readonly_prop
    method strict : bool t optdef readonly_prop
    method devtools : bool t optdef readonly_prop
    method modules : (top, top) dynamic_cs_options t table optdef readonly_prop
  end

  class type root_options = object
    method root : bool t readonly_prop
  end

  class type ['state] instance_js = object
    method state : 'state t readonly_prop
    method getters : ('state t -> ('state t -> any) table -> any) table readonly_prop
    method commit : js_string t -> 'payload optdef -> root_options t optdef -> unit meth
    method dispatch : js_string t -> 'payload optdef -> root_options t optdef -> unit_promise t meth
    method replaceState : 'state t -> unit meth
    method watch : ('state t -> ('state t -> 'a) table -> 'b) -> ('b -> unit) -> unit meth
    method subscribe : (mutation_js t -> 'state t -> unit) -> unit meth
    method subscribeAction : (mutation_js t -> 'state t -> unit) -> unit meth
    method registerModule : js_string t -> (top, top) dynamic_cs_options t -> unit meth
    method unregisterModule : js_string t -> unit meth
    method hasModule : js_string t -> bool t meth
    method hotUpdate : 'a -> unit meth
  end

  type ('state, 'getters) vuex_cs = (('state, 'getters) cs_options t -> 'state instance_js t) constr

  class type vuex = object
    method _Store : (('state, 'getters) cs_options t -> 'state instance_js t) constr readonly_prop
    method mapState : js_string t js_array t -> 'a table meth
    method mapState_name : js_string t -> js_string t js_array t -> 'a table meth
    method mapState_obj : ('b, 'state -> 'c) meth_callback table -> 'a table meth
    method mapGetters : js_string t js_array t -> 'a table meth
    method mapGetters_name : js_string t -> js_string t js_array t -> 'a table meth
    method mapMutations : js_string t js_array t -> 'a table meth
    method mapMutations_name : js_string t -> js_string t js_array t -> 'a table meth
    method mapMutations_obj : ('b, 'state -> 'c) meth_callback table -> 'a table meth
    method mapActions : js_string t js_array t -> 'a table meth
    method mapActions_name : js_string t -> js_string t js_array t -> 'a table meth
    method mapActions_obj : ('b, 'state -> 'c) meth_callback table -> 'a table meth
  end

  (** store constructor options *)
  type ('state, 'getters) options = {
    o_state : 'state t;
    o_getters : ('state t -> any) table_cons;
    mutations : ('state t -> any -> unit) table_cons;
    actions : (('state, 'getters) action_input -> any -> unit) table_cons;
    plugins : any list option;
    strict : bool option;
    devtools : bool option;
    modules : (top, top) dynamic_cs_options t table_cons;
    namespaced : bool option;
  }

  val make : ('state, 'getters) cs_options t -> 'state instance_js t

  val to_action_input : ('state, 'getters) action_input_js t -> ('state, 'getters) action_input
  val to_action_input_module : ('state, 'getters) action_input_module t -> ('state, 'getters) action_input

  val of_dynamic_options : ('state, 'getters) options -> ('state, 'getters) dynamic_cs_options t

  val of_options : ('state, 'getters) options -> ('state, 'getters) cs_options t

  val of_root_options : bool option -> root_options t optdef

  val to_mutation : mutation_js t -> 'a mutation

end

(** store constructor options *)
type ('state, 'getters) options = ('state, 'getters) Internal.options = {
  o_state : 'state t;
  o_getters : ('state t -> any) table_cons;
  mutations : ('state t -> any -> unit) table_cons;
  actions : (('state, 'getters) action_input -> any -> unit) table_cons;
  plugins : any list option;
  strict : bool option;
  devtools : bool option;
  modules : (top, top) Internal.dynamic_cs_options t table_cons;
  namespaced : bool option;
}

(** JS store object *)
class type ['state] store = ['state] Internal.instance_js

(** JS module object *)
class type ['state, 'getters] module_obj = ['state, 'getters] Internal.dynamic_cs_options

(** empty constructor options *)
val empty : 'state t -> ('state, 'getters) options

(** main make store function *)
val make : ('state, 'getters) options -> 'state store t

(** make module store function *)
val make_module : ('state, 'getters) options -> ('state, 'getters) module_obj t

(** get state of store *)
val state : 'state store t -> 'state t

(** get value of one of the getters *)
val get : 'state store t -> string -> any

(** commit to the state of the store *)
val commit : ?payload:any -> ?root:bool -> 'state store t -> string -> unit

(** dispatch to the state of the store *)
val dispatch : ?payload:any -> ?root:bool -> 'state store t -> string -> (unit_promise t -> 'async) -> 'async

(** replace the state of the store *)
val replace_state : 'state store t -> 'state t -> unit

(** hook for mutation subscription *)
val subscribe : 'state store t -> ('a mutation -> 'state t -> unit) -> unit

(** hook for action subscription *)
val subscribe_action : 'state store t -> ('a mutation -> 'state t -> unit) -> unit

(** register a new module to the store *)
val register_module : 'state store t -> string -> (top, top) options -> unit

(** unregister a module of the store *)
val unregister_module : 'state store t -> string -> unit

(** module existence *)
val has_module : 'state store t -> string -> bool

val hot_update : 'state store t -> 'a -> unit

(** Binding helpers *)
module Map : sig
  val state : ?namespace:string -> string list -> 'a table
  val getters : ?namespace:string -> string list -> 'a table
  val mutations : ?namespace:string -> string list -> 'a table
  val actions : ?namespace:string -> string list -> 'a table
end

module type Tables_S = sig
  type state
  type getters
  val getters_t : (state t -> any) Table.t
  val mutations_t : (state t -> any -> unit) Table.t
  val actions_t : ((state, getters) Internal.action_input_js t -> any -> unit) Table.t
  val modules_t : (top, top) module_obj t Table.t

  val getters_to_computed : string list ref
  val mutations_to_methods : string list ref
  val actions_to_methods : string list ref

  val add_getter : ?computed:bool -> string -> (state t -> 'a) -> unit
  val add_mutation : ?methods:bool -> string -> (state t -> 'a -> unit) -> unit
  val add_action : ?methods:bool -> string -> ((state, getters) action_input -> 'a -> unit) -> unit
  val add_module : string -> ('a, 'b) module_obj t -> unit

  val merge_lists :
    ?getters: (string * (state t -> any)) list ->
    ?mutations: (string * (state t -> any -> unit)) list ->
    ?actions: (string * ((state, getters) action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) module_obj t) list -> unit -> unit
end

module Tables(S : sig type state type getters end) :
  Tables_S with type state = S.state and type getters = S.getters

module type Make_S = sig
  include Tables_S

  val init :
    ?plugins:any list ->
    ?getters: (string * (state t -> any)) list ->
    ?mutations: (string * (state t -> any -> unit)) list ->
    ?actions: (string * ((state, getters) action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) module_obj t) list -> ?strict:bool ->
    ?devtools:bool -> state t -> state store t

  val store : unit -> state store t
end

module Make(S : sig type state type getters end) :
  Make_S with type state = S.state and type getters = S.getters

module type Module_S = sig
  include Tables_S
  val get :
    ?getters:(string * (state t -> any)) list ->
    ?mutations:(string * (state t -> any -> unit)) list ->
    ?actions:(string * ((state, getters) action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) module_obj t) list ->
    ?namespaced:bool -> state t -> (state, getters) module_obj t
end

module Module(S : sig type state type getters end) :
  Module_S with type state = S.state and type getters = S.getters
