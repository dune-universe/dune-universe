open Mjs

(** input for actions *)
type 'state action_input = {
  state : 'state t;
  getters : ('state t -> ('state t -> any) table -> any) table;
  commit : ?payload:any -> string -> unit;
  dispatch : ?payload:any -> string -> unit_promise t;
  root_state : any option;
  root_getters : (any -> (any -> any) table -> any) table option;
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

    class type ['state] action_input_js = object
    method state : 'state t readonly_prop
    method getters : ('state t -> ('state t -> any) table -> any) table readonly_prop
    method commit : js_string t -> 'payload optdef -> unit meth
    method dispatch : js_string t -> 'payload optdef -> unit_promise t meth
  end

  class type ['state] action_input_module = object
    inherit ['state] action_input_js
    method rootState : any readonly_prop
    method rootGetters : (any -> (any -> any) table -> any) table readonly_prop
  end

  class type ['state] cs_options_base = object
    method mutations : ('state t -> any -> unit) table optdef readonly_prop
    method getters : ('state t -> any) table optdef readonly_prop
  end

  class type ['state] dynamic_cs_options = object
    method state : (unit -> 'state t) readonly_prop
    method actions : ('state action_input_module t -> any -> unit) table optdef readonly_prop
    inherit ['state] cs_options_base
    method modules : 'a dynamic_cs_options t table optdef readonly_prop
    method namespaced : bool t optdef readonly_prop
  end

  class type ['state] cs_options = object
    method state : 'state t readonly_prop
    method actions : ('state action_input_js t -> any -> unit) table optdef readonly_prop
    inherit ['state] cs_options_base
    method plugins : any js_array t optdef readonly_prop
    method strict : bool t optdef readonly_prop
    method devtools : bool t optdef readonly_prop
    method modules : top dynamic_cs_options t table optdef readonly_prop
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
    method registerModule : js_string t -> top dynamic_cs_options t -> unit meth
    method unregisterModule : js_string t -> unit meth
    method hasModule : js_string t -> bool t meth
    method hotUpdate : 'a -> unit meth
  end

  type 'state vuex_cs = ('state cs_options t -> 'state instance_js t) constr

  class type vuex = object
    method _Store : ('state cs_options t -> 'state instance_js t) constr readonly_prop
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
  type 'state options = {
    o_state : 'state t;
    o_getters : ('state t -> any) table_cons;
    mutations : ('state t -> any -> unit) table_cons;
    actions : ('state action_input -> any -> unit) table_cons;
    plugins : any list option;
    strict : bool option;
    devtools : bool option;
    modules : top dynamic_cs_options t table_cons;
    namespaced : bool option;
  }

  val make : 'state cs_options t -> 'state instance_js t

  val to_action_input : 'state action_input_js t -> 'state action_input
  val to_action_input_module : 'state action_input_module t -> 'state action_input

  val of_dynamic_options : 'state options -> 'state dynamic_cs_options t

  val of_options : 'state options -> 'state cs_options t

  val of_root_options : bool option -> root_options t optdef

  val to_mutation : mutation_js t -> 'a mutation

end

(** store constructor options *)
type 'state options = 'state Internal.options = {
  o_state : 'state t;
  o_getters : ('state t -> any) table_cons;
  mutations : ('state t -> any -> unit) table_cons;
  actions : ('state action_input -> any -> unit) table_cons;
  plugins : any list option;
  strict : bool option;
  devtools : bool option;
  modules : top Internal.dynamic_cs_options t table_cons;
  namespaced : bool option;
}

(** JS store object *)
type 'state store = 'state Internal.instance_js t

(** JS module object *)
type 'state module_obj = 'state Internal.dynamic_cs_options t

(** empty constructor options *)
val empty : 'state t -> 'state options

(** main make store function *)
val make : 'state options -> 'state store

(** make module store function *)
val make_module : 'state options -> 'state module_obj

(** get state of store *)
val state : 'state store -> 'state t

(** get value of one of the getters *)
val get : 'state store -> string -> any

(** commit to the state of the store *)
val commit : ?payload:any -> ?root:bool -> 'state store -> string -> unit

(** dispatch to the state of the store *)
val dispatch : ?payload:any -> ?root:bool -> 'state store -> string -> (unit_promise t -> 'async) -> 'async

(** replace the state of the store *)
val replace_state : 'state store -> 'state t -> unit

(** hook for mutation subscription *)
val subscribe : 'state store -> ('a mutation -> 'state t -> unit) -> unit

(** hook for action subscription *)
val subscribe_action : 'state store -> ('a mutation -> 'state t -> unit) -> unit

(** register a new module to the store *)
val register_module : 'state store -> string -> top options -> unit

(** unregister a module of the store *)
val unregister_module : 'state store -> string -> unit

(** module existence *)
val has_module : 'state store -> string -> bool

val hot_update : 'state store -> 'a -> unit

(** Binding helpers *)
module Map : sig
  val state : ?namespace:string -> string list -> 'a table
  val getters : ?namespace:string -> string list -> 'a table
  val mutations : ?namespace:string -> string list -> 'a table
  val actions : ?namespace:string -> string list -> 'a table
end

module type Tables_S = sig
  type state
  val getters_t : (state t -> any) Table.t
  val mutations_t : (state t -> any -> unit) Table.t
  val actions_t : (state Internal.action_input_js t -> any -> unit) Table.t
  val modules_t : top module_obj Table.t

  val add_getter : string -> (state t -> any) -> unit
  val add_mutation : string -> (state t -> any -> unit) -> unit
  val add_action : string -> (state action_input -> any -> unit) -> unit
  val add_module : string -> 'a module_obj -> unit

  val merge_lists :
    ?getters: (string * (state t -> any)) list ->
    ?mutations: (string * (state t -> any -> unit)) list ->
    ?actions: (string * (state action_input -> any -> unit)) list ->
    ?modules:(string * top module_obj) list -> unit -> unit
end

module Tables(S : sig type state end) : Tables_S with type state = S.state

module type Make_S = sig
  include Tables_S

  val init :
    ?plugins:any list ->
    ?getters: (string * (state t -> any)) list ->
    ?mutations: (string * (state t -> any -> unit)) list ->
    ?actions: (string * (state action_input -> any -> unit)) list ->
    ?modules:(string * top module_obj) list -> ?strict:bool ->
    ?devtools:bool -> state t -> state store

  val store : unit -> state store
end

module Make(S : sig type state end) : Make_S with type state = S.state

module type Module_S = sig
  include Tables_S
  val get :
    ?getters:(string * (state t -> any)) list ->
    ?mutations:(string * (state t -> any -> unit)) list ->
    ?actions:(string * (state action_input -> any -> unit)) list ->
    ?modules:(string * top module_obj) list ->
    ?namespaced:bool -> state t -> state module_obj
end

module Module(S : sig type state end) : Module_S with type state = S.state
