open Js_of_ocaml
open Js
open Mjs

type ('state, 'getters) action_input = {
  state : 'state t;
  getters : 'getters t;
  commit : ?payload:any -> string -> unit;
  dispatch : ?payload:any -> string -> unit_promise t;
  root_state : any option;
  root_getters : any option;
}

type 'payload mutation = {
  name : string;
  payload : 'payload;
}

module Internal = struct

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

  let make (options : ('state, 'getters) cs_options t) =
    let vuex : vuex t = Unsafe.variable "Vuex" in
    let cs = vuex##._Store in
    new%js cs options

  let to_action_input (o : ('state, 'getters) action_input_js t) : ('state, 'getters) action_input =
    let commit ?payload s = o##commit (string s) (Optdef.option payload) in
    let dispatch ?payload s = o##dispatch (string s) (Optdef.option payload) in {
      state = o##.state;  getters = o##.getters;
      commit; dispatch; root_state = None; root_getters = None }

  let to_action_input_module (o : ('state, 'getters) action_input_module t) : ('state, 'getters) action_input =
    let commit ?payload s = o##commit (string s) (Optdef.option payload) in
    let dispatch ?payload s = o##dispatch (string s) (Optdef.option payload) in
    let root_state = Some o##.rootState in
    let root_getters = Some o##.rootGetters in {
      state = o##.state;  getters = o##.getters;
      commit; dispatch; root_state; root_getters }

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

  let of_dynamic_options (o : ('state, 'getters) options) : ('state, 'getters) dynamic_cs_options t = object%js
    val state = fun () -> o.o_state
    val mutations = to_table_def o.mutations
    val actions = to_tablef_def (fun f -> (fun o x -> f (to_action_input_module o) x)) o.actions
    val getters = to_table_def o.o_getters
    val modules = to_tablef_def (fun m -> Unsafe.coerce m) o.modules
    val namespaced = optdef bool o.namespaced
  end

  let of_options (o : ('state, 'getters) options) : ('state, 'getters) cs_options t = object%js
    val state = o.o_state
    val mutations = to_table_def o.mutations
    val actions = to_tablef_def (fun f -> (fun o x -> f (to_action_input o) x)) o.actions
    val getters = to_table_def o.o_getters
    val plugins = optdef of_list o.plugins
    val strict = optdef bool o.strict
    val devtools = optdef bool o.devtools
    val modules = to_table_def o.modules
  end

  let of_root_options root : root_options t optdef =
    optdef (fun b -> object%js val root = bool b end) root

  let to_mutation (o : mutation_js t) = {
    name = to_string o##.type_;
    payload = o##.payload
  }

end

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

class type ['state] store = ['state] Internal.instance_js
class type ['state, 'getters] module_obj = ['state, 'getters] Internal.dynamic_cs_options

let empty o_state = {
  o_state; o_getters = L []; mutations = L []; actions = L []; plugins = None;
  strict = None; devtools = None; modules = L []; namespaced = None }

let make options =
  let options = Internal.of_options options in
  Internal.make options

let make_module options =
  Internal.of_dynamic_options options

let state (v : 'state store t) = v##.state

let get (v : 'state store t) s =
  match Table.find v##.getters s with
  | None -> Firebug.console##log (string ("getter " ^ s ^ " doesn't exist")); assert false
  | Some (g : 'state t -> any) -> g (state v)

let commit ?payload ?root (v : 'state store t) s =
  v##commit (string s) (Optdef.option payload) (Internal.of_root_options root)

let dispatch ?payload ?root (v : 'state store t) s f =
  let p = v##dispatch (string s) (Optdef.option payload) (Internal.of_root_options root) in
  f p

let replace_state (v : 'state store t) s = v##replaceState s

let subscribe (v : 'state store t) f =
  v##subscribe (fun m state -> f (Internal.to_mutation m) state)

let subscribe_action (v : 'state store t) f =
  v##subscribeAction (fun m state -> f (Internal.to_mutation m) state)

let register_module (v : 'state store t) s m =
  v##registerModule (string s) (Internal.of_dynamic_options m)

let unregister_module (v : 'state store t) s = v##unregisterModule (string s)

let has_module (v : 'state store t) s = to_bool @@ v##hasModule (string s)

let hot_update (v : 'state store t) o = v##hotUpdate o

module Map = struct

  let state ?namespace props =
    let vuex : Internal.vuex t = Unsafe.variable "Vuex" in
    match namespace with
    | None -> vuex##mapState (of_listf string props)
    | Some n -> vuex##mapState_name (string n) (of_listf string props)

  let getters ?namespace props =
    let vuex : Internal.vuex t = Unsafe.variable "Vuex" in
    match namespace with
    | None -> vuex##mapGetters (of_listf string props)
    | Some n -> vuex##mapGetters_name (string n) (of_listf string props)

  let mutations ?namespace props =
    let vuex : Internal.vuex t = Unsafe.variable "Vuex" in
    match namespace with
    | None -> vuex##mapMutations (of_listf string props)
    | Some n -> vuex##mapMutations_name (string n) (of_listf string props)

  let actions ?namespace props =
    let vuex : Internal.vuex t = Unsafe.variable "Vuex" in
    match namespace with
    | None -> vuex##mapActions (of_listf string props)
    | Some n -> vuex##mapActions_name (string n) (of_listf string props)

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
  Tables_S with type state = S.state and type getters = S.getters = struct

  type state = S.state
  type getters = S.getters

  let getters_t = Table.create ()
  let mutations_t = Table.create ()
  let actions_t = Table.create ()
  let modules_t = Table.create ()

  let getters_to_computed = ref []
  let mutations_to_methods = ref []
  let actions_to_methods = ref []

  let add_getter ?(computed=false) name g =
    if computed then getters_to_computed := name :: !getters_to_computed;
    Table.add getters_t name (fun s -> to_any (g s))
  let add_mutation ?(methods=false) name m =
    if methods then mutations_to_methods := name :: !mutations_to_methods;
    Table.add mutations_t name (Obj.magic m)
  let add_action ?(methods=false) name a =
    if methods then actions_to_methods := name :: !mutations_to_methods;
    Table.add actions_t name (fun o x -> a (Internal.to_action_input o) (Obj.magic x))
  let add_module name (m : ('a, 'b) module_obj t) =
    Table.add modules_t name (coerce m)

  let merge_lists ?(getters=[]) ?(mutations=[]) ?(actions=[]) ?(modules=[]) () =
    Table.add_list getters_t getters;
    Table.add_list mutations_t mutations;
    Table.add_list modules_t modules;
    Table.add_listf actions_t
      (fun f -> fun o x -> f (Internal.to_action_input o) x) actions
end

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

module Make(S : sig type state type getters end) = struct
  include Tables(S)

  let store : state store t ref = ref (Unsafe.obj [||])

  let init ?plugins ?getters ?mutations ?actions ?modules ?strict ?devtools state =
    merge_lists ?getters ?mutations ?actions ?modules ();
    let options = {
      (empty state) with
      o_getters = T getters_t; mutations = T mutations_t; actions = T actions_t;
      plugins; modules = T modules_t; strict; devtools } in
    store := make options;
    !store

  let store () = !store

end

module type Module_S = sig
  include Tables_S
  val get :
    ?getters:(string * (state t -> any)) list ->
    ?mutations:(string * (state t -> any -> unit)) list ->
    ?actions:(string * ((state, getters) action_input -> any -> unit)) list ->
    ?modules:(string * (top, top) module_obj t) list ->
    ?namespaced:bool -> state t -> (state, getters) module_obj t
end

module Module(S : sig type state type getters end) = struct
  include Tables(S)

  let get ?getters ?mutations ?actions ?modules ?namespaced state =
    merge_lists ?getters ?mutations ?actions ?modules ();
    make_module {
      (empty state) with
      o_getters = T getters_t; mutations = T mutations_t; actions = T actions_t;
      modules = T modules_t; namespaced }

end
