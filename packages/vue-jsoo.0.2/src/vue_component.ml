open Js_of_ocaml
open Js
open Mjs

(** ml types *)

type js_type =
  | JString of string
  | JObject of any
  | JNumber of float
  | JBoolean of bool
  | JArray of any list

type prop_object = {
  pr_default : js_type;
  pr_required : bool option;
  pr_validator : (string -> bool) option;
}

type prop_options =
  | PrType of js_type
  | PrTypeArray of js_type list
  | PrObj of prop_object

type props_options = PrsArray of string list | PrsObj of (string * prop_options) list

module Internal = struct

  class type prop_object_js = object
    method type_ : (js_string t -> any t) callback readonly_prop
    method default : any optdef readonly_prop
    method required : bool t optdef readonly_prop
    method validator : (js_string t -> bool t) callback optdef readonly_prop
  end

  type vnode = any

  class type binding = object
    method name : js_string t readonly_prop
    method value : any optdef readonly_prop
    method oldValue : any optdef readonly_prop
    method expression : js_string t optdef readonly_prop
    method arg : js_string t optdef readonly_prop
    method modifiers : bool t table optdef readonly_prop
  end

  class type model = object
    method prop : js_string t optdef readonly_prop
    method event : js_string t optdef readonly_prop
  end

  class type ['data, 'all] component_common = object
    (* options / data *)
    method computed : ('all t, unit -> any optdef) meth_callback table optdef readonly_prop
    method methods : any table optdef readonly_prop
    method watch : ('all t, any -> any -> any) meth_callback table optdef readonly_prop
    (* options / lifecycle hooks *)
    method beforeCreate : ('all t, unit) meth_callback optdef readonly_prop
    method created : ('all t, unit) meth_callback optdef readonly_prop
    method beforeMount : ('all t, unit) meth_callback optdef readonly_prop
    method mounted : ('all t, unit) meth_callback optdef readonly_prop
    method beforeUpdate : ('all t, unit) meth_callback optdef readonly_prop
    method updated : ('all t, unit) meth_callback optdef readonly_prop
    method activated : ('all t, unit) meth_callback optdef readonly_prop
    method deactivated : ('all t, unit) meth_callback optdef readonly_prop
    method beforeDestroy : ('all t, unit) meth_callback optdef readonly_prop
    method destroyed : ('all t, unit) meth_callback optdef readonly_prop
    method errorCaptured : (any -> any -> js_string t -> bool t optdef) callback optdef readonly_prop
    (* options / assets *)
    method directives : (Dom.element t -> binding t -> vnode -> vnode -> unit) callback table table optdef readonly_prop
    method filters : (any -> any) callback table optdef readonly_prop
    method delimiters : js_string t js_array t optdef readonly_prop
    method functional : bool t optdef readonly_prop
    method model : model t optdef readonly_prop
    method inheritAttrs : bool t optdef readonly_prop
    method comments : bool t optdef readonly_prop
  end

  class type ['data, 'all] component_arg = object
    inherit ['data, 'all] component_common
    (* options / data *)
    method data : ('all t, unit -> 'data t) meth_callback optdef readonly_prop
    method props : any optdef readonly_prop
    (* options / dom *)
    method template : js_string t optdef readonly_prop
    method render : ('all t, any -> any) meth_callback optdef readonly_prop
    (* options / assets *)
    method components : ('data, 'all) component_arg t table optdef readonly_prop
    (* option / composition *)
    method mixins : any js_array t optdef readonly_prop
    method extends : any optdef readonly_prop
    (* options / misc *)
    method name : js_string t optdef readonly_prop
    (* route hooks *)
    method beforeRouteEnter : (Vue_nav.push_args t -> Vue_nav.push_args t -> (any -> unit) -> unit) callback optdef readonly_prop
    method beforeRouteUpdate : ('all t, Vue_nav.push_args t -> Vue_nav.push_args t -> (any -> unit) -> unit) meth_callback optdef readonly_prop
    method beforeRouteLeave : ('all t, Vue_nav.push_args t -> Vue_nav.push_args t -> (any -> unit) -> unit) meth_callback optdef readonly_prop
  end

  type 'all vue_instance = 'all t

  class type ['data, 'all] vue_object = object
    method component : js_string t -> ('data, 'all) component_arg t optdef -> 'all vue_instance meth
    method component_extend : js_string t -> ('data, 'all) component_arg t optdef -> 'all vue_instance constr meth
  end

  let js_type_cs : (js_type -> (js_string t -> Unsafe.any t) callback) = function
    | JString _ -> wrap_callback @@ Unsafe.global##_String
    | JObject _ -> wrap_callback @@ Unsafe.global##_Object
    | JNumber _ -> wrap_callback @@ Unsafe.global##_Number
    | JBoolean _ -> wrap_callback @@ Unsafe.global##_Boolean
    | JArray _ -> wrap_callback @@ Unsafe.global##_Array

  let js_prop_obj {pr_default; pr_required; pr_validator} : prop_object_js t =
    let default = match pr_default with
      | JString s -> def @@ to_any (string s)
      | JNumber f -> def @@ to_any (number_of_float f)
      | JBoolean b -> def @@ to_any (bool b)
      | JObject o -> def @@ to_any (wrap_callback (fun () -> o))
      | JArray a -> def @@ to_any (wrap_callback (fun () -> of_list a)) in
    object%js
      val type_ = js_type_cs pr_default
      val default = default
      val required = optdef bool pr_required
      val validator = optdef (fun v -> wrap_callback (fun s -> bool (v (to_string s)))) pr_validator
    end

  let make_prop = function
    | PrType s -> to_any @@ js_type_cs s
    | PrTypeArray a -> to_any @@ of_listf js_type_cs a
    | PrObj o -> to_any @@ js_prop_obj o

  let make_props = function
    | PrsObj l ->
      to_any @@ Table.make @@ List.map (fun (name, pr) -> name, make_prop pr) l
    | PrsArray l ->
      to_any (of_listf string l)

  type ('data, 'all) component_args = {
    template : string option;
    props : props_options option;
    data : ('all t -> 'data t) option ;
    render : ('all t -> any -> any) option;
    computed : ('all t -> any optdef) table_cons option;
    watch : ('all t -> any -> any -> any) table_cons option;
    methods : any table_cons option;
    lifecycle : (string * ('all t -> unit)) list;
    error_captured : (any -> any -> string -> bool option) option;
    directives : (string * (string * (Dom.element t -> binding t -> vnode -> vnode -> unit)) list) list option;
    filters : (string * (any -> any)) list option;
    components : (top, top) component_arg t table_cons;
    mixins : any list option;
    extends : any option;
    name : string option;
    delimiters : (string * string) option;
    functional : bool option;
    model : (string option * string option) option;
    inherit_attrs : bool option;
    comments : bool option;
    hook_enter : (Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) option;
    hook_update : ('all t -> Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) option;
    hook_leave : ('all t -> Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) option;
  }

  let make_arg c : ('data, 'all) component_arg t =
    object%js
      val template = optdef string c.template
      val props = optdef make_props c.props
      val data = optdef (fun f -> wrap_meth_callback (fun d () -> f d)) c.data
      val render = optdef wrap_meth_callback c.render
      val computed = optdef (to_tablef (fun c -> wrap_meth_callback (fun this () -> c this))) c.computed
      val watch = optdef (to_tablef wrap_meth_callback) c.watch
      val methods = optdef to_table c.methods
      val beforeCreate = optdef wrap_meth_callback @@ List.assoc_opt "beforeCreate" c.lifecycle
      val created = optdef wrap_meth_callback @@ List.assoc_opt "created" c.lifecycle
      val beforeMount = optdef wrap_meth_callback @@ List.assoc_opt "beforeMount" c.lifecycle
      val mounted = optdef wrap_meth_callback @@ List.assoc_opt "mounted" c.lifecycle
      val beforeUpdate = optdef wrap_meth_callback @@ List.assoc_opt "beforeUpdate" c.lifecycle
      val updated = optdef wrap_meth_callback @@ List.assoc_opt "updated" c.lifecycle
      val activated = optdef wrap_meth_callback @@ List.assoc_opt "activated" c.lifecycle
      val deactivated = optdef wrap_meth_callback @@ List.assoc_opt "deactivated" c.lifecycle
      val beforeDestroy = optdef wrap_meth_callback @@ List.assoc_opt "beforeDestroy" c.lifecycle
      val destroyed = optdef wrap_meth_callback @@ List.assoc_opt "destroyed" c.lifecycle
      val errorCaptured = optdef (fun f -> wrap_callback (fun x y s -> optdef bool @@ f x y (to_string s))) c.error_captured
      val directives = optdef (Table.makef (fun l -> Table.makef wrap_callback l)) c.directives
      val filters = optdef (Table.makef wrap_callback) c.filters
      val components = to_tablef_def coerce c.components
      val mixins = optdef of_list c.mixins
      val extends = Optdef.option c.extends
      val name = optdef string c.name
      val delimiters = optdef (fun (a, b) -> array [| string a; string b |]) c.delimiters
      val functional = optdef bool c.functional
      val model = optdef (fun (prop, event) -> object%js val prop = optdef string prop val event = optdef string event end) c.model
      val inheritAttrs = optdef bool c.inherit_attrs
      val comments = optdef bool c.comments
      val beforeRouteEnter = optdef Vue_nav.wrap_hook c.hook_enter
      val beforeRouteUpdate = optdef Vue_nav.wrap_meth_hook c.hook_update
      val beforeRouteLeave = optdef Vue_nav.wrap_meth_hook c.hook_leave
    end

  let make_arg_js ?template ?render ?props ?data ?computed ?methods ?watch
      ?(lifecycle = []) ?error_captured ?directives ?filters ?(components= L [])
      ?mixins ?extends ?name ?delimiters ?functional ?model ?inherit_attrs ?comments
      ?enter ?update ?leave () =
    make_arg {
      template; props; data; render; computed; watch; methods; lifecycle;
      error_captured; directives; filters; components; mixins; extends; name;
      delimiters; functional; model; inherit_attrs; comments;
      hook_enter = enter; hook_update = update; hook_leave = leave }

end

type ('data, 'all) component_args = ('data, 'all) Internal.component_args = {
  template : string option;
  props : props_options option;
  data : ('all t -> 'data t) option ;
  render : ('all t -> any -> any) option;
  computed : ('all t -> any optdef) table_cons option;
  watch : ('all t -> any -> any -> any) table_cons option;
  methods : any table_cons option;
  lifecycle : (string * ('all t -> unit)) list;
  error_captured : (any -> any -> string -> bool option) option;
  directives : (string * (string * (Dom.element t -> Internal.binding t -> Internal.vnode -> Internal.vnode -> unit)) list) list option;
  filters : (string * (any -> any)) list option;
  components : (top, top) Internal.component_arg t table_cons;
  mixins : any list option;
  extends : any option;
  name : string option;
  delimiters : (string * string) option;
  functional : bool option;
  model : (string option * string option) option;
  inherit_attrs : bool option;
  comments : bool option;
  hook_enter : (Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) option;
  hook_update : ('all t -> Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) option;
  hook_leave : ('all t -> Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) option;
}

let empty = {
  template = None; props = None; data = None; render = None;
  computed = None; watch = None; methods = None; lifecycle = []; error_captured = None;
  directives = None; filters = None; components = L []; mixins = None; extends = None;
  name = None; delimiters = None; functional = None; model = None; inherit_attrs = None;
  comments = None; hook_enter = None; hook_update = None; hook_leave = None
}

type 'a component = 'a Internal.vue_instance

let make ?template ?render ?props ?data ?computed ?methods ?watch
    ?lifecycle ?error_captured ?directives ?filters ?components
    ?mixins ?extends ?delimiters ?functional ?model ?inherit_attrs ?comments name =
  let arg = Internal.make_arg_js ?template ?render ?props ?data
      ?computed ?methods ?watch ?lifecycle ?error_captured ?directives ?filters
      ?components ?mixins ?extends ?delimiters ?functional ?model ?inherit_attrs
      ?comments () in
  match extends, mixins with
  | None, None ->
    let v : ('data, 'all) Internal.vue_object t = Unsafe.global##._Vue in
    v##component (string name) (def arg)
  | _ ->
    let v : ('data, 'all) Internal.vue_object t = Unsafe.global##._Vue in
    let cs = v##component_extend (string name) (def arg) in
    new%js cs

(** functors *)

module type Tables_S = sig
  type all

  val methods_t : any table
  val watch_t : (all t -> any -> any -> any) table
  val computed_t : (all t -> any optdef) table
  val components_t : (top, top) Internal.component_arg t table

  val add_method : string -> (all t -> 'a) -> unit
  val add_method0 : string -> (all t -> 'a) -> unit
  val add_method1 : string -> (all t -> 'a -> 'b) -> unit
  val add_method2 : string -> (all t -> 'a -> 'b -> 'c) -> unit
  val add_method3 : string -> (all t -> 'a -> 'b -> 'c -> 'd) -> unit
  val add_method4 : string -> (all t -> 'a -> 'b -> 'c -> 'd -> 'e) -> unit
  val add_watch : string -> (all t -> 'a -> 'a -> 'b) -> unit
  val add_computed : string -> (all t -> 'a optdef) -> unit
  val add_2way_computed : string ->
    get:(all t -> 'a optdef) -> set:(all t -> 'b -> unit) -> unit
  val add_component : string -> ('a, 'b) Internal.component_arg t -> unit

  val merge_lists_component :
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Internal.component_arg t) list ->
    unit -> unit
end

module Tables(S : sig type all end) = struct
  type all = S.all
  let methods_t = Table.create ()
  let watch_t = Table.create ()
  let computed_t = Table.create ()
  let components_t = Table.create ()

  let add_method name m =
    Table.add methods_t name (to_any (wrap_meth_callback m))
  let add_method0 name m = add_method name m
  let add_method1 name m = add_method name m
  let add_method2 name m = add_method name m
  let add_method3 name m = add_method name m
  let add_method4 name m = add_method name m
  let add_watch name w = Table.add watch_t name w
  let add_computed name c =
    Table.add computed_t name (fun this -> Optdef.map (c this) to_any)
  let add_2way_computed name ~get ~set = Table.add computed_t name @@
    coerce @@ Unsafe.obj [|
      "get", to_any @@ wrap_meth_callback (fun this () -> get this);
      "set", to_any @@ wrap_meth_callback (fun this value -> set this value) |]
  let add_component name c = Table.add components_t name @@ coerce c

  let merge_lists_component ?(computed=[]) ?(methods=[]) ?(watch=[]) ?(components=[]) () =
    Table.add_list methods_t methods;
    Table.add_list computed_t computed;
    Table.add_list watch_t watch;
    Table.add_list components_t components
end


module Make(S : sig
    val name : string
    val template : string option
    val props : props_options option
    type data
    type all
  end) = struct

  include Tables(struct type all = S.all end)

  let component : S.all component ref = ref (Unsafe.obj [||])

  let load ?(export=true) ?(data : (all t -> S.data t) option)
      ?computed ?methods ?watch ?components () =
    merge_lists_component ?computed ?methods ?watch ?components();
    component := make ?data ?template:S.template ?props:S.props
        ~methods:(T methods_t) ~watch:(T watch_t) ~computed:(T computed_t) S.name;
    if export then Js.export S.name !component;
    !component

  let get () = !component
end
