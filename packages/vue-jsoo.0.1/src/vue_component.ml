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

  class type ['data, 'all] component_arg = object
    method template : js_string t optdef readonly_prop
    method props : any optdef readonly_prop
    method data : ('all t, unit -> 'data t) meth_callback optdef readonly_prop
    method render : ('all t, any -> any) meth_callback optdef readonly_prop
    method computed : ('all t, unit -> any optdef) meth_callback table optdef readonly_prop
    method watch : ('all t, any -> any -> any) meth_callback table optdef readonly_prop
    method methods : any table optdef readonly_prop
    method mixins : any js_array t optdef readonly_prop
    method extends : any optdef readonly_prop
    method mounted : ('all t, unit) meth_callback optdef readonly_prop
    method name : js_string t optdef readonly_prop
    method components : ('data, 'all) component_arg t table optdef readonly_prop
  end

  type 'all vue_output = 'all t

  class type ['data, 'all] vue_object = object
    method component : js_string t -> ('data, 'all) component_arg t optdef -> 'all vue_output meth
    method component_extend : js_string t -> ('data, 'all) component_arg t optdef -> 'all vue_output constr meth
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
    mixins : any list option;
    extends : any option;
    mounted : ('all t -> unit) option;
    name : string option;
    components : (top, top) component_arg t table_cons
  }

  let make_arg c : ('data, 'all) component_arg t =
    object%js
      val template = optdef string c.template
      val props = optdef make_props c.props
      val data = optdef (fun f -> wrap_meth_callback (fun d () -> f d)) c.data
      val render = optdef wrap_meth_callback c.render
      val computed = optdef (to_tablef (fun f -> wrap_meth_callback (fun d () -> f d))) c.computed
      val watch = optdef (to_tablef wrap_meth_callback) c.watch
      val methods = optdef to_table c.methods
      val mixins = optdef of_list c.mixins
      val extends = Optdef.option c.extends
      val mounted = optdef wrap_meth_callback c.mounted
      val name = optdef string c.name
      val components = to_tablef_def coerce c.components
    end

  let make_arg_js ?template ?render ?props ?data ?computed ?methods ?watch ?mixins
      ?extends ?mounted ?name ?(components= L []) () =
    make_arg {template; props; data; render; computed; watch; methods; mixins;
              extends; mounted; name; components}

end

type ('data, 'all) component_args = ('data, 'all) Internal.component_args = {
  template : string option;
  props : props_options option;
  data : ('all t -> 'data t) option ;
  render : ('all t -> any -> any) option;
  computed : ('all t -> any optdef) table_cons option;
  watch : ('all t -> any -> any -> any) table_cons option;
  methods : any table_cons option;
  mixins : any list option;
  extends : any option;
  mounted : ('all t -> unit) option;
  name : string option;
  components : (top, top) Internal.component_arg t table_cons
}

let empty = {
  template = None; props = None; data = None; render = None; computed = None;
  watch = None; methods = None; mixins = None; extends = None; mounted = None;
  name = None; components = L []
}

type 'a component = 'a Internal.vue_output

let make ?template ?render ?props ?data ?computed ?methods ?watch ?mixins
    ?extends ?mounted ?components name =
  let arg = Internal.make_arg_js ?template ?render ?props ?data ?computed ?methods ?watch
      ?mixins ?extends ?mounted ?components () in
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
  let add_computed name c = Table.add computed_t name c
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

  let load ?(export=true) ?(data: (all t -> S.data t) option) () =
    component := make ?data ?template:S.template ?props:S.props
        ~methods:(T methods_t) ~watch:(T watch_t) ~computed:(T computed_t) S.name;
    if export then Js.export S.name !component;
    !component

  let get () = !component
end
