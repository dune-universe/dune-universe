open Mjs

(** JS constructors *)
type js_type =
  | JString of string
  | JObject of any
  | JNumber of float
  | JBoolean of bool
  | JArray of any list

(** generic prop object *)
type prop_object = {
  pr_default : js_type;
  pr_required : bool option;
  pr_validator : (string -> bool) option;
}

(** possible types for prop *)
type prop_options =
  | PrType of js_type
  | PrTypeArray of js_type list
  | PrObj of prop_object

(** possible types for props argument *)
type props_options = PrsArray of string list | PrsObj of (string * prop_options) list

(** Internal JS module *)
module Internal : sig

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
    method directives : (Js_of_ocaml.Dom.element t -> binding t -> vnode -> vnode -> unit) callback table table optdef readonly_prop
    method filters : (any -> any) callback table optdef readonly_prop
    (* options / misc *)
    method delimiters : js_string t js_array t optdef readonly_prop
    method functional : bool t optdef readonly_prop
    method model : model t optdef readonly_prop
    method inheritAttrs : bool t optdef readonly_prop
    method comments : bool t optdef readonly_prop
  end

  and ['data, 'all] component_arg = object
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

  val js_type_cs : js_type -> (js_string t -> any t) callback

  val js_prop_obj : prop_object -> prop_object_js t

  val make_prop : prop_options -> any

  val make_props : props_options -> any

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
    directives : (string * (string * (Js_of_ocaml.Dom.element t -> binding t -> vnode -> vnode -> unit)) list) list option;
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

  val make_arg : ('data, 'all) component_args -> ('data, 'all) component_arg t

  val make_arg_js :
    ?template:string ->
    ?render:('all t -> any -> any) ->
    ?props:props_options ->
    ?data:('all t -> 'data t) ->
    ?computed:('all t -> any optdef) table_cons ->
    ?methods:any table_cons ->
    ?watch:('all t -> any -> any -> any) table_cons ->
    ?lifecycle:(string * ('all t -> unit)) list ->
    ?error_captured:(any -> any -> string -> bool option) ->
    ?directives:(string * (string * (Js_of_ocaml.Dom.element t -> binding t -> vnode -> vnode -> unit)) list) list ->
    ?filters:(string * (any -> any)) list ->
    ?components:(top, top) component_arg t table_cons ->
    ?mixins:any list ->
    ?extends:any ->
    ?name:string ->
    ?delimiters:string * string ->
    ?functional:bool ->
    ?model:string option * string option ->
    ?inherit_attrs:bool ->
    ?comments:bool ->
    ?enter:(Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) ->
    ?update:('all t -> Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) ->
    ?leave:('all t -> Vue_nav.push_obj -> Vue_nav.push_obj -> 'all Vue_nav.next) ->
    unit -> ('data, 'all) component_arg t

end

(** component arguments object *)
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
  directives : (string * (string * (Js_of_ocaml.Dom.element t -> Internal.binding t -> Internal.vnode -> Internal.vnode -> unit)) list) list option;
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

(** empty component arguments object *)
val empty : ('data, 'all) component_args

(** generic component js type *)
type 'a component = 'a Internal.vue_instance

(** generic component maker Vue.component *)
val make :
  ?template:string ->
  ?render:('all t -> any -> any) ->
  ?props:props_options ->
  ?data:('all t -> 'data t) ->
  ?computed:('all t -> any optdef) table_cons ->
  ?methods:any table_cons ->
  ?watch:('all t -> any -> any -> any) table_cons ->
  ?lifecycle:(string * ('all t -> unit)) list ->
  ?error_captured:(any -> any -> string -> bool option) ->
  ?directives:(string * (string * (Js_of_ocaml.Dom.element t -> Internal.binding t -> Internal.vnode -> Internal.vnode -> unit)) list) list ->
  ?filters:(string * (any -> any)) list ->
  ?components:(top, top) Internal.component_arg t table_cons ->
  ?mixins:any list ->
  ?extends:any ->
  ?delimiters:string * string ->
  ?functional:bool ->
  ?model:string option * string option ->
  ?inherit_attrs:bool ->
  ?comments:bool ->
  string -> 'all component

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

(** Functor to fill methods/watch/computed/components objects *)
module Tables(S : sig type all end) : Tables_S with type all = S.all

(** functor to create a component with typecheck of input data *)
module Make(S : sig
    (** component name *)
    val name : string

    (** template *)
    val template : string option

    (** props *)
    val props : props_options option

    (** class type for vue data *)
    type data

    (** class type for output : data + computed + props (+methods?) *)
    type all
  end) : sig

  include Tables_S with type all = S.all

  (** load the component from previously added methods/watch/computed *)
  val load : ?export:bool ->
    ?data:(all t -> S.data t) ->
    ?computed:(string * (all t -> any optdef)) list ->
    ?methods:(string * any) list ->
    ?watch:(string * (all t -> any -> any -> any)) list ->
    ?components:(string * (top, top) Internal.component_arg t) list ->
    unit -> all component

  (** Unsafe functions *)

  (** unsafe reference to component object *)
  val get : unit -> all component
end
