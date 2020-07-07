open Mjs

(** JS constructor *)
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
    mixins : any list option;
    extends : any option;
    mounted : ('all t -> unit) option;
    name : string option;
    components : (top, top) component_arg t table_cons
  }

  val make_arg : ('data, 'all) component_args -> ('data, 'all) component_arg t

  val make_arg_js :
    ?template: string ->
    ?render: ('all t -> any -> any) ->
    ?props: props_options ->
    ?data: ('all t -> 'data t) ->
    ?computed: ('all t -> any optdef) table_cons ->
    ?methods: any table_cons ->
    ?watch: ('all t -> any -> any -> any) table_cons ->
    ?mixins: any list ->
    ?extends: any ->
    ?mounted: ('all t -> unit) ->
    ?name: string ->
    ?components: (top, top) component_arg t table_cons ->
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
  mixins : any list option;
  extends : any option;
  mounted : ('all t -> unit) option;
  name : string option;
  components : (top, top) Internal.component_arg t table_cons
}

(** empty component arguments object *)
val empty : ('data, 'all) component_args

(** generic component js type *)
type 'a component = 'a Internal.vue_output

(** generic component maker Vue.component *)
val make :
  ?template: string ->
  ?render: ('all t -> any -> any) ->
  ?props: props_options ->
  ?data: ('all t -> 'data t) ->
  ?computed: ('all t -> any optdef) table_cons ->
  ?methods: any table_cons ->
  ?watch: ('all t -> any -> any -> any) table_cons ->
  ?mixins: any list ->
  ?extends: any ->
  ?mounted: ('all t -> unit) ->
  ?components: (top, top) Internal.component_arg t table_cons ->
  string -> 'all component

(* (\** coerce component args to any *\)
 * val component_to_any : 'a component_args -> top component_args *)

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
  val load : ?export:bool -> ?data:(all t -> S.data t) -> unit -> all component

  (** Unsafe functions *)

  (** unsafe reference to component object *)
  val get : unit -> all component
end
