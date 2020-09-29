open Js_of_ocaml
open Js
open Mjs
open Vue_nav

type redirect = Rd of string | Rd_name of string

type props = PrBool of bool | PrObj of any | PrFn of (push_obj -> any)

module Internal = struct

  class type push_args = object
    method path : js_string t optdef readonly_prop
    method name : js_string t opt readonly_prop
    method params : js_string t table optdef readonly_prop
    method query : js_string t table optdef readonly_prop
  end

  class type ['a, 'b] route = object
    method path : js_string t readonly_prop
    method component : ('a, 'b) Vue_component.Internal.component_arg t optdef readonly_prop
    method children : ('a, 'b) route t js_array t optdef readonly_prop
    method name : js_string t optdef readonly_prop
    method components : ('a, 'b) Vue_component.Internal.component_arg t table optdef readonly_prop
    method redirect : any optdef readonly_prop
    method alias : js_string t optdef readonly_prop
    method props : any optdef readonly_prop
    method beforeEnter : (push_args t -> push_args t -> (any -> unit) -> unit) callback optdef readonly_prop
  end

  class type name = object
    method name : js_string t readonly_prop
  end

  let to_redirect = function
    | Rd s -> Unsafe.inject (string s)
    | Rd_name s -> Unsafe.inject (object%js val name = string s end : name t)


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

  let make_route_base {
      path; component; children; name; components; redirect;
      alias; props; hook} : ('a, 'b) route t =
    let components = optdef (to_tablef (fun v -> Vue_component.Internal.make_arg v)) components in
    let children = optdef (fun l -> of_listf Unsafe.coerce l) children in
    let hook = optdef wrap_hook hook in
    let props = match props with
      | None -> undefined
      | Some (PrBool b) -> def @@ Unsafe.inject @@ bool b
      | Some (PrObj o) -> def o
      | Some (PrFn f) -> let f x = f (to_push_obj x) in
        def @@ Unsafe.inject @@ wrap_callback f in
    object%js
      val path = string path
      val component = optdef Vue_component.Internal.make_arg component
      val children = children
      val name = optdef string name
      val components = components
      val redirect = optdef to_redirect redirect
      val alias = optdef string alias
      val props = props
      val beforeEnter = hook
    end

  let make_route ?component ?children ?name ?components ?redirect ?alias ?props ?hook path =
    make_route_base {path; component; children; name; components; redirect; alias; props; hook}

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

  let make_args_base ?mode routes : router_args t =
    object%js
      val routes = of_list routes
      val mode = optdef string mode
    end

  let make_args ?mode routes =
    let routes = List.map make_route_base routes in
    make_args_base ?mode routes

  let vue_router_cs : (router_args t -> router t) constr = Unsafe.variable "VueRouter"

  let make_base ?mode routes =
    new%js vue_router_cs (make_args_base ?mode routes)

end

class type ['data, 'all] route = ['data, 'all] Internal.route
class type router = Internal.router

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

let empty path = {
  path; component = None; children = None; name = None; components = None;
  redirect = None; alias = None; props = None; hook = None }

let make ?mode routes =
  new%js Internal.vue_router_cs (Internal.make_args ?mode routes)

let push router = function
  | PStr s -> router##push (string s)
  | PObj o -> router##push_obj (of_push_obj o)

let replace router = function
  | PStr s -> router##replace (string s)
  | PObj o -> router##replace_obj (of_push_obj o)

let go router i = router##go i

let before_hook router f =
  let f to_ from next = f (to_push_obj to_) (to_push_obj from) next in
  router##beforeEach (wrap_callback f)

let resolve_hook router f =
  let f to_ from next = f (to_push_obj to_) (to_push_obj from) next in
  router##beforeResolve (wrap_callback f)

let after_hook router f =
  let f to_ from next = f (to_push_obj to_) (to_push_obj from) next in
  router##afterEach (wrap_callback f)

module type Tables_S = sig
  val routes_t : (top, top) route t list ref
  val add_route : ('a, 'b) route t -> unit
  val merge_routes : ?routes:(top, top) route t list -> unit -> unit
end

module Tables = struct
  let routes_t = ref []

  let add_route c =
    routes_t := !routes_t @ [ coerce c ]

  let merge_routes ?(routes=[]) () =
    routes_t := !routes_t @ routes
end

module Route(S : sig
    val name : string
    val template : string option
    val props : Vue_component.props_options option * props option
    type data
    type all
  end) = struct

  type data = S.data
  include Vue_component.Tables(struct type all = S.all end)
  include Tables

  let route : (data, all) route t ref = ref (Unsafe.obj [||])

  let make ?(export=true) ?data
      ?computed ?methods ?watch ?components ?children ?path ?hook () =
    merge_lists_component ?computed ?methods ?watch ?components();
    merge_routes ?routes:children ();
    let component = Some {
        Vue_component.empty with
        Vue_component.template = S.template;
        props = fst S.props;
        data;
        computed = Some (T computed_t);
        methods = Some (T methods_t);
        watch = Some (T watch_t);
        components = T components_t;
        hook_enter = hook;
        name = Some S.name } in
    let path = match path with None -> "/" ^ S.name | Some path -> path in
    let r = Internal.make_route_base {
        (empty path) with
        component;
        props = snd S.props;
        children = Some !routes_t;
      } in
    route := r;
    if export then Js.export S.name !route;
    !route

  let get () = !route
end
