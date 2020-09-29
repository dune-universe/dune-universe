open Js_of_ocaml
open Js
open Mjs

type 'all vue = 'all t

class type ['data, 'all, 'state] vue_arg = object
  inherit ['data, 'all] Vue_component.Internal.component_common
  method el : js_string t readonly_prop
  method data : 'data t readonly_prop
  method components : (top, top) Vue_component.Internal.component_arg t table optdef readonly_prop
  method router : Vue_router.router t optdef readonly_prop
  method store : 'state Vuex.store t optdef readonly_prop
end

type ('data, 'all, 'state) vue_cs = (('data, 'all, 'state) vue_arg t -> 'all vue) constr

let make ?computed ?watch ?methods ?data ?(lifecycle = []) ?error_captured
    ?directives ?filters ?components ?delimiters ?functional
    ?model ?inherit_attrs ?comments ?router ?store id =
  let data : 'data t = match data with None -> Unsafe.obj [||] | Some d -> d in
  let v : ('data, 'all, 'state) vue_arg t = object%js
    val el = string ("#" ^ id)
    val data = data
    val computed = optdef (to_tablef (fun c -> wrap_meth_callback (fun this () -> c this))) computed
    val watch = optdef (to_tablef wrap_meth_callback) watch
    val methods = optdef to_table methods
    val beforeCreate = optdef wrap_meth_callback @@ List.assoc_opt "beforeCreate" lifecycle
    val created = optdef wrap_meth_callback @@ List.assoc_opt "created" lifecycle
    val beforeMount = optdef wrap_meth_callback @@ List.assoc_opt "beforeMount" lifecycle
    val mounted = optdef wrap_meth_callback @@ List.assoc_opt "mounted" lifecycle
    val beforeUpdate = optdef wrap_meth_callback @@ List.assoc_opt "beforeUpdate" lifecycle
    val updated = optdef wrap_meth_callback @@ List.assoc_opt "updated" lifecycle
    val activated = optdef wrap_meth_callback @@ List.assoc_opt "activated" lifecycle
    val deactivated = optdef wrap_meth_callback @@ List.assoc_opt "deactivated" lifecycle
    val beforeDestroy = optdef wrap_meth_callback @@ List.assoc_opt "beforeDestroy" lifecycle
    val destroyed = optdef wrap_meth_callback @@ List.assoc_opt "destroyed" lifecycle
    val errorCaptured = optdef (fun f -> wrap_callback (fun x y s -> optdef bool @@ f x y (to_string s))) error_captured
    val directives = optdef (Table.makef (fun l -> Table.makef wrap_callback l)) directives
    val filters = optdef (Table.makef wrap_callback) filters
    val components = optdef to_table components
    val delimiters = optdef (fun (a, b) -> array [| string a; string b |]) delimiters
    val functional = optdef bool functional
    val model = optdef (fun (prop, event) -> object%js val prop = optdef string prop val event = optdef string event end) model
    val inheritAttrs = optdef bool inherit_attrs
    val comments = optdef bool comments
    val router = Optdef.option router
    val store = Optdef.option store
  end in
  let cs : ('data, 'all, 'state) vue_cs = Unsafe.global##._Vue in
  new%js cs v

let set_global s v = Unsafe.set (Unsafe.variable "Vue.prototype") ("$" ^ s) v
let get_prop vm s = Unsafe.get vm @@ "$" ^ s
let get_ref vm id =
  let refs = get_prop vm "refs" in
  Unsafe.get refs id

let get_router vm : Vue_router.router t = get_prop vm "router"
let get_store vm : 'state Vuex.store t = get_prop vm "store"
let get_route vm : ('b, 'c) Vue_router.route t = get_prop vm "route"

let store_2way ?(prefix="update") name =
  (fun this -> Unsafe.get (Vuex.state (get_store this)) name),
  (fun this value ->
     Vuex.commit (get_store this) (prefix ^ "_" ^ name)
       ~payload:(to_any value))

let emit vm a = Unsafe.meth_call vm "$emit" a
let emit0 vm event = emit vm [| to_any @@ string event |]
let emit1 vm event o = emit vm [| to_any @@ string event; to_any o |]

let unhide ?(suffix="loading") id =
  (match Dom_html.getElementById_opt id with
   | None -> ()
   | Some app -> app##.style##.display := string "block");
  match Dom_html.getElementById_opt (id ^ "-" ^ suffix) with
  | None -> ()
  | Some loading -> loading##.style##.display := string "none"

module Make(S : sig
    type data
    type all
    val id : string
  end) = struct

  type data = S.data
  include Vue_component.Tables(struct type all = S.all end)
  include Vue_router.Tables

  let app : all t ref = ref (Unsafe.obj [||])

  let init ?computed ?methods ?watch ?components
      ?(export=true) ?data ?(show=false) ?suffix ?router ?store () =
    merge_lists_component ?computed ?methods ?watch ?components ();
    app :=
      make
        ?data
        ~methods:(T methods_t)
        ~watch:(T watch_t)
        ~computed:(T computed_t)
        ~components:(T components_t)
        ?router
        ?store
        S.id;
    if show then unhide ?suffix S.id;
    if export then Js.export S.id !app;
    !app

  let app () = !app
end

module Router(S : sig
    type data
    type all
    val id : string
  end) = struct

  type data = S.data
  include Vue_component.Tables(struct type all = S.all end)
  include Vue_router.Tables

  let app : all vue ref = ref (Unsafe.obj [||])
  let router : Vue_router.router t ref = ref (Unsafe.obj [||])

  let init ?computed ?methods ?watch ?components ?routes
      ?(show=false) ?(export=true) ?suffix ?data ?mode ?store () =
    merge_lists_component ?computed ?methods ?watch ?components ();
    merge_routes ?routes ();
    let rt = new%js Vue_router.Internal.vue_router_cs
      (Vue_router.Internal.make_args_base ?mode !routes_t) in
    router := rt;
    app :=
      make
        ?data
        ~methods:(T methods_t)
        ~watch:(T watch_t)
        ~computed:(T computed_t)
        ~components:(T components_t)
        ~router:rt
        ?store
        S.id;
    if show then unhide ?suffix S.id;
    if export then Js.export S.id !app;
    !app

  let router () = !router
  let app () = !app

end

module SPA(S : sig
    type data
    type all
    type state
    type getters
    val name : string
    val template : string
    val props : string list
  end) = struct

  type data = S.data
  include Vuex.Module(struct type state = S.state type getters = S.getters end)
  include Vue_component.Tables(struct type all = S.all end)
  include Vue_router.Tables

  let store : (state, getters) Vuex.module_obj t ref = ref (Unsafe.obj [||])
  let route : (S.data, all) Vue_router.route t ref = ref (Unsafe.obj [||])

  let add_2way ?(prefix="update") name =
    let get, set = store_2way ~prefix name in
    add_2way_computed name ~get ~set;
    add_mutation (prefix ^ "_" ^ name) (fun st x -> Unsafe.set st name x)

  let init ?getters ?mutations ?actions ?modules ?computed ?methods ?watch ?components
      ?children ?hook ?path ?data ?state ?(state_to_computed=[]) ?(two_way=[]) () =
    let data = match data with None -> Unsafe.obj [||] | Some d -> d in
    let state = match state with None -> Unsafe.obj [||] | Some s -> s in
    merge_lists_component ?computed ?methods ?watch ?components ();
    merge_routes ?routes:children ();
    List.iter add_2way two_way;
    let st = get ?getters ?mutations ?actions ?modules ~namespaced:true state in
    let computed = Table.merge [
        computed_t;
        Vuex.Map.state ~namespace:S.name state_to_computed;
        Vuex.Map.getters ~namespace:S.name !getters_to_computed ] in
    let methods = Table.merge [
        methods_t;
        Vuex.Map.mutations ~namespace:S.name !mutations_to_methods;
        Vuex.Map.actions ~namespace:S.name !actions_to_methods;
      ] in
    let component = Some {
        Vue_component.empty with
        Vue_component.template = Some S.template;
        props = Some (Vue_component.PrsArray S.props);
        data = Some (fun _ -> data);
        computed = Some (T computed);
        methods = Some (T methods);
        watch = Some (T watch_t);
        components = T components_t;
        hook_enter = hook;
        name = Some S.name } in
    let path = match path with None -> "/" ^ S.name | Some path -> path in
    let r = Vue_router.Internal.make_route_base {
        (Vue_router.empty path) with
        Vue_router.component;
        props = (match S.props with [] -> None | _ -> Some (Vue_router.PrBool true));
        children = Some !routes_t;
      } in
    store := st;
    route := r;
    S.name, st, r

  let make ?getters ?mutations ?actions ?modules ?computed ?methods ?watch ?components
      ?children ?hook ?path ?data ?state ?state_to_computed ?two_way () =
    let name, st, r = init ?getters ?mutations ?actions ?modules ?computed ?methods ?watch ?components
        ?children ?hook ?path ?data ?state ?state_to_computed ?two_way () in
    name, coerce st, coerce r

  let store () = !store
  let route () = !route

end

module Root(S : sig
    type data
    type all
    type state
    type getters
    val id : string
  end) = struct

  type data = S.data
  include Vuex.Make(struct type state = S.state type getters = S.getters end)
  include Vue_component.Tables(struct type all = S.all end)
  include Vue_router.Tables

  let app : all vue ref = ref (Unsafe.obj [||])
  let router : Vue_router.router t ref = ref (Unsafe.obj [||])

  let add_spa (name, store, route) =
    add_route route;
    add_module name store

  let add_2way ?(prefix="update") name =
    let get, set = store_2way ~prefix name in
    add_2way_computed name ~get ~set;
    add_mutation (prefix ^ "_" ^ name) (fun st x -> Unsafe.set st name x)

  let init ?getters ?mutations ?actions ?modules ?computed ?methods ?watch
      ?components ?routes ?strict ?devtools ?plugins ?mode
      ?(show=false) ?(export=true) ?suffix ?data ?state ?(state_to_computed=[]) ?(two_way=[]) () =
    let data = match data with None -> Unsafe.obj [||] | Some d -> d in
    let state = match state with None -> Unsafe.obj [||] | Some s -> s in
    merge_lists_component ?computed ?methods ?watch ?components ();
    merge_routes ?routes ();
    List.iter add_2way two_way;
    let store = init ?plugins ?getters ?mutations ?actions ?modules ?strict ?devtools state in
    let computed = Table.merge [
        computed_t;
        Vuex.Map.state state_to_computed;
        Vuex.Map.getters !getters_to_computed;
      ] in
    let methods = Table.merge [
        methods_t;
        Vuex.Map.mutations !mutations_to_methods;
        Vuex.Map.actions !actions_to_methods;
      ] in
    let rt = new%js Vue_router.Internal.vue_router_cs
      (Vue_router.Internal.make_args_base ?mode !routes_t) in
    router := rt;
    app :=
      make
        ~data
        ~methods:(T methods)
        ~watch:(T watch_t)
        ~computed:(T computed)
        ~components:(T components_t)
        ~router:rt
        ~store
        S.id;
    if show then unhide ?suffix S.id;
    if export then Js.export S.id !app;
    !app

  let router () = !router
  let app () = !app

end
