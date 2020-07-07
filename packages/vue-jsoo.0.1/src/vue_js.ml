open Js_of_ocaml
open Js
open Mjs

type 'all vue = 'all t

class type ['data, 'all, 'state] vue_arg = object
  method el : js_string t readonly_prop
  method data : 'data t readonly_prop
  method computed : ('all t, unit -> any optdef) meth_callback table optdef readonly_prop
  method watch : ('all t, any -> any -> any) meth_callback table optdef readonly_prop
  method methods : any table optdef readonly_prop
  method components : (top, top) Vue_component.Internal.component_arg table optdef readonly_prop
  method router : Vue_router.router optdef readonly_prop
  method store : 'state Vuex.store optdef readonly_prop
end

type ('data, 'all, 'state) vue_cs = (('data, 'all, 'state) vue_arg t -> 'all t) constr

let make ?computed ?watch ?methods ?data ?components ?router ?store id =
  let data : 'data t = match data with None -> Unsafe.obj [||] | Some d -> d in
  let v : ('data, 'all, 'state) vue_arg t = object%js
    val el = string ("#" ^ id)
    val data = data
    val computed = optdef (to_tablef (fun c -> wrap_meth_callback (fun this () -> c this))) computed
    val watch = optdef (to_tablef wrap_meth_callback) watch
    val methods = optdef to_table methods
    val components = optdef to_table components
    val router = Optdef.option router
    val store = Optdef.option store
  end in
  let cs : ('data, 'all, 'state) vue_cs = Unsafe.global##._Vue in
  new%js cs v

let get_prop vm s = Unsafe.get vm @@ "$" ^ s
let get_ref vm id =
  let refs = get_prop vm "refs" in
  Unsafe.get refs id

let get_router vm = get_prop vm "router"
let get_store vm = get_prop vm "store"
let get_route vm = get_prop vm "route"

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

  include Vue_component.Tables(struct type all = S.all end)

  let app : all t ref = ref (Unsafe.obj [||])

  let init ?(export=true) ?(data : S.data t option) ?(show=false) ?suffix
      ?router ?store () =
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

module SPA(S : sig
    type data
    type all
    type state
    val name : string
    val template : string
    val props : string list
  end) = struct

  type data = S.data
  include Vuex.Module(struct type state = S.state end)
  include Vue_component.Tables(struct type all = S.all end)
  include Vue_router.Tables

  let store : state Vuex.module_obj ref = ref (Unsafe.obj [||])
  let route : (data, all) Vue_router.Internal.route t ref = ref (Unsafe.obj [||])

  let init ?getters ?mutations ?actions ?modules ?computed ?methods ?watch ?components
      ?children ?hook ?path ~data ~state () =
    merge_lists_component ?computed ?methods ?watch ?components ();
    merge_routes ?routes:children ();
    let st = get ?getters ?mutations ?actions ?modules ~namespaced:true state in
    let computed = Table.merge [
        computed_t;
        Vuex.Map.state ~namespace:S.name (to_listf to_string @@ object_keys state);
        Vuex.Map.getters ~namespace:S.name (Table.keys getters_t) ] in
    let methods = Table.merge [
        methods_t;
        Vuex.Map.mutations ~namespace:S.name (Table.keys mutations_t);
        Vuex.Map.actions ~namespace:S.name (Table.keys actions_t);
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
        name = Some S.name } in
    let path = match path with None -> "/" ^ S.name | Some path -> path in
    let r = Vue_router.Internal.make_route_base {
        (Vue_router.empty path) with
        Vue_router.component;
        props = (match S.props with [] -> None | _ -> Some (Vue_router.PrBool true));
        children = Some !routes_t;
        hook
      } in
    store := st;
    route := r;
    S.name, st, r

  let store () = !store
  let route () = !route

end

module Root(S : sig
    type data
    type all
    type state
    val id : string
  end) = struct

  type data = S.data
  include Vuex.Make(struct type state = S.state end)
  include Vue_component.Tables(struct type all = S.all end)
  include Vue_router.Tables

  let app : all vue ref = ref (Unsafe.obj [||])
  let router : Vue_router.router ref = ref (Unsafe.obj [||])

  let init ?getters ?mutations ?actions ?modules ?computed ?methods ?watch
      ?components ?routes ?strict ?devtools ?plugins ?mode
      ?(show=false) ?(export=true) ?suffix ~(data: data t) ~state () =
    merge_lists_component ?computed ?methods ?watch ?components ();
    merge_routes ?routes ();
    let store = init ?plugins ?getters ?mutations ?actions ?modules ?strict ?devtools state in
    let computed = Table.merge [
        computed_t;
        Vuex.Map.state (to_listf to_string @@ object_keys state);
        Vuex.Map.getters (Table.keys getters_t);
      ] in
    let methods = Table.merge [
        methods_t;
        Vuex.Map.mutations (Table.keys mutations_t);
        Vuex.Map.actions (Table.keys actions_t);
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
