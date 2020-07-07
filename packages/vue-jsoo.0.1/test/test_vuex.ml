open Js_of_ocaml
open Js
open Mjs
open Vuex

class type todo = object
  method id : int readonly_prop
  method text : js_string t readonly_prop
  method done_ : bool t readonly_prop
end

class type state = object
  method count : int prop
  method todos : todo t js_array t prop
end

let () =
  (** vue options *)
  let data = object%js
    val payload = 0
    val payload_async_ = 0
  end in

  (** store *)
  let state : state t = object%js
    val mutable count = 0
    val mutable todos = array [|
        object%js val id = 1 val text = string "..." val done_ = _true end;
        object%js val id = 2 val text = string "..." val done_ = _false end;
      |]
  end in
  let increment state payload =
    let payload = int_of_float @@ float_of_number @@ Unsafe.coerce payload in
    state##.count := state##.count + payload in
  let done_todos state =
    to_any @@ manip_list (List.filter (fun x -> x##.done_ = _true)) state##.todos in
  let get_todo state =
    let f id =
      let todos = Mjs.to_list state##.todos in
      match List.find_opt (fun x -> x##.id = id) todos with
      | None -> undefined
      | Some todo -> def todo in
    to_any f in
  let increment_async store payload =
    ignore @@
    Dom_html.window##setTimeout
      (wrap_callback (fun () ->
           store.commit ~payload "increment" )) 1000. in
  let options = {
    (empty state) with
    mutations = L [ "increment", increment ];
    o_getters = L [ "doneTodos", done_todos; "getTodoById", get_todo ];
    actions = L ["incrementAsync", increment_async ];
  } in
  let store : top store = Unsafe.coerce @@ make options in

  let methods = T (Table.merge [
      Map.mutations ["increment"];
      Map.actions ["incrementAsync"];
    ]) in
  let computed = T (Table.merge [
      Map.state ["count"; "todos"];
      Map.getters ["doneTodos"; "getTodoById"]]) in

  (** create vue *)
  export "app" @@ Vue_js.make ~data ~methods ~computed ~store "app";
