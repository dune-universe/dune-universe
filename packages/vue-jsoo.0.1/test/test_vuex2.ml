open Js_of_ocaml
open Js

module Foo = struct

  class type data = object
    method payload_foo_ : int prop
  end

  class type state = object
    method count_foo_ : int prop
  end

  let template =
    "<div>
     <h4>Foo Page</h4>
     <input v-model='payload_foo'/>
     <button @click='increment_foo(payload_foo)'>increment</button>
     <span>state: {{ count_foo }}</span>
     </div>"

  let data : data t = object%js val mutable payload_foo_ = 0 end
  let computed = Vuex.Map.state ~namespace:"foo" ["count_foo"]
  let methods = Vuex.Map.mutations ~namespace:"foo" ["increment_foo"]

  let component = Some {
      Vue_component.empty with
      template = Some template;
      data = Some (fun _ -> Unsafe.coerce data);
      computed = Some (T computed);
      methods = Some (T methods);
      name = Some "foo" }

  let route = { (Vue_router.empty "/foo") with component }

  let state : state t = object%js val mutable count_foo_ = 0 end

  let store = Vuex.make_module {
      (Vuex.empty state) with
      namespaced = Some true;
      mutations = L ["increment_foo", (fun state payload ->
          state##.count_foo_ :=
            state##.count_foo_ +
            (int_of_float @@ float_of_number @@ Unsafe.coerce payload)) ]}
end

module Bar = struct

  class type data = object
    method payload_bar_ : int prop
  end

  class type state = object
    method count_bar_ : int prop
  end

  let template =
    "<div>
     <h4>Foo Page</h4>
     <input v-model='payload_bar'/>
     <button @click='increment_bar(payload_bar)'>increment</button>
     <span>state: {{ count_bar }}</span>
     </div>"

  let data : data t = object%js val mutable payload_bar_ = 0 end
  let computed = Vuex.Map.state ~namespace:"bar" ["count_bar"]
  let methods = Vuex.Map.mutations ~namespace:"bar" ["increment_bar"]

  let component = Some {
      Vue_component.empty with
      template = Some template;
      data = Some (fun _ -> Unsafe.coerce data);
      computed = Some (T computed);
      methods = Some (T methods);
      name = Some "bar" }

  let route = { (Vue_router.empty "/bar") with component }

  let state : state t = object%js val mutable count_bar_ = 0 end

  let store = Vuex.make_module {
      (Vuex.empty state) with
      namespaced = Some true;
      mutations = L ["increment_bar", (fun state payload ->
          state##.count_bar_ :=
            state##.count_bar_ +
            (int_of_float @@ float_of_number @@ Unsafe.coerce payload)) ]}


end

let () =
  let data = Unsafe.obj [||] in
  let state = Unsafe.obj [||] in
  let router = Vue_router.make [Foo.route; Bar.route] in
  let store = Vuex.make {
      (Vuex.empty state) with
      modules = L [ "foo", Unsafe.coerce Foo.store;
                    "bar", Unsafe.coerce Bar.store ] } in
  export "app" @@ Vue_js.make ~router ~store ~data "app"
