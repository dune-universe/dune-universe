open Js_of_ocaml
open Js

module Foo = struct

  include Vue_js.SPA(struct
      class type data = object
        method payload_foo_ : int prop
      end
      class type state = object
        method count_foo_ : int prop
      end
      class type all = object
        inherit data
        inherit state
      end
      let name = "foo"
      let template =
        "<div>
        <h4>Foo Page</h4>
        <input v-model='payload_foo'/>
        <button @click='increment_foo(payload_foo)'>increment</button>
        <span>state: {{ count_foo }}</span>
        </div>"
      let props =[]
    end)

  let data : data t = object%js val mutable payload_foo_ = 0 end
  let state : state t = object%js val mutable count_foo_ = 0 end
  let mutations = [
    "increment_foo", (fun state payload ->
        state##.count_foo_ :=
          state##.count_foo_ +
          (int_of_float @@ float_of_number @@ Unsafe.coerce payload)) ]

end

module Bar = struct

  include Vue_js.SPA(struct
      class type data = object
        method payload_bar_ : int prop
      end
      class type state = object
        method count_bar_ : int prop
      end
      class type all = object
        inherit data
        inherit state
      end
      let name = "bar"
      let template =
        "<div>
         <h4>Foo Page</h4>
         <input v-model='payload_bar'/>
         <button @click='increment_bar(payload_bar)'>increment</button>
         <span>state: {{ count_bar }}</span>
         </div>"
      let props = []
    end)
  let data : data t = object%js val mutable payload_bar_ = 0 end

  let state : state t = object%js val mutable count_bar_ = 0 end
  let mutations = [
    "increment_bar", (fun state payload ->
        state##.count_bar_ :=
          state##.count_bar_ +
          (int_of_float @@ float_of_number @@ Unsafe.coerce payload)) ]

end

module Root = struct
  include Vue_js.Root(struct
      class type data = object end
      class type state = object end
      class type all = object end
      let id = "app"
    end)

  let data = Unsafe.obj [||]
  let state = Unsafe.obj [||]

  let foo_name, foo_store, foo_route = Foo.(init ~mutations ~state ~data ())
  let bar_name, bar_store, bar_route = Bar.(init ~mutations ~state ~data ())
  let routes = [
    Unsafe.coerce foo_route;
    Unsafe.coerce bar_route ]
  let modules = [
    foo_name, Unsafe.coerce foo_store;
    bar_name, Unsafe.coerce bar_store ]

end

let () =
  let app = Root.(init ~routes ~modules ~data ~state ()) in
  ignore app
