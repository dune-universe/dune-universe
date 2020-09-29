# vue-jsoo

vue-jsoo is a js_of_ocaml binding of vue-js

There are separate implementations of vue components in Vue_components, vue router in Vue_router and vuex in Vuex.
Vue_js also provides functors to help structure a vue project.

The API documentation is available here: [API Reference](https://o-labs.gitlab.io/vue-jsoo/vue-jsoo/index.html)

## Simple Vue

In the .ml file:

    open Js_of_ocaml

    let data = object%js
      val message = string "Hello Vue.js!"
    end

    let methods = Mjs.L [
      "reverseMessage", Mjs.to_any (wrap_meth_callback (fun this () ->
          let s = Js.to_string this##.message in
          let n = String.length s in
          let s = String.mapi (fun i _c -> String.get s (n-i-1)) s in
          this##.message := Js.string s ]

    let () =
      Vue_js.make ~data ~methods "app"

In the .html file:

    <div id="app">
      <p>{{ message }}</p>
      <button v-on:click="reverseMessage()">Reverse Message</button>
    </div>

## Vue_component

In the .ml file:

    open Mjs
    open Vue_component

    let template =
      "<div>
       <h4>Component Name : {{ name }}</h4>
       <input v-model='payload'/>
       <button @click='increment()'>increment</button>
       <span>count: {{ count }}</span>
       </div>"

    let data _this = object%js
      val mutable count = 0
      val mutable payload = 0
    end

    let methods = L [
      "increment_foo", to_any (wrap_meth_callback (fun this () ->
        this##.count := this##.count + this##.payload)) ]

    let load () =
      make ~template ~props:(PrsArray [ "name" ]) ~methods ~data "component"

    let () =
      load ();
      Vue_js.make "app"

In the .html file:

    <div id="app"
      <component name="test"></component>
    </div>

## Vue_router

In the .ml file:

    open Mjs
    open Vue_router

    let () =
      let router = make [
        { (empty "/foo") with
          component = Some { Vue_component.empty with template = Some "<div>foo</div>" } };
        { (Vue_router.empty "/bar") with
          component = Some { Vue_component.empty with template = Some "<div>bar</div>" } } ] in

      Vue_js.make ~router "app"

In the .html file:

    <div id="app">
      <p>
        <router-link to="/foo">Go to Foo</router-link>
        <router-link to="/bar">Go to Bar</router-link>
      </p>
      <router-view></router-view>
    </div>

## Vuex

In the .ml files:

    open Js_of_ocaml
    open Js
    open Mjs
    open Vuex

    let () =
      let data = object%js
        val payload = 0
        val payload_async_ = 0
      end in

      let state : state t = object%js
        val mutable count = 0
        val mutable todos = array [|
          object%js val id = 1 val text = string "..." val done_ = _true end;
          object%js val id = 2 val text = string "..." val done_ = _false end;
        |]
      end in

      let increment state payload =
        let payload = int_of_float @@ float_of_number @@ coerce payload in
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

      let store = make options in

      let methods = T (Table.merge [
        Map.mutations ["increment"];
        Map.actions ["incrementAsync"];
       ]) in

      let computed = T (Table.merge [
        Map.state ["count"; "todos"];
        Map.getters ["doneTodos"; "getTodoById"]]) in

      Vue_js.make ~data ~methods ~computed ~store "app"

In the .html file:

    <div id="app">
      <div>
        <h4>Test mutation</h4>
        <input v-model="payload"/>
        <button @click="increment(payload)">increment</button>
        <span>state: {{ count }}</span>
      </div>

      <div>
        <h4>Test getters</h4>
        <div>{{ doneTodos }}</div>
        <div>{{ getTodoById(2) }}</div>
      </div>

      <div>
        <h4>Test action</h4>
        <input v-model="payload_async"/>
        <button @click="incrementAsync(payload_async)">increment</button>
        <span>state: {{ count }}</span>
      </div>
    </div>

## Simple Functor

In the .ml file:

    open Js_of_ocaml
    open Vue_js

    module V = Make(struct
      let id = "app"

      class type data = object
        method message : Js.js_string Js.t Js.prop
      end

      type all = data
    end)

    V.add_method0 "reverseMessage" (fun this ->
      let s = Js.to_string this##.message in
      let n = String.length s in
      let s = String.mapi (fun i _c -> String.get s (n-i-1)) s in
      this##.message := Js.string s)

    let () =
      let data = object%js
        val message = "Hello world!"
      end in

      V.init data

## Functor for SPA

In the .ml file:

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

    module Root = Vue_js.Root(struct
        class type data = object end
        class type state = object end
        class type all = object end
        let id = "app"
    end)

    let () =
      Root.add_spa @@ Foo.(make ~mutations ~state ~data ());
      Root.add_spa @@ Bar.(make ~mutations ~state ~data ());
      let app = Root.(init ~routes ~modules ~data ~state ()) in
      ignore app

In the .html file:

    <div id="app">
      <p>
        <router-link to="/foo">Go to Foo</router-link>
        <router-link to="/bar">Go to Bar</router-link>
      </p>
      <router-view></router-view>
    </div>
