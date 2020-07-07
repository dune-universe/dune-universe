open Js_of_ocaml
open Js
open Vue_router


let () =
  let router = Vue_router.make [
      { (Vue_router.empty "/foo") with
        component = Some { Vue_component.empty with template = Some "<div>foo</div>" } };
      { (Vue_router.empty "/bar") with
        component = Some { Vue_component.empty with template = Some "<div>bar</div>" } } ] in
  export "app" @@ Vue_js.make ~router "app"
