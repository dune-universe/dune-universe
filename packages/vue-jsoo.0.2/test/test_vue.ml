open Js_of_ocaml.Js
open Js_of_ocaml.Firebug

let () =

  let data = object%js
    val message = string "Hello Vue!"
  end in
  export "app" @@ Vue_js.make ~data "app";

  let data = object%js
    val message = string @@ "You loaded this page on " ^ (to_string @@ (new%js date_now)##toLocaleString)
  end in
  export "app2" @@ Vue_js.make ~data "app-2";

  let data = object%js
    val seen = _true
  end in
  export "app3" @@ Vue_js.make ~data "app-3";

  let data = object%js
    val todos = array [|
        object%js val text = "Learn JavaScript" end;
        object%js val text = "Learn Vue" end;
        object%js val text = "Build something awesome" end |]
  end in
  export "app4" @@ Vue_js.make ~data "app-4";

  let data = object%js
    val message = string "Hello Vue.js!"
  end in
  let methods = Mjs.L [
    "reverseMessage", Mjs.to_any (wrap_meth_callback (fun self x y ->
          Js_of_ocaml.Firebug.console##log_3 self x y;
          let s = to_string self##.message in
          let n = String.length s in
          let s = String.mapi (fun i _c -> String.get s (n-i-1)) s in
          self##.message := string s;
          Mjs.to_any ()))
  ] in
  export "app5" @@ Vue_js.make ~data ~methods "app-5";

  let data = object%js
    val message = string "Hello Vue!"
  end in
  let watch = Mjs.L [
    "message", fun _self new_value old_value ->
      console##log_3 (string "watch_message") new_value old_value;
      Mjs.to_any ()
  ] in
  export "app6" @@ Vue_js.make ~data ~watch "app-6";

  let _ = Vue_component.make
      ~props:(PrsArray ["todo"]) ~template:"<li>{{ todo.text }}</li>" "todo-item" in
  let data = object%js
    val groceryList = array [|
        object%js val id = 0 val text = string "Vegetables" end;
        object%js val id = 1 val text = string "Cheese" end;
        object%js val id = 2 val text = string "Nutella" end |]
  end in
  export "app7" @@ Vue_js.make ~data "app-7";

  let data = object%js
    val message = string "Hello Vue!"
  end in
  let computed = Mjs.L [
    "message2", fun self ->
      console##log (string "test computed");
      def (Mjs.to_any @@ string (to_string self##.message ^ "_balbla"))
  ] in
  export "app8" @@ Vue_js.make ~data ~computed "app-8"
