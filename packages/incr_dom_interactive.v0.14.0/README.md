# `Incr_dom_interactive`

A monad for composing chains of interactive UI elements inside an `Incr_dom`
application.

<i> 
  A form that only allows submission after 10 characters have been entered
</i>

```ocaml
let open Interactive.Let_syntax in
let open Interactive.Primitives in
let submit_button = button ~text:"Submit" () in
let%bind_open user_input = text () in
if String.length user_input < 10
then
  let%map_open () = message "Please enter at least 10 characters." in
  None
else
  match%map submit_button with
  | Not_pressed -> None
  | Pressed -> Some user_input
```

