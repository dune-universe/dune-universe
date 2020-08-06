open Js_of_ocaml

class type ['a] promise = object
  method _then : ('a -> unit) -> 'a promise Js.t Js.meth
  method catch : ('a -> unit) -> 'a promise Js.t Js.meth
end

class type notification_action = object
  method action : Js.js_string Js.t Js.prop
  method title : Js.js_string Js.t Js.prop
  method icon : Js.js_string Js.t Js.prop
end

class type notification_options = object
  method actions : notification_action Js.t Js.js_array Js.t Js.prop
  method body : Js.js_string Js.t Js.prop
  method icon : Js.js_string Js.t Js.prop
  method tag : Js.js_string Js.t Js.prop
  method vibrate : int Js.js_array Js.t Js.prop
  method requireInteraction : bool Js.t Js.prop
  method data : Js.Unsafe.any Js.prop
end

class type subscription_options = object
  method userVisibleOnly : bool Js.t Js.prop
  method applicationServerKey : Js_of_ocaml.Typed_array.uint8Array Js.t Js.prop
end

class type notification = object
  method permission : Js.js_string Js.t Js.readonly_prop
  method requestPermission : string Js.t promise Js.t Js.meth
end

class type notificationEvent = object
  method notification : notification Js.t Js.readonly_prop
end

class type pushSubscription = object
  method endpoint : Js.js_string Js.t Js.prop
  method toJSON : Js.json Js.t Js.meth
  method getKeys : Js_of_ocaml.Typed_array.arrayBuffer Js.meth
  method unsubscribe : bool Js.t promise Js.t Js.meth
end

class type pushManager = object
  method subscribe : subscription_options Js.t -> pushSubscription Js.t promise Js.t Js.meth
  method getSubscription : pushSubscription Js.t Js.opt promise Js.t Js.meth
end

class type serviceWorkerRegistration = object
  method showNotification :
    Js.js_string Js.t -> notification_options Js.t ->
    notificationEvent Js.t promise Js.t Js.meth
  method pushManager : pushManager Js.t Js.prop
  method update : serviceWorkerRegistration Js.t promise Js.t Js.meth
  method unregister : bool Js.t promise Js.t Js.meth
end

class type serviceWorkerContainer = object
  method register :
    Js.js_string Js.t -> serviceWorkerRegistration Js.t promise Js.t Js.meth
  method getRegistration : serviceWorkerRegistration Js.t Js.optdef promise Js.t Js.meth
end

class type navigator = object
  method serviceWorker : serviceWorkerContainer Js.t Js.readonly_prop
end

class type pushMessageData = object
  method arrayBuffer : Js_of_ocaml.Typed_array.arrayBuffer Js.meth
  method json : Js.json Js.t Js.meth
  method text : Js.js_string Js.t Js.meth
end

let navigator : navigator Js.t = Js.Unsafe.variable "navigator"
let notification : notification Js.t = Js.Unsafe.variable "Notification"

type notification_action_ml = {
  na_action: string option;
  na_title: string option;
  na_icon: string option
}

type data = string

type notifictaion_options_ml = {
  no_actions: notification_action_ml list option;
  no_body: string option;
  no_icon: string option;
  no_tag: string option;
  no_vibrate: int list option;
  no_require_interaction: bool option;
  no_data: data option
}

type subscrition_options_ml = {
  so_user_visible_only: bool option;
  so_application_server_key: string option;
}

let jthen prom f = prom##(_then f)
let then1 prom f = ignore (jthen prom f)
let then0 prom = then1 prom (fun _ -> ())
let catch prom f = prom##(catch f)
let catch1 prom f = ignore (catch prom f)

let unopt def f = function
  | None -> def
  | Some x -> f x

let request_permission f = then1 notification##requestPermission f
let permission () = Js.to_string notification##.permission

let service_worker () = navigator##.serviceWorker

let register_worker path f =
  let service_worker = service_worker () in
  then1 service_worker##(register (Js.string path)) f

let get_registration ?(none=fun () -> Firebug.console##log (Js.string "No service worker")) f =
  let service_worker = service_worker () in
  then1 service_worker##getRegistration (fun reg ->
      Js.Optdef.case reg none f)

let registration path f =
  get_registration ~none:(fun () -> register_worker path f) f

let update_service_worker reg = reg##update |> ignore
let unregister_service_worker reg = reg##unregister |> ignore

let push_manager reg = reg##.pushManager

let urlBase64ToUint8Array str =
  let str = Bytes.of_string str in
  let n = Bytes.length str in
  let str = Bytes.extend str 0 ((4 - n mod 4) mod 4) in
  Bytes.fill str n ((4 - n mod 4) mod 4) '=';
  let str = Bytes.map (fun c ->
      if c = '-' then '+'
      else if c = '_' then '/'
      else c) str in
  let str = Bytes.to_string str in
  let str = Dom_html.window##(atob (Js.string str)) in
  let a = Array.init (str##.length) (fun i ->
      int_of_float @@ Js.to_float str##(charCodeAt i)) in
  new%js Js_of_ocaml.Typed_array.uint8Array_fromArray (Js.array a)

let make_subscription_options {so_user_visible_only; so_application_server_key} =
  let opts : subscription_options Js.t = Js.Unsafe.obj [||] in
  unopt () (fun visible -> opts##.userVisibleOnly := Js.bool visible)
    so_user_visible_only;
  unopt () (fun key ->
      opts##.applicationServerKey := urlBase64ToUint8Array key)
    so_application_server_key;
  opts

let subscribe ?options reg f =
  let options : subscription_options Js.t =
    unopt (Js.Unsafe.obj [||]) make_subscription_options options in
  let push_manager = push_manager reg in
  then1 push_manager##(subscribe options) f

let get_subscription ?(none=fun () -> Firebug.console##log (Js.string "No subscription")) reg f =
  let push_manager = push_manager reg in
  then1 push_manager##getSubscription (fun subs ->
      Js.Opt.case subs none f)

let subscription ?options reg f =
  get_subscription ~none:(fun () -> subscribe ?options reg f) reg f

let make_notification_action {na_action; na_title; na_icon}  =
  let a : notification_action Js.t = Js.Unsafe.obj [||] in
  unopt () (fun action -> a##.action := Js.string action) na_action;
  unopt () (fun title -> a##.title := Js.string title) na_title;
  unopt () (fun icon -> a##.icon := Js.string icon) na_icon;
  a

let make_notification_options options =
  let opts : notification_options Js.t = Js.Unsafe.obj [||] in
  unopt () (fun body -> opts##.body := Js.string body) options.no_body;
  unopt () (fun icon -> opts##.icon := Js.string icon) options.no_icon;
  unopt () (fun tag -> opts##.tag := Js.string tag) options.no_tag;
  unopt () (fun vibrate ->
      opts##.vibrate := Js.array (Array.of_list vibrate)) options.no_vibrate;
  unopt () (fun actions ->
      opts##.actions := Js.array (Array.of_list (List.map make_notification_action actions)))
    options.no_actions;
  unopt () (fun require -> opts##.requireInteraction := Js.bool require)
    options.no_require_interaction;
  unopt () (fun data -> opts##.data := Js.Unsafe.inject data) options.no_data;
  opts

let show_notification ?options title reg =
  let options : notification_options Js.t =
    unopt (Js.Unsafe.obj [||]) make_notification_options options in
  then0 reg##(showNotification (Js.string title) options)
