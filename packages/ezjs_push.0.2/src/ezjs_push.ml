open Ezjs_min
open Promise

class type extendableEvent = object
  inherit Dom_html.event
  method waitUntil : Unsafe.any promise t -> unit meth
end

(** Notification API *)

class type notification_action = object
  method action : js_string t optdef readonly_prop
  method title : js_string t optdef readonly_prop
  method icon : js_string t optdef readonly_prop
end

class type notification_options = object
  method dir : js_string t optdef readonly_prop
  method lang : js_string t optdef readonly_prop
  method badge : js_string t optdef readonly_prop
  method body : js_string t optdef readonly_prop
  method tag : js_string t optdef readonly_prop
  method icon : js_string t optdef readonly_prop
  method image : js_string t optdef readonly_prop
  method data : Unsafe.any optdef readonly_prop
  method vibrate : int js_array t optdef readonly_prop
  method renotify : bool t optdef readonly_prop
  method requireInteraction : bool t optdef readonly_prop
  method actions : notification_action t js_array t optdef readonly_prop
  method silent : bool t optdef readonly_prop
  method timestamp : js_string t optdef readonly_prop
end

class type notification_static = object
  method permission : js_string t readonly_prop
  method maxActions : number t readonly_prop
  method requestPermission : (js_string t -> unit) callback -> unit meth
end

class type notification_instance = object
  method actions : notification_action t js_array t readonly_prop
  method badge : js_string t readonly_prop
  method body : js_string t readonly_prop
  method data : Unsafe.any readonly_prop
  method dir : js_string t readonly_prop
  method lang : js_string t readonly_prop
  method tag : js_string t readonly_prop
  method icon : js_string t readonly_prop
  method image : js_string t readonly_prop
  method renotify : bool t readonly_prop
  method requireInteraction : bool t readonly_prop
  method silent : bool t readonly_prop
  method timestamp : js_string t readonly_prop
  method title : js_string t readonly_prop
  method vibrate : int js_array t readonly_prop
  method onclick : (Dom_html.event t -> unit) callback prop
  method onclose : (Dom_html.event t -> unit) callback prop
  method onerror : (Dom_html.event t -> unit) callback prop
  method onshow : (Dom_html.event t -> unit) callback prop
  method close : unit meth
end

class type notificationEvent = object
  inherit extendableEvent
  method notification : notification_static t readonly_prop
  method action : js_string t readonly_prop
end

(** Push API *)

class type subscription_options = object
  method userVisibleOnly : bool t optdef readonly_prop
  method applicationServerKey : Typed_array.uint8Array t optdef readonly_prop
end

class type pushSubscription = object
  method endpoint : js_string t readonly_prop
  method exiprationTime : number t opt readonly_prop
  method options : subscription_options t readonly_prop
  method getKey : js_string t -> Typed_array.arrayBuffer t meth
  method toJSON : json t meth
  method unsubscribe : bool t promise t meth
end

class type pushManager = object
  method supportedContentEncodings : js_string t js_array t readonly_prop
  method getSubscription : pushSubscription t opt promise t meth
  method permissionState : subscription_options t optdef -> js_string t promise t meth
  method subscribe : subscription_options t optdef -> pushSubscription t promise t meth
end

class type pushMessageData = object
  method arrayBuffer : Typed_array.arrayBuffer t meth
  method blob : File.blob t meth
  method json : json t meth
  method text : js_string t meth
end

class type pushEvent = object
  inherit extendableEvent
  method data : pushMessageData t opt readonly_prop
end

(** Service Worker API *)

class type serviceWorker = object
  inherit [Unsafe.any, Unsafe.any] Js_of_ocaml.Worker.worker
  method scriptURL : js_string t readonly_prop
  method state : js_string t readonly_prop
  method onstatechange : (Dom_html.event t -> unit) callback prop
end

class type navigationPreloadState = object
  method enabled : bool t readonly_prop
  method headerValue : js_string t readonly_prop
end

class type navigationPreloadManager = object
  method enable : unit promise t meth
  method disable : unit promise t meth
  method setHeaderValue : js_string t -> unit promise t meth
  method getState : navigationPreloadState t promise t meth
end

class type sync_register_options = object
  method allowOnBattery : bool t optdef readonly_prop
  method id : js_string t optdef readonly_prop
  method idleRequired : bool t optdef readonly_prop
  method maxDelay : int optdef readonly_prop
  method minDelay : int optdef readonly_prop
  method minPeriod : int optdef readonly_prop
  method minRequiredNetwork : js_string t optdef readonly_prop
end

class type syncRegistration = object
end

class type syncManager = object
  method register : sync_register_options t optdef -> syncRegistration t promise t meth
  method getTags : js_string t js_array t promise t meth
end

class type ['a] contentIndex = object
  method add : 'a -> unit meth
  method delete : 'a -> unit meth
  method getAll : 'a js_array t promise t meth
end

class type get_notification_options = object
  method tag : js_string t readonly_prop
end

class type serviceWorkerRegistration = object
  inherit Dom_html.eventTarget
  method scope : js_string t readonly_prop
  method installing : serviceWorker t opt readonly_prop
  method waiting : serviceWorker t opt readonly_prop
  method active : serviceWorker t opt readonly_prop
  method navigationPreload : navigationPreloadManager t readonly_prop
  method pushManager : pushManager t readonly_prop
  method sync : syncManager t readonly_prop
  method index : _ contentIndex t readonly_prop
  method onupdatefound : (Dom_html.event -> unit) callback prop
  method getNotifications : get_notification_options t optdef ->
    notification_instance t js_array t promise t meth
  method showNotification :
    js_string t -> notification_options t optdef ->
    notificationEvent t promise t meth
  method update : serviceWorkerRegistration t promise t meth
  method unregister : bool t promise t meth
end

class type registration_options = object
  method scope : js_string t optdef readonly_prop
end

class type serviceWorkerContainer = object
  method controller : serviceWorker t opt prop
  method ready : serviceWorkerRegistration t promise t prop
  method register :
    js_string t -> registration_options t optdef ->
    serviceWorkerRegistration t promise t meth
  method getRegistration : registration_options t optdef ->
    serviceWorkerRegistration t optdef promise t meth
  method getRegistrations : serviceWorkerRegistration t js_array t promise t meth
  method startMessages : unit meth
end

class type navigator = object
  method serviceWorker : serviceWorkerContainer t readonly_prop
end

class type serviceWorkerGlobalScope = object
  method registration : serviceWorkerRegistration t readonly_prop
  method onactivate : (Dom_html.event t -> unit) callback prop
  method oncontentdelete : (Dom_html.event t -> unit) callback prop
  method onfetch : (Dom_html.event t -> unit) callback prop
  method oninstall : (Dom_html.event t -> unit) callback prop
  method onmessage : (Dom_html.event t -> unit) callback prop
  method onnotificationclick : (Dom_html.event t -> unit) callback prop
  method onnotificationclose : (Dom_html.event t -> unit) callback prop
  method onpush : (pushEvent t -> unit) callback prop
  method onpushsubscriptionchange : (Dom_html.event t -> unit) callback prop
  method onsync : (Dom_html.event t -> unit) callback prop
end

let navigator : navigator t = Unsafe.variable "navigator"
let notification : notification_static t = Unsafe.variable "Notification"
let self : serviceWorkerGlobalScope t = Unsafe.variable "self"

(** OCaml Helpers *)

type notification_action_ml = {
  na_action : string option;
  na_title : string option;
  na_icon : string option
}

let empty_action = { na_action = None; na_title = None; na_icon = None}

type 'data notification_options_ml = {
  no_dir : string option;
  no_lang : string option;
  no_badge : string option;
  no_body: string option;
  no_tag : string option;
  no_icon : string option;
  no_image : string option;
  no_data : 'data option;
  no_vibrate : int list option;
  no_renotify : bool option;
  no_require_interaction : bool option;
  no_actions : notification_action_ml list option;
  no_silent : bool option;
  no_timestamp : string option;
}

let empty_notif = {
  no_dir = None; no_lang = None; no_badge = None; no_body = None; no_tag = None;
  no_icon = None; no_image = None; no_data = None; no_vibrate = None;
  no_renotify = None; no_require_interaction = None; no_actions = None;
  no_silent = None; no_timestamp = None
}

type subscription_options_ml = {
  so_user_visible_only: bool option;
  so_application_server_key: string option;
}

let request_permission f = notification##requestPermission (wrap_callback f)
let permission () = to_string notification##.permission

let service_worker () = navigator##.serviceWorker

let register_worker ?scope ?callback path =
  let service_worker = service_worker () in
  let options = optdef (fun s -> object%js val scope = def (string s) end) scope in
  jthen ~error:(fun r -> log "service worker %S registration failed" path; js_log r)
    (service_worker##register (string path) options)
    (fun _ -> log "service worker %S registered" path;
      match callback with None -> () | Some cb -> cb ())

let get_registration ?(none=fun () -> log_str "No service worker registered") ?scope f =
  let service_worker = service_worker () in
  let options = optdef (fun s -> object%js val scope = def (string s) end) scope in
  jthen (service_worker##getRegistration options) (fun reg -> Optdef.case reg none f)

let registration path f =
  get_registration ~none:(fun () ->
      register_worker ~callback:(fun () ->
          let service_worker = service_worker () in
          jthen service_worker##.ready f) path) f

let update_worker ?callback reg = jthen_opt reg##update callback
let unregister_worker reg = ignore @@ reg##unregister

let get_registrations f =
  let sw = service_worker () in
  jthen sw##getRegistrations (fun a -> f (to_list a))

let unregister_all () =  get_registrations (List.iter unregister_worker)

let push_manager reg = reg##.pushManager

let onactivate f = self##.onactivate := wrap_callback f
let onpush f = self##.onpush := wrap_callback f
let onfetch f = self##.onfetch := wrap_callback f
let oninstall f = self##.oninstall := wrap_callback f
let onmessage f = self##.onmessage := wrap_callback f
let onnotificationclick f = self##.onnotificationclick := wrap_callback f
let onnotificationclose f = self##.onnotificationclose := wrap_callback f
let onpushsubscriptionchange f = self##.onpushsubscriptionchange := wrap_callback f
let onsync f = self##.onsync := wrap_callback f

let urlBase64ToUint8Array s =
  let n = String.length s in
  let s = s ^ String.make ((4 - n mod 4) mod 4) '=' in
  Typed_array.Bigstring.to_uint8Array @@ Bigstring.of_string @@
  Base64.(decode_exn ~alphabet:uri_safe_alphabet) s

let make_subscription_options {so_user_visible_only; so_application_server_key} : subscription_options t =
  object%js
    val userVisibleOnly = optdef bool so_user_visible_only
    val applicationServerKey = optdef urlBase64ToUint8Array so_application_server_key
  end

let subscribe ?(verbose=false) ?options reg f =
  let push_manager = push_manager reg in
  jthen (push_manager##subscribe (optdef make_subscription_options options))
    (fun subscr ->
       log_str "subscribed to push messages";
       if verbose then js_log subscr;
       f subscr)

let get_subscription ?(none=fun () -> log_str "No subscription") reg f =
  let push_manager = push_manager reg in
  jthen push_manager##getSubscription (fun subs -> Opt.case subs none f)

let subscription ?verbose ?options reg f =
  get_subscription ~none:(fun () -> subscribe ?verbose ?options reg f) reg f

let make_notification_action {na_action; na_title; na_icon} : notification_action t =
  object%js
    val action = optdef string na_action
    val title = optdef string na_title
    val icon = optdef string na_icon
  end

let make_notification_options o : notification_options t =
  object%js
    val dir = optdef string o.no_dir
    val lang = optdef string o.no_lang
    val badge = optdef string o.no_badge
    val body = optdef string o.no_body
    val tag = optdef string o.no_tag
    val icon = optdef string o.no_icon
    val image = optdef string o.no_image
    val data = optdef Unsafe.inject o.no_data
    val vibrate = optdef of_list o.no_vibrate
    val renotify = optdef bool o.no_renotify
    val requireInteraction = optdef bool o.no_require_interaction
    val actions = optdef (of_listf make_notification_action) o.no_actions
    val silent = optdef bool o.no_silent
    val timestamp = optdef string o.no_timestamp
  end

let show_notification ?callback ?options (reg : serviceWorkerRegistration t) title =
  let options = optdef make_notification_options options in
  jthen_opt reg##(showNotification (string title) options) callback
