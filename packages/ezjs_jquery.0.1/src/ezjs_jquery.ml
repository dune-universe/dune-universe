open Js_of_ocaml
open Js
open Dom_html

let make_json s = Json.unsafe_input (Js.string s)

class type promise = object
  method _then : ('a -> unit) -> promise t meth
  method catch : ('a -> unit) -> promise t meth
end

(* jquery options *)

class type options = object
  method duration : int prop
  method easing : js_string t prop
  method queue : bool t prop
  method specialEasing : json t prop
  (* method step : (int -> tween -> bool t) callback prop *)
  method progress : (promise t -> int -> int -> bool t) callback prop
  method complete : (unit -> bool t) callback prop
  method start : (promise t -> bool t) callback prop
  method _done : (promise t -> bool t -> bool t) callback prop
  method fail : (promise t -> bool t -> bool t) callback prop
  method always : (promise t -> bool t -> bool t) callback prop
end

let set_opt f = function
  | None -> ()
  | Some x -> f x

let make_options ?duration ?easing ?queue ?special ?progress
    ?complete ?start ?done_opt ?fail ?always () =
  let options : options t = Unsafe.obj [||] in
  set_opt (fun d -> options##.duration := d) duration;
  set_opt (fun e -> options##.easing := string e) easing;
  set_opt (fun q -> options##.queue := bool q) queue;
  set_opt (fun s -> options##.specialEasing := s) special;
  set_opt (fun p -> options##.progress := wrap_callback p) progress;
  set_opt (fun c -> options##.complete := wrap_callback c) complete;
  set_opt (fun s -> options##.start := wrap_callback s) start;
  set_opt (fun d -> options##._done := wrap_callback (fun p b -> d p (to_bool b))) done_opt;
  set_opt (fun f -> options##.fail := wrap_callback (fun p b -> f p (to_bool b))) fail;
  set_opt (fun a -> options##.always := wrap_callback (fun p b -> a p (to_bool b))) always


(* jquery position *)

class type position = object
  method top : js_string t prop
  method left : js_string t prop
end

type position_ml = {top : string; left : string}

let make_position {top; left} =
  let pos : position t = Unsafe.obj [||] in
  pos##.top := string top;
  pos##.left := string left;
  pos
let position_top (p: position t) = to_string p##.top
let position_left (p: position t) = to_string p##.left
let positions (p:position t) = {top = to_string p##.top; left = to_string p##.left}


(* jquery event *)

class type event_jq = object
  method currentTarget : element t prop
  method data : 'a t prop
  method delegateTarget : element t prop
  method isDefaultPrevented : bool t meth
  method isImmediatePropagationStopped : bool t meth
  method isPropagationStopped : bool t meth
  method metaKey : bool t prop
  method namespace : js_string t prop
  method pageX : int prop
  method pageY : int prop
  method preventDefault : unit meth
  method relatedTarget : element t prop
  method result : 'a t prop
  method stopImmediatePropagation : unit meth
  method stopPropagation : unit meth
  method target : element t prop
  method timeStamp : int prop
  method _type : js_string t prop
  method which : int prop
end

let current_target (e:event_jq t) = e##.currentTarget
let event_data (e:event_jq t) = e##.data
let delegate_target (e:event_jq t) = e##.delegateTarget
let is_default_prevented (e:event_jq t) = to_bool e##isDefaultPrevented
let is_immediate_propagation_stopped (e:event_jq t) = to_bool e##isImmediatePropagationStopped
let is_propagation_stopped (e:event_jq t) = to_bool e##isPropagationStopped
let meta_key (e:event_jq t) = e##.metaKey
let event_namespace (e:event_jq t) = to_string e##.namespace
let pageX (e:event_jq t) = e##.pageX
let pageY (e:event_jq t) = e##.pageY
let prevent_default (e:event_jq t) = e##preventDefault
let related_target (e:event_jq t) = e##.relatedTarget
let event_result (e:event_jq t) = e##.result
let stop_immediate_propagation (e:event_jq t) = e##stopImmediatePropagation
let stop_propagation (e:event_jq t) = e##stopPropagation
let event_target (e:event_jq t) = e##.target
let event_timestamp (e:event_jq t) = e##.timeStamp
let event_type (e:event_jq t) = to_string e##._type
let which (e:event_jq t) = e##.which


(* jquery deferred *)

class type deferred = object
  method always : (unit -> bool t) callback -> (unit -> bool t) callback opt -> deferred t meth
  method catch : (unit -> bool t) callback -> promise t meth
  method _done : (unit -> bool t) callback -> (unit -> bool t) callback opt -> deferred t meth
  method fail : (unit -> bool t) callback -> (unit -> bool t) callback opt -> deferred t meth
  method isRejected : bool t meth
  method isResolved : bool t meth
  method notify : 'a t -> deferred t meth
  method notifyWith : 'a t -> 'a t opt -> deferred t meth
  method pipe : (unit -> bool t) callback opt -> (unit -> bool t) callback opt -> promise t meth
  method pipe_prg : (unit -> bool t) callback opt -> (unit -> bool t) callback opt
    -> (unit -> bool t) callback opt -> promise t meth
  method progress : (unit -> bool t) callback -> (unit -> bool t) callback opt -> deferred t meth
  method promise : 'a t opt -> promise t meth
  method reject : 'a t opt -> deferred t meth
  method rejectWith : 'a t -> 'a t opt -> deferred t meth
  method resolve : 'a t opt -> deferred t meth
  method resolveWith : 'a t -> 'a t opt -> deferred t meth
  method state : js_string t meth
  method _then : (unit -> bool t) callback -> (unit -> bool t) callback opt
    -> (unit -> bool t) callback opt -> promise t meth
end

(* options for bootstrap *)
class type pop_options = object
   method animation : bool t prop
   method container : js_string t prop
   method content : js_string t prop
   method delay_val : int prop
   method html : bool t prop
   method placement : js_string t prop
   method selector : js_string t prop
   method template : js_string t prop
   method title : js_string t prop
   method trigger_val : js_string t prop
   method offset_val : int prop
   method fallbackPlacement : js_string t prop
   method boundary : js_string t prop
 end

class type toast_options = object
  method animation : bool t prop
  method autohide : bool t prop
  method delay : int t prop
end

class type modal_options = object
  method backdrop : js_string t prop
  method keyboard : bool t prop
  method show : bool t prop
end

(* jquery main object *)

class type jquery = object
  method add_str : js_string t -> jquery t meth
  method add_elt : element t -> jquery t meth
  method add_arr : element t js_array t -> jquery t meth
  method add_jq : jquery t -> jquery t meth
  method addBack : js_string t -> jquery t meth
  method addClass_str : js_string t -> jquery t meth
  method addClass_fun : (int -> js_string t -> js_string t) callback -> jquery t meth
  method after_str : js_string t -> jquery t meth
  method after_elt : element t -> jquery t meth
  method after_arr : element t js_array t -> jquery t meth
  method after_jq : jquery t -> jquery t meth
  method after_fun1 : (int -> 'a t) callback -> jquery t meth
  method after_fun2 : (int -> js_string t -> 'a t) callback -> jquery t meth
  method ajaxComplete : (event_jq t -> XmlHttpRequest.xmlHttpRequest t -> json t -> bool t)
      callback -> jquery t meth
  method ajaxError : (event_jq t -> XmlHttpRequest.xmlHttpRequest t -> json t ->
                      js_string t -> bool t) callback -> jquery t meth
  method ajaxSend : (event_jq t -> XmlHttpRequest.xmlHttpRequest t -> json t -> bool t)
      callback -> jquery t meth
  method ajaxStart : (unit -> bool t) callback -> jquery t meth
  method ajaxStop : (unit -> bool t) callback -> jquery t meth
  method ajaxSuccess : (event_jq t -> XmlHttpRequest.xmlHttpRequest t ->
                        json -> json -> bool t) callback -> jquery t meth
  method andSelf : jquery t meth
  method animate : cssStyleDeclaration t -> int opt -> js_string t opt -> unit callback opt -> jquery t meth
  method animate_json : cssStyleDeclaration t -> options t -> jquery t meth
  method append_str : js_string t -> jquery t meth
  method append_elt : element t -> jquery t meth
  method append_jq : jquery t -> jquery t meth
  method append_fun : (int -> js_string t -> 'a t) callback -> jquery t meth
  method appendTo_str : js_string t -> jquery t meth
  method appendTo_elt : element t -> jquery t meth
  method appendTo_arr : element t js_array t -> jquery t meth
  method appendTo_jq : jquery t -> jquery t meth
  method attr_get : js_string t -> js_string t meth
  method attr_setStr : js_string t -> js_string t -> jquery t meth
  method attr_setInt : js_string t -> int -> jquery t meth
  method attr_setFloat : js_string t -> float t -> jquery t meth
  method before_str : js_string t -> jquery t meth
  method before_elt : element t -> jquery t meth
  method before_arr : element t js_array t -> jquery t meth
  method before_jq : jquery t -> jquery t meth
  method before_fun1 : (int -> 'a t) callback -> jquery t meth
  method before_fun2 : (int -> js_string t -> 'a t) callback -> jquery t meth
  method bind_fun : js_string t -> 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method bind_b : js_string t -> 'a t opt -> bool opt -> jquery t meth
  method blur_fun : (event_jq t -> bool t) callback -> jquery t meth
  method blur_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method blur : jquery t meth
  method change_fun : (event_jq t -> bool t) callback -> jquery t meth
  method change_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method change : jquery t meth
  method children : js_string t opt -> jquery t meth
  method clearQueue : js_string t opt -> jquery t meth
  method click_fun : (event_jq t -> bool t) callback -> jquery t meth
  method click_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method click : jquery t meth
  method clone : bool opt -> bool opt -> jquery t meth
  method closest_str : js_string t -> jquery t meth
  method closest_elt : element t -> jquery t meth
  method closest_jq : jquery t -> jquery t meth
  method contents : jquery t meth
  method context : element t prop
  method contextmenu : (event_jq t -> bool t) callback -> jquery t meth
  method css_set : js_string t -> js_string t -> jquery t meth
  method css_get : js_string t -> js_string t meth
  method data_set : js_string t -> 'a t -> jquery t meth
  method data_set0 : 'a t -> jquery t meth
  method data_get : js_string t -> 'a t meth
  method data_get0 : 'a t meth
  method dblclick : jquery t meth
  method dblclick_fun : (event_jq t -> bool t) callback -> jquery t meth
  method dblclick_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method delay : int -> js_string t opt -> jquery t meth
  method delegate : js_string t -> js_string t -> (event_jq t -> bool t) callback -> jquery t meth
  method delegate_data : js_string t -> js_string t -> 'a t ->
    (event_jq t -> bool t) callback -> jquery t meth
  method dequeue : js_string t opt -> jquery t meth
  method detach : js_string t opt -> jquery t meth
  method die : jquery t meth
  method die_type : js_string t -> js_string t opt -> jquery t meth
  method each : ( int -> element t -> bool t) callback -> jquery t meth
  method empty : jquery t meth
  method _end : jquery t meth
  method eq : int -> jquery t meth
  method error : (event_jq t -> bool t) callback -> jquery t meth
  method error_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method fadeIn : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method fadeIn_json : options t -> jquery t meth
  method fadeOut : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method fadeOut_json : json t -> jquery t meth
  method fadeTo : int -> int -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method fadeToggle : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method fadeToggle_json : json t -> jquery t meth
  method filter_str : js_string t -> jquery t meth
  method filter_fun : (int -> element t -> bool t) callback -> jquery t meth
  method filter_elt : element t -> jquery t meth
  method filter_jq : jquery t -> jquery t meth
  method find_str : js_string t -> jquery t meth
  method find_elt : element t -> jquery t meth
  method find_jq : jquery t -> jquery t meth
  method finish : js_string t opt -> jquery t meth
  method first : jquery t meth
  method focus_fun : (event_jq t -> bool t) callback -> jquery t meth
  method focus_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method focus : jquery t meth
  method focusin_fun : (event_jq t -> bool t) callback -> jquery t meth
  method focusin_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method focusin : jquery t meth
  method focusout_fun : (event_jq t -> bool t) callback -> jquery t meth
  method focusout_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method focusout : jquery t meth
  method get_i : int -> element t meth
  method get : element t js_array t meth
  method has_str : js_string t -> jquery t meth
  method has_elt : element t -> jquery t meth
  method hasClass : js_string t -> bool t meth
  method height_get : int meth
  method height_str : js_string t -> jquery t meth
  method height_int : int -> jquery t meth
  method height_fun : (int -> int -> int) callback -> jquery t meth
  method hide : jquery t meth
  method hide_opt : int -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method hide_json : options t -> jquery t meth
  method hover_2 : (event_jq t -> bool t) callback ->
    (event_jq t -> bool t) callback -> jquery t meth
  method hover : (event_jq t -> bool t) callback -> jquery t meth
  method html_get : js_string t meth
  method html_str : js_string t -> jquery t meth
  method html_fun : (int -> js_string t -> js_string t) callback  -> jquery t meth
  method index : int meth
  method index_str : js_string t -> jquery t meth
  method index_elt : element t -> jquery t meth
  method innerHeight : int meth
  method innerHeight_int : int -> jquery t meth
  method innerHeight_fun : (int -> int -> int) callback -> jquery t meth
  method innerWidth : int meth
  method innerWidth_int : int -> jquery t meth
  method innerWidth_fun : (int -> int -> int) callback -> jquery t meth
  method insertAfter_str : js_string t -> jquery t meth
  method insertAfter_elt : element t -> jquery t meth
  method insertAfter_arr : element t js_array t -> jquery t meth
  method insertAfter_jq : jquery t -> jquery t meth
  method insertBefore_str : js_string t -> jquery t meth
  method insertBefore_elt : element t -> jquery t meth
  method insertBefore_arr : element t js_array t -> jquery t meth
  method insertBefore_jq : jquery t -> jquery t meth
  method is_str : js_string t -> bool t meth
  method is_fun : (int -> element t -> bool t) callback -> bool t meth
  method is_jq : jquery t -> bool t meth
  method is_elt : element t -> bool t meth
  method jquery : js_string t prop
  method keydown_fun : (event_jq t -> bool t) callback -> jquery t meth
  method keydown_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method keydown : jquery t meth
  method keypress_fun : (event_jq t -> bool t) callback -> jquery t meth
  method keypress_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method keypress : jquery t meth
  method keyup_fun : (event_jq t -> bool t) callback -> jquery t meth
  method keyup_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method keyup : jquery t meth
  method last : jquery t meth
  method length : int prop
  method live : js_string t -> (event_jq t -> bool t) callback -> jquery t meth
  method live_json : js_string t -> json t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method load_str : js_string t -> js_string t opt ->
    (js_string t -> js_string t -> XmlHttpRequest.xmlHttpRequest -> bool t) callback opt
    -> jquery t meth
  method load_fun : (event_jq t-> bool t) callback -> jquery t meth
  method load_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method map : (int -> element t -> 'a t) callback -> jquery t meth
  method mousedown : jquery t meth
  method mousedown_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mousedown_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method mouseenter : jquery t meth
  method mouseenter_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mouseenter_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method mouseleave : jquery t meth
  method mouseleave_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mouseleave_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method mousemove : jquery t meth
  method mousemove_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mousemove_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method mouseout : jquery t meth
  method mouseout_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mouseout_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method mouseover : jquery t meth
  method mouseover_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mouseover_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method mouseup : jquery t meth
  method mouseup_fun : (event_jq t -> bool t) callback -> jquery t meth
  method mouseup_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method next : js_string t opt -> jquery t meth
  method nextAll : js_string t opt -> jquery t meth
  method nextUntil_str : js_string t opt -> js_string t opt -> jquery t meth
  method nextUntil_elt : element t opt -> js_string t opt -> jquery t meth
  method nextUntil_jq : jquery opt -> js_string t opt -> jquery t meth
  method not_str : js_string t -> jquery t meth
  method not_elt : element t -> jquery t meth
  method not_arr : element t js_array t -> jquery t meth
  method not_jq : jquery t meth -> jquery t meth
  method off_str : js_string t -> js_string t opt -> (event_jq t -> bool t) callback opt
    -> jquery t meth
  method off_ev : event_jq t -> jquery t meth
  method off : jquery t meth
  method offset : 'a t meth
  method offset_json : json t -> jquery t meth
  method offset_fun : (int -> json t -> json t) callback -> jquery t meth
  method offsetParent : jquery t meth
  method on : js_string t -> js_string t opt -> 'a t opt -> jquery t meth
  method on_fun : js_string t -> js_string t opt -> 'a t opt
    -> (event_jq t -> bool t) callback -> jquery t meth
  method one : js_string t -> js_string t opt -> 'a t opt -> jquery t meth
  method one_fun : js_string t -> js_string t opt -> 'a t opt
    -> (event_jq t -> bool t) callback -> jquery t meth
  method outerHeight : bool t opt -> int meth
  method outerHeight_int : int -> jquery t meth
  method outerHeight_fun : (int -> int -> int) callback -> jquery t meth
  method outerWidth : bool t opt -> int meth
  method outerWidth_int : int -> jquery t meth
  method outerWidth_fun : (int -> int -> int) callback -> jquery t meth
  method parent : js_string t opt -> jquery t meth
  method parents : js_string t opt -> jquery t meth
  method parentsUntil_str : js_string t opt -> js_string t opt -> jquery t meth
  method parentsUntil_elt : element t opt -> js_string t opt -> jquery t meth
  method parentsUntil_jq : jquery opt -> js_string t opt -> jquery t meth
  method position : position t meth
  method prepend_str : js_string t -> jquery t meth
  method prepend_elt : element t -> jquery t meth
  method prepend_arr : element t js_array t -> jquery t meth
  method prepend_jq : jquery t -> jquery t meth
  method prepend_fun : (int -> js_string t -> js_string t) callback -> jquery t meth
  method prependTo_str : js_string t -> jquery t meth
  method prependTo_elt : element t -> jquery t meth
  method prependTo_arr : element t js_array t -> jquery t meth
  method prependTo_jq : jquery t -> jquery t meth
  method prev : js_string t opt -> jquery t meth
  method prevAll : js_string t opt -> jquery t meth
  method prevUntil_str : js_string t opt -> js_string t opt -> jquery t meth
  method prevUntil_elt : element t opt -> js_string t opt -> jquery t meth
  method prevUntil_jq : jquery t opt -> js_string t opt -> jquery t meth
  method promise : js_string t opt -> json t opt -> promise t meth
  method prop : js_string t -> 'a t meth
  method prop_str : js_string t -> js_string t -> jquery t meth
  method prop_json : json t -> jquery t meth
  method prop_fun : js_string t -> (int -> 'a t -> 'a t) callback -> jquery t meth
  method pushStack : element t js_array t -> jquery t meth
  method pushStack_str : element t js_array t -> js_string t -> js_string t js_array t -> jquery t meth
  method queue : js_string t opt -> 'a t js_array t meth
  method queue_arr : js_string t opt -> 'a t js_array t -> jquery t meth
  method queue_fun : js_string t opt -> ('a t callback -> bool t) callback -> jquery t meth
  method ready : (unit -> bool t) callback -> jquery t meth
  method remove : js_string t opt -> jquery t meth
  method removeAttr : js_string t -> jquery t meth
  method removeClass_str : js_string t opt -> jquery t meth
  method removeClass_fun : (int -> js_string t -> js_string t) callback -> jquery t meth
  method removeData : js_string t opt -> jquery t meth
  method removeData_arr : js_string t js_array t opt -> jquery t meth
  method removeProp : js_string t -> jquery t meth
  method replaceAll_str : js_string t -> jquery t meth
  method replaceAll_jq : jquery t -> jquery t meth
  method replaceAll_elt : element t -> jquery t meth
  method replaceAll_arr : element t js_array t -> jquery t meth
  method replaceWith_str : js_string t -> jquery t meth
  method replaceWith_jq : jquery t -> jquery t meth
  method replaceWith_elt : element t -> jquery t meth
  method replaceWith_arr : element t js_array t -> jquery t meth
  method replaceWith_fun : (unit -> element t) callback -> jquery t meth
  method resize_fun : (event_jq t -> bool t) callback -> jquery t meth
  method resize_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method resize : jquery t meth
  method scroll_fun : (event_jq t -> bool t) callback -> jquery t meth
  method scroll_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method scroll : jquery t meth
  method scrollLeft_get : int meth
  method scrollLeft_set : int -> jquery t meth
  method scrollTop_get : int meth
  method scrollTop_set : int -> jquery t meth
  method select_fun : (event_jq t -> bool t) callback -> jquery t meth
  method select_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method select : jquery t meth
  method selector : js_string t prop
  method serialize : js_string t meth
  method serializeArray : json t js_array t meth
  method show : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method show_json : options t -> jquery t meth
  method siblings : js_string t opt -> jquery t meth
  method size : int meth
  method slice : int -> int opt -> jquery t meth
  method slideDown : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method slideDown_json : options t -> jquery t meth
  method slideToggle : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method slideToggle_json : json t -> jquery t meth
  method slideUp : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method slideUp_json : options t -> jquery t meth
  method stop : bool t opt -> bool t opt -> jquery t meth
  method stop_str : js_string t opt -> bool t opt -> bool t opt -> jquery t meth
  method submit_fun : (event_jq t -> bool t) callback -> jquery t meth
  method submit_opt : 'a t opt -> (event_jq t -> bool t) callback -> jquery t meth
  method submit : jquery t meth
  method text_get : js_string t meth
  method text_str : js_string t -> jquery t meth
  method text_bool : bool t -> jquery t meth
  method text_int : int -> jquery t meth
  method text_fun : (int -> js_string t -> js_string t) callback -> jquery t meth
  method toArray : 'a t js_array t meth
  method toggle : int opt -> js_string t opt -> (unit -> bool t) callback opt -> jquery t meth
  method toggle_json : options t -> jquery t meth
  method toggle_bool : bool t -> jquery t meth
  method toggle_fun : (event_jq t -> bool t) callback -> (event_jq t -> bool t) callback
    -> (event_jq t -> bool t) callback opt -> jquery t meth
  method toggleClass : js_string t -> jquery t meth
  method toggleClass_state : js_string t -> bool t -> jquery t meth
  method toggleClass_opt : bool t opt -> jquery t meth
  method toggleClass_fun : (int -> js_string t -> bool t -> js_string t) callback
    -> bool t opt -> jquery t meth
  method trigger : js_string t -> json t opt -> jquery t meth
  method trigger_ev : event_jq t -> json t opt -> jquery t meth
  method triggerHandler : js_string t -> json t opt -> jquery t meth
  method triggerHandler_ev : event_jq t -> json t opt -> jquery t meth
  method unbind_fun : js_string t -> (event_jq t -> bool t) callback opt -> jquery t meth
  method unbind_str : js_string t -> bool t -> jquery t meth
  method unbind_ev : event_jq t -> jquery t meth
  method unbind : jquery t meth
  method undelegate : jquery t meth
  method undelegate_str : js_string t -> js_string t -> jquery t meth
  method undelegate_fun : js_string t -> js_string t -> (event_jq t -> bool t) callback -> jquery t meth
  method undelegate_json : js_string t -> json t -> jquery t meth
  method undelegate_nm : js_string t -> jquery t meth
  method unload : (event_jq -> bool t) callback -> jquery t meth
  method unload_opt : 'a t opt -> (event_jq -> bool t) callback -> jquery t meth
  method unwrap : jquery t meth
  method unwrap_opt : js_string t opt -> jquery t meth
  method val_str : js_string t meth
  method val_int : int meth
  method val_arr : int js_array t meth
  method val_setStr : js_string t -> jquery t meth
  method val_setInt : int -> jquery t meth
  method val_setArr : js_string t js_array t -> jquery t meth
  method val_setFun : (int -> js_string t -> js_string t) callback -> jquery t meth
  method width : int meth
  method width_set : int -> jquery t meth
  method width_fun : (int -> int -> int) callback -> jquery t meth
  method wrap_str : js_string t -> jquery t meth
  method wrap_elt : element t -> jquery t meth
  method wrap_jq : jquery t -> jquery t meth
  method wrap_fun : (int -> js_string t) callback -> jquery t meth
  method wrapAll_str : js_string t -> jquery t meth
  method wrapAll_elt : element t -> jquery t meth
  method wrapAll_jq : jquery t -> jquery t meth
  method wrapAll_fun : (unit -> js_string t) callback -> jquery t meth
  method wrapInner_str : js_string t -> jquery t meth
  method wrapInner_elt : element t -> jquery t meth
  method wrapInner_jq : jquery t -> jquery t meth
  method wrapInner_fun : (int -> js_string t) callback -> jquery t meth

  (* for bootstrap only *)
  method popover_opt : pop_options t opt -> jquery t meth
  method popover : js_string t -> jquery t meth
  method tooltip_opt : pop_options t opt -> jquery t meth
  method tooltip : js_string t -> jquery t meth
  method toast_opt : toast_options t opt -> jquery t meth
  method toast : js_string t -> jquery t meth
  method modal_opt : modal_options t opt -> jquery t meth
  method modal : js_string t -> jquery t meth
end

let jQ s : jquery t = Unsafe.fun_call
    (Unsafe.variable "$") [|Unsafe.inject (string s)|]

let tropt f = function
  | None -> Opt.empty
  | Some x -> Opt.return (f x)

let wrap_handler0 handler = wrap_callback (fun () -> bool (handler ()))
let wrap_handler1 handler = wrap_callback (fun e -> bool (handler e))
let wrap_handler3 handler = wrap_callback (fun e a b -> bool (handler e a b))
let wrap_handler4 handler = wrap_callback (fun e a b c -> bool (handler e a b c))

let add_str s (o:jquery t) = o##(add_str (string s))
let add_elt e (o:jquery t) = o##(add_elt e)
let add_jq j (o:jquery t) = o##(add_jq j)
let add_back s (o:jquery t) = o##(addBack (string s))
let add_class_str s (o:jquery t) = o##(addClass_str (string s))
let add_class_fun f (o:jquery t) =
  o##(addClass_fun (wrap_callback (fun i s -> string @@ f i (to_string s))))
let after_str s (o:jquery t) = o##(after_str (string s))
let after_elt e (o:jquery t) = o##(after_elt e)
let after_jq j (o:jquery t) = o##(after_jq j)
let after_fun1 f (o:jquery t) = o##(after_fun1 (wrap_callback f))
let after_fun2 f (o:jquery t) = o##(after_fun2
    (wrap_callback (fun i s -> string @@ f i (to_string s))))
let ajax_complete handler (o:jquery t) = o##(ajaxComplete (wrap_handler3 handler))
let ajax_error handler (o:jquery t) =
  o##(ajaxError (wrap_callback (fun e xhr json s ->
      bool @@ handler e xhr json (to_string s))))
let ajax_send handler (o:jquery t) = o##(ajaxSend (wrap_handler3 handler))
let ajax_start handler (o:jquery t) = o##(ajaxStart (wrap_handler0 handler))
let ajax_stop handler (o:jquery t) = o##(ajaxStop (wrap_handler0 handler))
let ajax_success handler (o:jquery t) = o##(ajaxSuccess (wrap_handler4 handler))
let and_self (o:jquery t) = o##andSelf
let animate ?duration ?easing ?complete css (o:jquery t) =
  o##(animate css (Opt.option duration) (tropt string easing) (Opt.option complete))
let animate_json css options (o:jquery t) = o##(animate_json css options)
let append_str s (o:jquery t) = o##(append_str (string s))
let append_elt e (o:jquery t) = o##(append_elt e)
let append_jq j (o:jquery t) = o##(append_jq j)
let append_fun f (o:jquery t) =
  o##(append_fun (wrap_callback (fun i s -> string @@ f i (to_string s))))
let append_to_str s (o:jquery t) = o##(appendTo_str (string s))
let append_to_elt e (o:jquery t) = o##(appendTo_elt e)
let append_to_arr a (o:jquery t) = o##(appendTo_arr (array a))
let append_to_jq j (o:jquery t) = o##(appendTo_jq j)
let attr_get k (o:jquery t) = to_string o##(attr_get (string k))
let attr_set_str k v (o:jquery t) = o##(attr_setStr (string k) (string v))
let attr_set_int k v (o:jquery t) = o##(attr_setInt (string k) v)
let attr_set_float k v (o:jquery t) = o##(attr_setFloat (string k) v)
let before_str s (o:jquery t) = o##(before_str (string s))
let before_elt el (o:jquery t) = o##(before_elt el)
let before_jq j (o:jquery t) = o##(before_jq j)
let before_fun1 f (o:jquery t) = o##(before_fun1 (wrap_callback f))
let before_fun2 f (o:jquery t) = o##(before_fun2
    (wrap_callback (fun i s -> string @@ f i (to_string s))))
let bind_fun ?data typ handler (o:jquery t) =
  o##(bind_fun typ (Opt.option data) (wrap_handler1 handler))
let bind_b ?prevent_bubble ?data typ (o:jquery t) =
  o##(bind_b typ (Opt.option data) (Opt.option prevent_bubble))
let blur_fun handler (o:jquery t) = o##(blur_fun (wrap_handler1 handler))
let blur_opt ?data handler (o:jquery t) =
  o##(blur_opt (Opt.option data) (wrap_handler1 handler))
let blur (o:jquery t) = o##blur
let change_fun handler (o:jquery t) = o##(change_fun (wrap_handler1 handler))
let change_opt ?data handler (o:jquery t) =
  o##(change_opt (Opt.option data) (wrap_handler1 handler))
let change (o:jquery t) = o##change
let children ?selector (o:jquery t) = o##(children (tropt string selector))
let clear_queue ?queue (o:jquery t) = o##(children (tropt string queue))
let click_fun handler (o:jquery t) = o##(click_fun (wrap_handler1 handler))
let click_opt ?data handler (o:jquery t) =
  o##(click_opt (Opt.option data) (wrap_handler1 handler))
let click (o:jquery t) = o##click
let clone ?wdata ?deep (o:jquery t) = o##(clone (Opt.option wdata) (Opt.option deep))
let closest_str s (o:jquery t) = o##(closest_str (string s))
let closest_elt e (o:jquery t) = o##(closest_elt e)
let closest_jq j (o:jquery t) = o##(closest_jq j)
let contents (o:jquery t) = o##contents
let context (o:jquery t) = o##.context
let contextmenu handler (o:jquery t) = o##(contextmenu (wrap_handler1 handler))
let css_set k v (o:jquery t) = o##(css_set (string k) (string v))
let css_get k (o:jquery t) = o##(css_get (string k))
let data_set k data (o:jquery t) = o##(data_set (string k) data)
let data_set0 data (o:jquery t) = o##(data_set0 data)
let data_get k (o:jquery t) = o##(data_get (string k))
let data_get0 (o:jquery t) = o##data_get0
let dblclick (o:jquery t) = o##dblclick
let dblclick_fun handler (o:jquery t) = o##(dblclick_fun (wrap_handler1 handler))
let dblclick_opt ?data handler (o:jquery t) =
  o##(dblclick_opt (Opt.option data) (wrap_handler1 handler))
let delay ?queue d (o:jquery t) = o##(delay d (tropt string queue))
let delegate selector typ handler (o:jquery t) =
  o##(delegate (string selector) (string typ) (wrap_handler1 handler))
let delegate_data selector typ data handler (o:jquery t) =
  o##(delegate_data (string selector) (string typ) data (wrap_handler1 handler))
let dequeue ?queue (o:jquery t) = o##(dequeue (tropt string queue))
let detach ?selector (o:jquery t) = o##(detach (tropt string selector))
let die (o:jquery t) = o##die
let die_type ?handler typ (o:jquery t) =
  o##(die_type (string typ) (tropt string handler))
let each f (o:jquery t) = o##(each (wrap_callback f))
let empty (o:jquery t) = o##empty
let end0 (o:jquery t) = o##_end
let eq i (o:jquery t) = o##(eq i)
let error f (o:jquery t) = o##(error (wrap_callback f))
let error_opt ?data f (o:jquery t) = o##(error_opt (Opt.option data) (wrap_callback f))
let fade_in ?duration ?easing ?complete (o:jquery t) =
  o##(fadeIn (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let fade_in_json options (o:jquery t) = o##(fadeIn_json options)
let fade_out ?duration ?easing ?complete (o:jquery t) =
  o##(fadeOut (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let fade_out_json options (o:jquery t) = o##(fadeOut_json options)
let fade_to ?easing ?complete duration opacity (o:jquery t) =
  o##(fadeTo duration opacity (tropt string easing) (tropt wrap_callback complete))
let fade_toggle ?duration ?easing ?complete (o:jquery t) =
  o##(fadeToggle (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let fade_toggle_json options (o:jquery t) = o##(fadeToggle_json options)
let filter_str selector (o:jquery t) = o##(filter_str (string selector))
let filter_fun f (o:jquery t) =
  o##(filter_fun (wrap_callback (fun i e -> bool (f i e))))
let filter_elt e (o:jquery t) = o##(filter_elt e)
let filter_jq j (o:jquery t) = o##(filter_jq j)
let find_str selector (o:jquery t) = o##(find_str (string selector))
let find_elt e (o:jquery t) = o##(find_elt e)
let find_jq j (o:jquery t) = o##(find_jq j)
let finish ?queue (o:jquery t) = o##(finish (tropt string queue))
let first (o:jquery t) = o##first
let focus_fun handler (o:jquery t) = o##(focus_fun (wrap_handler1 handler))
let focus_opt ?data handler (o:jquery t) =
  o##(focus_opt (Opt.option data) (wrap_handler1 handler))
let focus (o:jquery t) = o##focus
let focusin_fun handler (o:jquery t) = o##(focusin_fun (wrap_handler1 handler))
let focusin_opt ?data handler (o:jquery t) =
  o##(focusin_opt (Opt.option data) (wrap_handler1 handler))
let focusin (o:jquery t) = o##focusin
let focusout_fun handler (o:jquery t) = o##(focusout_fun (wrap_handler1 handler))
let focusout_opt ?data handler (o:jquery t) =
  o##(focusout_opt (Opt.option data) (wrap_handler1 handler))
let focusout (o:jquery t) = o##focusout
let get_i i (o:jquery t) = o##(get_i i)
let get (o:jquery t) = to_array o##get
let has_str selector (o:jquery t) = o##(has_str (string selector))
let has_elt e (o:jquery t) = o##(has_elt e)
let has_class name (o:jquery t) = o##(hasClass (string name))
let height_get (o:jquery t) = o##height_get
let height_str v (o:jquery t) = o##(height_str (string v))
let height_int i (o:jquery t) = o##(height_int i)
let height_fun f (o:jquery t) = o##(height_fun (wrap_callback f))
let hide (o:jquery t) = o##hide
let hide_opt ?easing ?complete duration (o:jquery t) =
  o##(hide_opt duration (tropt string easing)  (tropt wrap_callback complete))
let hide_json options (o:jquery t) = o##(hide_json options)
let hover2 handler_in handler_out (o:jquery t) =
  o##(hover_2 (wrap_handler1 handler_in) (wrap_handler1 handler_out))
let hover handler (o:jquery t) = o##(hover (wrap_handler1 handler))
let html_get (o:jquery t) = o##html_get
let html_str s (o:jquery t) = o##(html_str (string s))
let html_fun f (o:jquery t) =
  o##(html_fun (wrap_callback (fun i s -> string (f i (to_string s)))))
let index (o:jquery t) = o##index
let index_str selector (o:jquery t) = o##(index_str (string selector))
let index_elt e (o:jquery t) = o##(index_elt e)
let inner_height (o:jquery t) = o##innerHeight
let inner_height_int i (o:jquery t) = o##(innerHeight_int i)
let inner_height_fun f (o:jquery t) = o##(innerHeight_fun (wrap_callback f))
let inner_width (o:jquery t) = o##innerWidth
let innerWidth_int i (o:jquery t) = o##(innerWidth_int i)
let innerWidth_fun f (o:jquery t) = o##(innerWidth_fun (wrap_callback f))
let insert_after_str s (o:jquery t) = o##(insertAfter_str (string s))
let insert_after_elt e (o:jquery t) = o##(insertAfter_elt e)
let insert_after_arr a (o:jquery t) = o##(insertAfter_arr (array a))
let insert_after_jq j (o:jquery t) = o##(insertAfter_jq j)
let insert_before_str s (o:jquery t) = o##(insertBefore_str (string s))
let insert_before_elt e (o:jquery t) = o##(insertBefore_elt e)
let insert_before_arr a (o:jquery t) = o##(insertBefore_arr (array a))
let insert_before_jq j (o:jquery t) = o##(insertBefore_jq j)
let is_str selector (o:jquery t) = o##(is_str (string selector))
let is_fun f (o:jquery t) = o##(is_fun (wrap_callback f))
let is_jq j (o:jquery t) = o##(is_jq j)
let is_elt e (o:jquery t) = o##(is_elt e)
let jquery (o:jquery t) = o##.jquery
let keydown_fun handler (o:jquery t) = o##(keydown_fun (wrap_callback handler))
let keydown_opt ?data handler (o:jquery t) =
  o##(keydown_opt (Opt.option data) (wrap_handler1 handler))
let keydown (o:jquery t) = o##keydown
let keypress_fun handler (o:jquery t) = o##(keypress_fun (wrap_handler1 handler))
let keypress_opt ?data handler (o:jquery t) =
  o##(keypress_opt (Opt.option data) (wrap_handler1 handler))
let keypress (o:jquery t) = o##keypress
let keyup_fun handler (o:jquery t) = o##(keyup_fun (wrap_handler1 handler))
let keyup_opt ?data handler (o:jquery t) =
  o##(keyup_opt (Opt.option data) (wrap_handler1 handler))
let keyup (o:jquery t) = o##keyup
let last (o:jquery t) = o##last
let length (o:jquery t) = o##.length
let live events handler (o:jquery t) = o##(live (string events) (wrap_handler1 handler))
let live_json ?data events handler (o:jquery t) =
  o##(live_json (string events) (Opt.option data) (wrap_handler1 handler))
let load_str ?data ?complete url (o:jquery t) =
  o##(load_str (string url) (tropt string data)
              (tropt (fun f -> wrap_callback
                        (fun r s xhr -> f (to_string r) (to_string s) xhr)) complete))
let load_fun handler (o:jquery t) = o##(load_fun (wrap_callback handler))
let load_opt ?data handler (o:jquery t) =
  o##(load_opt (Opt.option data) (wrap_handler1 handler))
let map callback (o:jquery t) = o##(map (wrap_callback callback))
let mousedown (o:jquery t) = o##mousedown
let mousedown_fun handler (o:jquery t) = o##(mousedown_fun (wrap_handler1 handler))
let mousedown_opt ?data handler (o:jquery t) =
  o##(mousedown_opt (Opt.option data) (wrap_handler1 handler))
let mouseenter (o:jquery t) = o##mouseenter
let mouseenter_fun handler (o:jquery t) = o##(mouseenter_fun (wrap_handler1 handler))
let mouseenter_opt ?data handler (o:jquery t) =
  o##(mouseenter_opt (Opt.option data) (wrap_handler1 handler))
let mouseleave (o:jquery t) = o##mouseleave
let mouseleave_fun handler (o:jquery t) = o##(mouseleave_fun (wrap_handler1 handler))
let mouseleave_opt ?data handler (o:jquery t) =
  o##(mouseleave_opt (Opt.option data) (wrap_handler1 handler))
let mousemove (o:jquery t) = o##mousemove
let mousemove_fun handler (o:jquery t) = o##(mousemove_fun (wrap_handler1 handler))
let mousemove_opt ?data handler (o:jquery t) =
  o##(mousemove_opt (Opt.option data) (wrap_handler1 handler))
let mouseout (o:jquery t) = o##mouseout
let mouseout_fun handler (o:jquery t) = o##(mouseout_fun (wrap_handler1 handler))
let mouseout_opt ?data handler (o:jquery t) =
  o##(mouseout_opt (Opt.option data) (wrap_handler1 handler))
let mouseover (o:jquery t) = o##mouseover
let mouseover_fun handler (o:jquery t) = o##(mouseover_fun (wrap_handler1 handler))
let mouseover_opt ?data handler (o:jquery t) =
  o##(mouseover_opt (Opt.option data) (wrap_handler1 handler))
let mouseup (o:jquery t) = o##mouseup
let mouseup_fun handler (o:jquery t) = o##(mouseup_fun (wrap_handler1 handler))
let mouseup_opt ?data handler (o:jquery t) =
  o##(mouseup_opt (Opt.option data) (wrap_handler1 handler))
let next ?selector (o:jquery t) = o##(next (tropt string selector))
let next_all ?selector (o:jquery t) = o##(nextAll (tropt string selector))
let next_until_str ?selector ?filter (o:jquery t) =
  o##(nextUntil_str (tropt string selector) (tropt string filter))
let next_until_elt ?element ?filter (o:jquery t) =
  o##(nextUntil_elt (Opt.option element) (tropt string filter))
let next_until_jq ?jquery ?filter (o:jquery t) =
  o##(nextUntil_jq (Opt.option jquery) (tropt string filter))
let not_str selector (o:jquery t) = o##(not_str (string selector))
let not_elt e (o:jquery t) = o##(not_elt e)
let not_arr a (o:jquery t) = o##(not_arr (array a))
let not_jq j (o:jquery t) = o##(not_jq j)
let off_str ?selector ?handler events (o:jquery t) =
  o##(off_str (string events) (tropt string selector) (tropt wrap_handler1 handler))
let off_ev event (o:jquery t) = o##(off_ev event)
let off (o:jquery t) = o##off
let offset (o:jquery t) = o##offset
let offset_json coordinates (o:jquery t) = o##(offset_json coordinates)
let offet_fun f (o:jquery t) = o##(offset_fun (wrap_callback f))
let offset_parent (o:jquery t) = o##offsetParent
let on ?selector ?data events (o:jquery t) =
  o##(on (string events) (tropt string selector) (Opt.option data))
let on_fun ?selector ?data events handler (o:jquery t) =
  o##(on_fun (string events) (tropt string selector) (Opt.option data) (wrap_handler1 handler))
let one ?selector ?data events (o:jquery t) =
  o##(one (string events) (tropt string selector) (Opt.option data))
let one_fun ?selector ?data events handler (o:jquery t) =
  o##(one_fun (string events) (tropt string selector) (Opt.option data) (wrap_handler1 handler))
let outer_height ?margin (o:jquery t) = o##(outerHeight (tropt bool margin))
let outer_height_int v (o:jquery t) = o##(outerHeight_int v)
let outer_height_fun f (o:jquery t) = o##(outerHeight_fun (wrap_callback f))
let outer_width ?margin (o:jquery t) = o##(outerWidth (tropt bool margin))
let outer_width_int v (o:jquery t) = o##(outerWidth_int v)
let outer_width_fun f (o:jquery t) = o##(outerWidth_fun (wrap_callback f))
let parent ?selector (o:jquery t) = o##(parent (tropt string selector))
let parents ?selector (o:jquery t) = o##(parents (tropt string selector))
let parents_until_str ?selector ?filter (o:jquery t) =
  o##(parentsUntil_str (tropt string selector) (tropt string filter))
let parents_until_elt ?element ?filter (o:jquery t) =
  o##(parentsUntil_elt (Opt.option element) (tropt string filter))
let parents_until_jq ?jquery ?filter (o:jquery t) =
  o##(parentsUntil_jq (Opt.option jquery) (tropt string filter))
let position (o:jquery t) = o##position
let prepend_str content (o:jquery t) = o##(prepend_str (string content))
let prepend_elt content (o:jquery t) = o##(prepend_elt content)
let prepend_jq content (o:jquery t) = o##(prepend_jq content)
let prepend_arr content (o:jquery t) = o##(prepend_arr (array content))
let prepend_fun f (o:jquery t) =
  o##(prepend_fun (wrap_callback (fun i s -> string @@ f i (to_string s))))
let prepend_to_str content (o:jquery t) = o##(prependTo_str (string content))
let prepend_to_elt content (o:jquery t) = o##(prependTo_elt content)
let prepend_to_jq content (o:jquery t) = o##(prependTo_jq content)
let prepend_to_arr content (o:jquery t) = o##(prependTo_arr (array content))
let prev ?selector (o:jquery t) = o##(prev (tropt string selector))
let prev_all ?selector (o:jquery t) = o##(prevAll (tropt string selector))
let prev_until_str ?selector ?filter (o:jquery t) =
  o##(prevUntil_str (tropt string selector) (tropt string filter))
let prev_until_elt ?element ?filter (o:jquery t) =
  o##(prevUntil_elt (Opt.option element) (tropt string filter))
let prev_until_jq ?jquery ?filter (o:jquery t) =
  o##(prevUntil_jq (Opt.option jquery) (tropt string filter))
let promise ?typ ?target (o:jquery t) =
  o##(promise (tropt string typ) (Opt.option target))
let prop name (o:jquery t) = o##(prop (string name))
let prop_str name value (o:jquery t) = o##(prop_str (string name) (string value))
let prop_json properties (o:jquery t) = o##(prop_json properties)
let prop_fun name f (o:jquery t) = o##(prop_fun (string name) (wrap_callback f))
let push_stack a (o:jquery t) = o##(pushStack (array a))
let push_stack_str a name arguments (o:jquery t) =
  o##(pushStack_str (array a) (string name) (array (Array.map string arguments)))
let queue ?name (o:jquery t) = to_array @@ o##(queue (tropt string name))
let queue_arr ?name new_queue (o:jquery t) =
  o##(queue_arr (tropt string name) (array new_queue))
let queue_fun ?name callback (o:jquery t) =
  o##(queue_fun (tropt string name) (wrap_callback callback))
let ready f (o:jquery t) = o##(ready (wrap_callback f))
let remove ?selector (o:jquery t) = o##(remove (tropt string selector))
let remove_attr name (o:jquery t) = o##(removeAttr name)
let remove_class_str ?name (o:jquery t) = o##(removeClass_str (tropt string name))
let remove_class_fun f (o:jquery t) =
  o##(removeClass_fun (wrap_callback (fun i s -> string (f i (to_string s)))))
let remove_data ?name (o:jquery t) = o##(removeData (tropt string name))
let remove_data_arr ?list (o:jquery t) =
  o##(removeData_arr (tropt (fun a -> array (Array.map string a)) list))
let remove_prop name (o:jquery t) = o##(removeProp (string name))
let replace_all_str selector (o:jquery t) = o##(replaceAll_str (string selector))
let replace_all_jq j (o:jquery t) = o##(replaceAll_jq j)
let replace_all_elt e (o:jquery t) = o##(replaceAll_elt e)
let replace_all_arr a (o:jquery t) = o##(replaceAll_arr (array a))
let replace_with_str selector (o:jquery t) = o##(replaceWith_str (string selector))
let replace_with_jq j (o:jquery t) = o##(replaceWith_jq j)
let replace_with_elt e (o:jquery t) = o##(replaceWith_elt e)
let replace_with_arr a (o:jquery t) = o##(replaceWith_arr (array a))
let replace_with_fun f (o:jquery t) = o##(replaceWith_fun (wrap_callback f))
let resize_fun handler (o:jquery t) = o##(resize_fun (wrap_handler1 handler))
let resize_opt ?data handler (o:jquery t) =
  o##(resize_opt (Opt.option data) (wrap_handler1 handler))
let resize (o:jquery t) = o##resize
let scroll_fun handler (o:jquery t) = o##(scroll_fun (wrap_handler1 handler))
let scroll_opt ?data handler (o:jquery t) =
  o##(scroll_opt (Opt.option data) (wrap_handler1 handler))
let scroll (o:jquery t) = o##scroll
let scroll_left_get (o:jquery t) = o##scrollLeft_get
let scroll_left_set value (o:jquery t) = o##(scrollLeft_set value)
let scroll_top_get (o:jquery t) = o##scrollTop_get
let scroll_top_set value (o:jquery t) = o##(scrollTop_set value)
let select_fun handler (o:jquery t) = o##(select_fun (wrap_handler1 handler))
let select_opt ?data handler (o:jquery t) =
  o##(select_opt (Opt.option data) (wrap_handler1 handler))
let select (o:jquery t) = o##select
let selector (o:jquery t) = o##.selector
let serialize (o:jquery t) = o##serialize
let serialize_array (o:jquery t) = to_array (o##serializeArray)
let show ?duration ?easing ?complete (o:jquery t) =
  o##(show (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let show_json options (o:jquery t) = o##(show_json options)
let siblings ?selector (o:jquery t) = o##(siblings (tropt string selector))
let size (o:jquery t) = o##size
let slice ?endd start (o:jquery t) = o##(slice start (Opt.option endd))
let slide_down ?duration ?easing ?complete (o:jquery t) =
  o##(slideDown (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let slide_down_json options (o:jquery t) = o##(slideDown_json options)
let slide_toggle ?duration ?easing ?complete (o:jquery t) =
  o##(slideToggle (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let slide_toggle_json options (o:jquery t) = o##(slideToggle_json options)
let slide_up ?duration ?easing ?complete (o:jquery t) =
  o##(slideUp (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let slide_up_json options (o:jquery t) = o##(slideUp_json options)
let stop ?clear ?jump (o:jquery t) = o##(stop (tropt bool clear) (tropt bool jump))
let stop_str ?queue ?clear ?jump (o:jquery t) =
  o##(stop_str (tropt string queue) (tropt bool clear) (tropt bool jump))
let submit_fun handler (o:jquery t) = o##(submit_fun (wrap_handler1 handler))
let submit_opt ?data handler (o:jquery t) =
  o##(submit_opt (Opt.option data) (wrap_handler1 handler))
let submit (o:jquery t) = o##submit
let text_get (o:jquery t) = to_string (o##text_get)
let text_str s (o:jquery t) = o##(text_str (string s))
let text_bool b (o:jquery t) = o##(text_bool (bool b))
let text_int i (o:jquery t) = o##(text_int i)
let text_fun f (o:jquery t) = o##(text_fun (wrap_callback (fun i s -> string (f i (to_string s)))))
let to_array (o:jquery t) = to_array (o##toArray)
let toggle ?duration ?easing ?complete (o:jquery t) =
  o##(toggle (Opt.option duration) (tropt string easing) (tropt wrap_callback complete))
let toggle_json options (o:jquery t) = o##(toggle_json options)
let toggle_bool display (o:jquery t) = o##(toggle_bool (bool display))
let toggle_fun ?handler3 handler1 handler2 (o:jquery t) =
  o##(toggle_fun (wrap_handler1 handler1) (wrap_handler1 handler2) (tropt wrap_handler1 handler3))
let toggle_class name (o:jquery t) = o##(toggleClass (string name))
let toggle_class_state name state (o:jquery t) = o##(toggleClass_state (string name) (bool state))
let toggle_class_opt ?state (o:jquery t) = o##(toggleClass_opt (tropt bool state))
let toggle_class_fun ?state f (o:jquery t) =
  o##(toggleClass_fun (wrap_callback (fun i s b -> string (f i (to_string s) (to_bool b))))
                     (tropt bool state))
let trigger ?extra event_type (o:jquery t) =
  o##(trigger (string event_type) (Opt.option extra))
let trigger_ev ?extra ev (o:jquery t) = o##(trigger_ev ev (Opt.option extra))
let triggerHandler ?extra event_type (o:jquery t) =
  o##(triggerHandler (string event_type) (Opt.option extra))
let triggerHandler_ev ev ?extra (o:jquery t) =
  o##(triggerHandler_ev ev (Opt.option extra))
let unbind_fun ?handler event_type (o:jquery t) =
  o##(unbind_fun (string event_type) (tropt wrap_callback handler))
let unbind_str event_type b (o:jquery t) =
  o##(unbind_str (string event_type) (bool b))
let unbind_ev ev (o:jquery t) = o##(unbind_ev ev)
let unbind (o:jquery t) =  o##unbind
let undelegate (o:jquery t) = o##undelegate
let undelegate_str selector event_type (o:jquery t) =
  o##(undelegate_str (string selector) (string event_type))
let undelegate_fun selector event_type handler (o:jquery t) =
  o##(undelegate_fun (string selector) (string event_type) (wrap_handler1 handler))
let undelegate_json selector events (o:jquery t) =
  o##(undelegate_json (string selector) events)
let undelegate_nm namespace (o:jquery t) = o##(undelegate_nm (string namespace))
let unload handler (o:jquery t) = o##(unload (wrap_callback handler))
let unload_opt ?data handler (o:jquery t) =
  o##(unload_opt (Opt.option data) (wrap_handler1 handler))
let unwrap (o:jquery t) = o##unwrap
let unwrap_opt ?selector (o:jquery t) = o##(unwrap_opt (tropt string selector))
let val_str (o:jquery t) = to_string (o##val_str)
let val_int (o:jquery t) = o##val_int
let val_arr (o:jquery t) = Js.to_array (o##val_arr)
let val_set_str s (o:jquery t) = o##(val_setStr (string s))
let val_set_int i (o:jquery t) = o##(val_setInt i)
let val_set_arr a (o:jquery t) = o##(val_setArr (array (Array.map string a)))
let val_set_fun f (o:jquery t) =
  o##(val_setFun (wrap_callback (fun i s -> string (f i (to_string s)))))
let width (o:jquery t) = o##width
let width_set i (o:jquery t) = o##(width_set i)
let width_fun f (o:jquery t) = o##(width_fun (wrap_callback f))
let wrap_str s (o:jquery t) = o##(wrap_str (string s))
let wrap_elt e (o:jquery t) = o##(wrap_elt e)
let wrap_jq j (o:jquery t) = o##(wrap_jq j)
let wrap_fun f (o:jquery t) = o##(wrap_fun (wrap_callback (fun i -> string (f i))))
let wrap_all_str s (o:jquery t) = o##(wrapAll_str (string s))
let wrap_all_elt e (o:jquery t) = o##(wrapAll_elt e)
let wrap_all_jq j (o:jquery t) = o##(wrapAll_jq j)
let wrap_all_fun f (o:jquery t) = o##(wrapAll_fun (wrap_callback (fun i -> string (f i))))
let wrap_inner_str s (o:jquery t) = o##(wrapInner_str (string s))
let wrap_inner_elt e (o:jquery t) = o##(wrapInner_elt e)
let wrap_inner_jq j (o:jquery t) = o##(wrapInner_jq j)
let wrap_inner_fun f (o:jquery t) = o##(wrapInner_fun (wrap_callback (fun i -> string (f i))))

(* for boostrap only *)
let popover_opt ?options (o:jquery t) = o##(popover_opt (Opt.option options))
let popover s (o:jquery t) = o##(popover (string s))
let tooltip_opt ?options (o:jquery t) = o##(tooltip_opt (Opt.option options))
let tooltip s (o:jquery t) = o##(tooltip (string s))
let toast_opt ?options (o:jquery t) = o##(toast_opt (Opt.option options))
let toast s (o:jquery t) = o##(toast (string s))
let modal_opt ?options (o:jquery t ) = o##(modal_opt (Opt.option options))
let modal s (o:jquery t) = o##(modal (string s))

let jquery_call o function_name arguments =
  Js.Unsafe.meth_call o function_name arguments
