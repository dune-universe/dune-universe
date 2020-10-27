open Js_of_ocaml
open Js
open Ezjs_jquery

class type container = object
  method drag : bool t prop
  method drop : bool t prop
  method exclude : js_string t prop
  method group : js_string t prop
  method nested : bool t prop
  method vertical : bool t prop
end

class type group = object
  method afterMove : (jquery t -> Dom_html.element t -> jquery t -> unit) callback prop
  method containerPath : js_string t prop
  method containerSelector : js_string t prop
  method distance : int prop
  method delay : int prop
  method group : js_string t prop
  method handle : js_string t prop
  method itemPath : js_string t prop
  method itemSelector : js_string t prop
  method bodyClass : js_string t prop
  method draggedClass : js_string t prop
  method isValidTarget : (jquery t -> Dom_html.element -> bool t) callback prop
  method onCancel : (jquery t -> Dom_html.element -> 'a -> event_jq t -> unit) callback prop
  method onDrag : (jquery t -> js_string t -> 'a -> event_jq t -> unit) callback prop
  method onDragStart : (jquery t -> Dom_html.element t -> 'a -> event_jq t -> unit) callback prop
  method onDrop : (jquery t -> Dom_html.element t -> 'a -> event_jq -> unit) callback prop
  method onMouseDown : (jquery t -> 'a -> event_jq t -> unit) callback prop
  method placeholderClass : js_string t prop
  method placeholder : js_string t prop
  method pullPlaceholder : bool t prop
  method serialize : (jquery t -> jquery t -> Dom_html.element t -> unit) callback prop
  method tolerance : int prop
end

let make_container_options ?drag ?drop ?exclude ?group ?nested ?vertical () =
  let o : container t = Unsafe.obj [||] in
  (match drag with None -> () | Some drag -> o##.drag := bool drag);
  (match drop with None -> () | Some drop -> o##.drop := bool drop);
  (match exclude with None -> () | Some exclude -> o##.exclude := string exclude);
  (match group with None -> () | Some group -> o##.group := string group);
  (match nested with None -> () | Some nested -> o##.nested := bool nested);
  (match vertical with None -> () | Some vertical -> o##.vertical := bool vertical);
  o

let make_group ?containerPath ?containerSelector ?distance ?delay ?group ?handle
    ?itemPath ?itemSelector ?bodyClass ?draggedClass ?placeholderClass ?placeholder
    ?pullPlaceholder ?tolerance () =
  let o : group t = Unsafe.obj [||] in
  (match containerPath with None -> () | Some containerPath -> o##.containerPath := string containerPath);
  (match containerSelector with None -> () | Some containerSelector -> o##.containerSelector := string containerSelector);
  (match group with None -> () | Some group -> o##.group := string group);
  (match handle with None -> () | Some handle -> o##.handle := string handle);
  (match itemPath with None -> () | Some itemPath -> o##.itemPath := string itemPath);
  (match itemSelector with None -> () | Some itemSelector -> o##.itemSelector := string itemSelector);
  (match bodyClass with None -> () | Some bodyClass -> o##.bodyClass := string bodyClass);
  (match draggedClass with None -> () | Some draggedClass -> o##.draggedClass := string draggedClass);
  (match placeholderClass with None -> () | Some placeholderClass -> o##.placeholderClass := string placeholderClass);
  (match placeholder with None -> () | Some placeholder -> o##.placeholder := string placeholder);
  (match distance with None -> () | Some distance -> o##.distance := distance);
  (match delay with None -> () | Some delay -> o##.delay := delay);
  (match tolerance with None -> () | Some tolerance -> o##.tolerance := tolerance);
  (match pullPlaceholder with None -> () | Some pullPlaceholder -> o##.pullPlaceholder := bool pullPlaceholder);
  o

let sortable ?container_options ?group_options (o:jquery t) =
  (match container_options with
   | None -> ()
   | Some opt -> (Unsafe.coerce o)##sortable opt);
  (match group_options with
   | None -> ()
   | Some opt -> (Unsafe.coerce o)##sortable opt)
