open Ctypes
open Foreign

module LV2 = struct
  type handle = unit ptr

  let handle : handle typ = ptr void

  type descriptor

  let descriptor : descriptor structure typ = structure "LV2_Descriptor"
  let descriptor_uri = field descriptor "URI" string

  let descriptor_instantiate =
    field descriptor "instantiate"
      (funptr
         (ptr descriptor @-> double @-> string @-> ptr void @-> returning handle))

  let descriptor_connect_port =
    field descriptor "instantiate"
      (funptr (handle @-> uint32_t @-> ptr void @-> returning void))

  let descriptor_activate =
    field descriptor "activate" (funptr (handle @-> returning void))

  let descriptor_run =
    field descriptor "run" (funptr (handle @-> uint32_t @-> returning void))

  let descriptor_deactivate =
    field descriptor "deactivate" (funptr (handle @-> returning void))

  let descriptor_cleanup =
    field descriptor "cleanup" (funptr (handle @-> returning void))

  let descriptor_extension_data =
    field descriptor "extension_data" (funptr (string @-> returning (ptr void)))

  let () =
    ignore descriptor_uri;
    ignore descriptor_instantiate;
    ignore descriptor_cleanup;
    ignore descriptor_extension_data;
    seal descriptor

  module Core = struct
    let uri = "http://lv2plug.in/ns/lv2core"
    let prefix s = uri ^ "#" ^ s
    let input_port = prefix "InputPort"
    let output_port = prefix "OutputPort"
    let audio_port = prefix "AudioPort"
    let control_port = prefix "ControlPort"
    let connection_optional = prefix "connectionOptional"
  end
end

type world = unit ptr

let world : world typ = ptr void

type plugin = unit ptr

let plugin : plugin typ = ptr void
let plugin_opt : plugin option typ = ptr_opt void

type instance_impl

let instance_impl : instance_impl structure typ = structure "LilvInstanceImpl"

let instance_impl_descriptor =
  field instance_impl "lv2_descriptor" (ptr LV2.descriptor)

let instance_impl_handle = field instance_impl "lv2_handle" LV2.handle
let instance_impl_pimpl = field instance_impl "pimpl" (ptr void)

let () =
  ignore instance_impl_pimpl;
  seal instance_impl

type instance = instance_impl structure ptr

let instance : instance typ = ptr instance_impl

type plugins = unit ptr

let plugins : plugins typ = ptr void

type port = unit ptr

let port : port typ = ptr void

type plugin_class = unit ptr

let plugin_class : plugin_class typ = ptr void

type node = unit ptr

let node : node typ = ptr void
let node_opt : node option typ = ptr_opt void

type nodes = unit ptr

let nodes : nodes typ = ptr void

type iterator = unit ptr

let iterator : iterator typ = ptr void

module Node = struct
  type t = node

  let null : t = from_voidp void null
  let free = foreign "lilv_node_free" (node @-> returning void)
  let equals = foreign "lilv_node_equals" (node @-> node @-> returning bool)
  let finalise n = Gc.finalise free n

  let finalised n =
    finalise n;
    n

  let is_uri = foreign "lilv_node_is_uri" (node @-> returning bool)
  let to_uri = foreign "lilv_node_as_uri" (node @-> returning string)
  let uri = foreign "lilv_new_uri" (world @-> string @-> returning node)
  let uri w s = finalised (uri w s)
  let is_blank = foreign "lilv_node_is_blank" (node @-> returning bool)
  let to_blank = foreign "lilv_node_as_blank" (node @-> returning string)
  let is_string = foreign "lilv_node_is_string" (node @-> returning bool)
  let to_string = foreign "lilv_node_as_string" (node @-> returning string)
  let string = foreign "lilv_new_string" (world @-> string @-> returning node)
  let string w s = finalised (string w s)
  let is_int = foreign "lilv_node_is_int" (node @-> returning bool)
  let to_int = foreign "lilv_node_as_int" (node @-> returning int)
  let int = foreign "lilv_new_int" (world @-> int @-> returning node)
  let int w s = finalised (int w s)
  let is_float = foreign "lilv_node_is_float" (node @-> returning bool)
  let to_float = foreign "lilv_node_as_float" (node @-> returning float)
  let float = foreign "lilv_new_float" (world @-> float @-> returning node)
  let float w s = finalised (float w s)
  let is_bool = foreign "lilv_node_is_bool" (node @-> returning bool)
  let bool = foreign "lilv_new_bool" (world @-> bool @-> returning node)
  let bool w s = finalised (bool w s)
end

module Nodes = struct
  type t = nodes
  type nodes_iterator = t * iterator

  let length = foreign "lilv_nodes_size" (nodes @-> returning int)
  let length p = length p
  let iterate = foreign "lilv_nodes_begin" (nodes @-> returning iterator)
  let iterate p : nodes_iterator = (p, iterate p)
  let get = foreign "lilv_nodes_get" (nodes @-> iterator @-> returning plugin)
  let get ((p, i) : nodes_iterator) = get p i

  let next =
    foreign "lilv_nodes_next" (nodes @-> iterator @-> returning iterator)

  let next ((p, i) : nodes_iterator) = (p, next p i)

  let is_end =
    foreign "lilv_nodes_is_end" (nodes @-> iterator @-> returning bool)

  let is_end ((p, i) : nodes_iterator) = is_end p i

  let iter f p =
    let i = ref (iterate p) in
    while not (is_end !i) do
      f (get !i);
      i := next !i
    done

  let to_list p : Node.t list =
    let ans = ref [] in
    iter (fun p -> ans := p :: !ans) p;
    List.rev !ans
end

module Port = struct
  type t = (world * plugin) * port

  let make plugin port : t = (plugin, port)
  let get_world (p : t) = fst (fst p)
  let get_plugin (p : t) = snd (fst p)
  let get_port (p : t) = snd p

  let is_a =
    foreign "lilv_port_is_a" (plugin @-> port @-> node @-> returning bool)

  let is_a n p = is_a (get_plugin p) (get_port p) n
  let is_input p = is_a (Node.uri (get_world p) LV2.Core.input_port) p
  let is_output p = is_a (Node.uri (get_world p) LV2.Core.output_port) p
  let is_audio p = is_a (Node.uri (get_world p) LV2.Core.audio_port) p
  let is_control p = is_a (Node.uri (get_world p) LV2.Core.control_port) p

  let has_property =
    foreign "lilv_port_has_property"
      (plugin @-> port @-> node @-> returning bool)

  let has_property n p = has_property (get_plugin p) (get_port p) n

  let is_connection_optional p =
    has_property (Node.uri (get_world p) LV2.Core.connection_optional) p

  let index =
    foreign "lilv_port_get_index" (plugin @-> port @-> returning uint32_t)

  let index p = Unsigned.UInt32.to_int (index (get_plugin p) (get_port p))

  let symbol =
    foreign "lilv_port_get_symbol" (plugin @-> port @-> returning node)

  let symbol p = Node.to_string (symbol (get_plugin p) (get_port p))
  let name = foreign "lilv_port_get_name" (plugin @-> port @-> returning node)

  let name p =
    Node.to_string (Node.finalised (name (get_plugin p) (get_port p)))

  let range =
    foreign "lilv_port_get_range"
      (plugin @-> port @-> ptr node @-> ptr node @-> ptr node @-> returning void)

  let range p =
    let def = allocate node Node.null in
    let min = allocate node Node.null in
    let max = allocate node Node.null in
    range (get_plugin p) (get_port p) def min max;
    let def = Node.finalised !@def in
    let min = Node.finalised !@min in
    let max = Node.finalised !@max in
    (def, min, max)

  let range_float p =
    let def, min, max = range p in
    (Node.to_float def, Node.to_float min, Node.to_float max)

  let default_float p =
    let def, _, _ = range p in
    let def = Node.to_float def in
    if compare def nan = 0 then None else Some def

  let min_float p =
    let _, min, _ = range p in
    let min = Node.to_float min in
    if compare min nan = 0 then None else Some min

  let max_float p =
    let _, _, max = range p in
    let max = Node.to_float max in
    if compare max nan = 0 then None else Some max
end

module Plugin = struct
  type t = world * plugin

  let make w p : t = (w, p)
  let get_world (p : t) = fst p
  let get_plugin (p : t) = snd p
  let uri = foreign "lilv_plugin_get_uri" (plugin @-> returning node)
  let uri p = Node.to_uri (uri (get_plugin p))
  let name = foreign "lilv_plugin_get_name" (plugin @-> returning node)
  let name p = Node.to_string (Node.finalised (name (get_plugin p)))

  let author_name =
    foreign "lilv_plugin_get_author_name" (plugin @-> returning node_opt)

  let author_name p =
    match author_name (get_plugin p) with
      | Some node -> Node.to_string (Node.finalised node)
      | None -> ""

  let author_email =
    foreign "lilv_plugin_get_author_email" (plugin @-> returning node_opt)

  let author_email p =
    match author_email (get_plugin p) with
      | Some node -> Node.to_string (Node.finalised node)
      | None -> ""

  let author_homepage =
    foreign "lilv_plugin_get_author_homepage" (plugin @-> returning node_opt)

  let author_homepage p =
    match author_homepage (get_plugin p) with
      | Some node -> Node.to_string (Node.finalised node)
      | None -> ""

  module Class = struct
    type t = plugin_class

    let label =
      foreign "lilv_plugin_class_get_label" (plugin_class @-> returning node)

    let label c = Node.to_string (label c)
  end

  let plugin_class =
    foreign "lilv_plugin_get_class" (plugin @-> returning plugin_class)

  let plugin_class p = plugin_class (get_plugin p)

  let supported_features =
    foreign "lilv_plugin_get_supported_features" (plugin @-> returning nodes)

  let supported_features p = Nodes.to_list (supported_features (get_plugin p))

  let required_features =
    foreign "lilv_plugin_get_required_features" (plugin @-> returning nodes)

  let required_features p = Nodes.to_list (required_features (get_plugin p))

  let optional_features =
    foreign "lilv_plugin_get_optional_features" (plugin @-> returning nodes)

  let optional_features p = Nodes.to_list (optional_features (get_plugin p))

  let num_ports =
    foreign "lilv_plugin_get_num_ports" (plugin @-> returning int32_t)

  let num_ports p = Int32.to_int (num_ports (get_plugin p))
  let is_replaced = foreign "lilv_plugin_is_replaced" (plugin @-> returning bool)
  let is_replaced p = is_replaced (get_plugin p)

  let port_by_index =
    foreign "lilv_plugin_get_port_by_index"
      (plugin @-> int32_t @-> returning port)

  let port_by_index p i =
    Port.make p (port_by_index (get_plugin p) (Int32.of_int i))

  let port_by_symbol =
    foreign "lilv_plugin_get_port_by_symbol" (plugin @-> node @-> returning port)

  let port_by_symbol p s =
    Port.make p (port_by_symbol (get_plugin p) (Node.string (get_world p) s))

  let has_latency = foreign "lilv_plugin_has_latency" (plugin @-> returning bool)
  let has_latency p = has_latency (get_plugin p)

  let latency_port_index =
    foreign "lilv_plugin_get_latency_port_index" (plugin @-> returning int32_t)

  let latency_port_index p = Int32.to_int (latency_port_index (get_plugin p))

  module Instance = struct
    type t = instance

    let free = foreign "lilv_instance_free" (instance @-> returning void)

    let finalised i =
      Gc.finalise free i;
      i

    let descriptor (i : t) = getf !@i instance_impl_descriptor
    let handle (i : t) = getf !@i instance_impl_handle

    let connect_port (i : t) n =
      getf
        !@(descriptor i)
        LV2.descriptor_connect_port (handle i) (Unsigned.UInt32.of_int n)

    let connect_port_float i n
        (data :
          (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t) =
      let data = array_of_bigarray array1 data in
      connect_port i n (to_voidp (CArray.start data))

    let activate i = getf !@(descriptor i) LV2.descriptor_activate (handle i)

    let deactivate i =
      getf !@(descriptor i) LV2.descriptor_deactivate (handle i)

    let run i n =
      getf
        !@(descriptor i)
        LV2.descriptor_run (handle i) (Unsigned.UInt32.of_int n)
  end

  let instantiate =
    foreign "lilv_plugin_instantiate"
      (plugin @-> double @-> ptr void @-> returning instance)

  (* TODO: features *)
  let instantiate p samplerate =
    Instance.finalised
      (instantiate (get_plugin p) samplerate (from_voidp void null))
end

module Plugins = struct
  type t = world * plugins
  type plugins_iterator = t * iterator

  let make world plugins : t = (world, plugins)
  let get_world (p : t) = fst p
  let get_plugins (p : t) = snd p
  let length = foreign "lilv_plugins_size" (plugins @-> returning int)
  let length p = length (get_plugins p)
  let iterate = foreign "lilv_plugins_begin" (plugins @-> returning iterator)
  let iterate p : plugins_iterator = (p, iterate (get_plugins p))

  let get =
    foreign "lilv_plugins_get" (plugins @-> iterator @-> returning plugin)

  let get ((p, i) : plugins_iterator) =
    Plugin.make (get_world p) (get (get_plugins p) i)

  let get_by_uri =
    foreign "lilv_plugins_get_by_uri" (plugins @-> node @-> returning plugin_opt)

  let get_by_uri p uri =
    match get_by_uri (get_plugins p) (Node.uri (get_world p) uri) with
      | Some plugin -> Plugin.make (get_world p) plugin
      | None -> raise Not_found

  let next =
    foreign "lilv_plugins_next" (plugins @-> iterator @-> returning iterator)

  let next ((p, i) : plugins_iterator) = (p, next (get_plugins p) i)

  let is_end =
    foreign "lilv_plugins_is_end" (plugins @-> iterator @-> returning bool)

  let is_end ((p, i) : plugins_iterator) = is_end (get_plugins p) i

  let iter f p =
    let i = ref (iterate p) in
    while not (is_end !i) do
      f (get !i);
      i := next !i
    done

  let to_list p =
    let ans = ref [] in
    iter (fun p -> ans := p :: !ans) p;
    List.rev !ans
end

module State = struct end

module World = struct
  type t = world

  let t = world
  let free = foreign "lilv_world_free" (t @-> returning void)
  let create = foreign "lilv_world_new" (void @-> returning t)

  let create () =
    let w = create () in
    Gc.finalise free w;
    w

  let load_all = foreign "lilv_world_load_all" (t @-> returning void)
  let plugins = foreign "lilv_world_get_all_plugins" (t @-> returning plugins)
  let plugins w = Plugins.make w (plugins w)
end
