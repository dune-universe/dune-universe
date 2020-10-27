open Js_of_ocaml
open Js

class type position =
  object
    method x : int readonly_prop
    method y : int readonly_prop
  end

class type ['a] style =
  object
    method selector : js_string t readonly_prop
    method style : 'a readonly_prop
  end

module DataItem =
struct

  class type data =
    object
      method id : js_string t prop
      method source : js_string t prop
      method target : js_string t prop
    end

  class type t =
    object
      method data : data Js.t prop
      method group : js_string Js.t prop
      method position : position Js.t prop
      method renderedPosition : position Js.t prop
    end
end

class type layout_options =
  object
    method name : js_string t readonly_prop
  end

class type layout =
  object
    method run : unit meth
  end

class type props =
  object
    method container : Dom_html.element t prop
    method elements : DataItem.t t js_array t prop
    method style : Unsafe.any style t js_array t prop
    method layout : layout_options t prop
    method zoom : int prop
    method pan : position t prop
    method minZoom : float prop
    method maxZoom : float prop
    method zoomingEnabled : bool t prop
    method userZoomingEnabled : bool t prop
    method panningEnabled : bool t prop
    method userPanningEnabled :bool t prop
    method boxSelectionEnabled : bool t prop
    method selectionType : js_string t prop
    method touchTapThreshold : int prop
    method desktopTapThreshold : int prop
    method autolock : bool t prop
    method autoungrabify : bool t prop
    method autounselectify : bool t prop
    method headless : bool t prop
    method styleEnabled : bool t prop
    method hideEdgesOnViewport : bool t prop
    method textureOnViewport : bool t prop
    method motionBlur : bool t prop
    method motionBlurOpacity : float prop
    method wheelSensitivity : float prop
    method pixelRatio : js_string t prop
  end

class type cytoscape =
  object
    method add : DataItem.t t -> unit meth
    method remove : DataItem.t t -> unit meth
    method mount : Dom_html.element t -> unit meth
    method layout : layout_options t -> layout t meth
    method resize : unit meth
    method on : js_string t -> js_string t -> (Dom_html.event t -> unit) -> unit meth
  end

type cytoscape_cs = (props t -> cytoscape t) constr

let cytoscape_cs : cytoscape_cs = Unsafe.variable "cytoscape"

let default_style : Unsafe.any style t js_array t =
  let node_style = Unsafe.coerce @@ object%js
      val selector = string "node"
      val style = def (object%js
          val label = string "data(id)"
        end)
    end in
  array [| node_style |]

let default_layout : layout_options t =
  object%js val name = string "preset" end

let position x y : position t =
  object%js val x = x val y = y end

let node ?pos id : DataItem.t t =
  let data : DataItem.data t = Unsafe.obj [||] in
  data##.id := string id;
  let node : DataItem.t t = Unsafe.obj [||] in
  node##.data := data;
  node##.group := string "nodes";
  (match pos with None -> () | Some (x, y) -> node##.position := position x y);
  node

let edge ?id source target : DataItem.t t =
  let data : DataItem.data t = Unsafe.obj [||] in
  (match id with None -> () | Some id -> data##.id := string id);
  data##.source := string source;
  data##.target := string target;
  let edge : DataItem.t t = Unsafe.obj [||] in
  edge##.data := data;
  edge##.group := string "edges";
  edge

let mk_graph ?(style=default_style) ?(layout=default_layout) ?(props=[]) container_id =
  let container = Dom_html.getElementById container_id in
  let props = array @@ Array.of_list props in
  let g : props t = Unsafe.obj [||] in
  g##.container := container;
  g##.elements := props;
  g##.style := style;
  g##.layout := layout;
  g

let display props = new%js cytoscape_cs props

let add_node ?pos g nodename  =
  g##add (node ?pos nodename)

let add_edge g ~source ~target =
  g##add (edge source target)

let random_layout g : layout t =
  let layout_opt = object%js
    val name = string "random"
  end in
  g##layout layout_opt

let run_layout (l : layout t) =
  l##run

let on cy event selector cb =
  cy##on (string event) (string selector) cb
