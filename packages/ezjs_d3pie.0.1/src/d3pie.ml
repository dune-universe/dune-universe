(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open D3pie_types

let js_option_of_option = function
  | None -> Js.null
  | Some x -> Js.some x

type d3pie_title = {
  text : string;
}

let create_title text =
  let title : title Js.t = Js.Unsafe.obj [| |] in
  title##.text := text;
  title

let class_of_title title =
  create_title (Js.string title.text)

let default_title = {
  text = "";
}

type d3pie_header = {
  title : d3pie_title;
}

let create_header title =
  let header : header Js.t = Js.Unsafe.obj [| |] in
  header##.title := title;
  header

let class_of_header header =
  create_header (class_of_title header.title)

let default_header = {
  title = default_title;
}

type d3pie_size_radius_value =
  | Radius_default
  | Radius_pixels of int
  | Radius_percentage of int

type d3pie_size = {
  canvas_height : int;
  canvas_width : int;
  pie_inner_radius : d3pie_size_radius_value;
  pie_outer_radius : d3pie_size_radius_value;
}

let create_size_inner_radius = function
  | Radius_default ->
     Js.string "0%"
  | Radius_pixels n ->
     Js.string (string_of_int n)
  | Radius_percentage n ->
     Js.string (string_of_int n ^ "%")

let create_size_outer_radius = function
  | Radius_default ->
     Js.null
  | Radius_pixels n ->
     Js.some (Js.string (string_of_int n))
  | Radius_percentage n ->
     Js.some (Js.string (string_of_int n ^ "%"))

let create_size canvas_height canvas_width pie_inner_radius pie_outer_radius =
  let size : size Js.t = Js.Unsafe.obj [| |] in
  size##.canvasHeight := canvas_height;
  size##.canvasWidth := canvas_width;
  size##.pieInnerRadius := pie_inner_radius;
  size##.pieOuterRadius := pie_outer_radius;
  size

let class_of_size size =
  create_size
    size.canvas_height
    size.canvas_width
    (create_size_inner_radius size.pie_inner_radius)
    (create_size_outer_radius size.pie_outer_radius)

let default_size = {
  canvas_height = 500;
  canvas_width = 500;
  pie_inner_radius = Radius_default;
  pie_outer_radius = Radius_default;
}

type d3pie_small_segment_grouping_type =
  | Percentage_grouping
  | Value_grouping

let create_small_segment_grouping_type = function
  | Percentage_grouping ->
     Js.string "percentage"
  | Value_grouping ->
     Js.string "value"

module Small_segment_grouping = struct

type d3pie_small_segment_grouping = {
  enabled : bool;
  value : int;
  value_type : d3pie_small_segment_grouping_type;
  label : string;
  color : string;
}

end

include Small_segment_grouping

let create_small_segment_grouping enabled value value_type label color =
  let segment_grouping : smallSegmentGrouping Js.t = Js.Unsafe.obj [| |] in
  segment_grouping##.enabled := enabled;
  segment_grouping##.value := value;
  segment_grouping##.valueType := value_type;
  segment_grouping##.label := label;
  segment_grouping##.color := color;
  segment_grouping

let class_of_small_segment_grouping segment_grouping =
  create_small_segment_grouping
    (Js.bool segment_grouping.enabled)
    (segment_grouping.value)
    (create_small_segment_grouping_type segment_grouping.value_type)
    (Js.string segment_grouping.label)
    (Js.string segment_grouping.color)

let default_small_segment_grouping = {
  enabled = false;
  value = 1;
  value_type = Percentage_grouping;
  label = "Other";
  color = "#CCCCCC";
}

module Data_content = struct

  type d3pie_data_content = {
    label : string;
    value : int;
    caption : string;
  }

end

include Data_content

let create_data_content label value caption =
  let data_content : dataContent Js.t = Js.Unsafe.obj [| |] in
  data_content##.label := label;
  data_content##.value := value;
  data_content##.caption := caption;
  data_content

let class_of_data_content data_content =
  create_data_content
    (Js.string data_content.Data_content.label)
    data_content.value
    (Js.string data_content.caption)

(*
let default_data_content = {
  label = "";
  value = 0;
  caption = "";
}
*)

type d3pie_sort_order =
  | No_sort
  | Sort_by_value_asc
  | Sort_by_value_desc

let create_sort_order sort_order =
  let str =
    match sort_order with
    | No_sort -> "none"
    | Sort_by_value_asc -> "value-asc"
    | Sort_by_value_desc -> "value-desc"
  in
  Js.string str

type d3pie_data = {
  content : d3pie_data_content list;
  sort_order : d3pie_sort_order;
  small_segment_grouping : d3pie_small_segment_grouping;
}

let create_data content sort_order small_segment_grouping =
  let data : data Js.t = Js.Unsafe.obj [| |] in
  data##.content := content;
  data##.sortOrder := sort_order;
  data##.smallSegmentGrouping := small_segment_grouping;
  data

let class_of_data data =
  let content = List.map class_of_data_content data.content in
  create_data
    (Js.array (Array.of_list content))
    (create_sort_order data.sort_order)
    (class_of_small_segment_grouping data.small_segment_grouping)

let default_data = {
  content = [];
  sort_order = No_sort;
  small_segment_grouping = default_small_segment_grouping;
}

type d3pie_label_format_value =
  | Format_none
  | Format_label
  | Format_value
  | Format_percentage

let create_label_format_value format_value =
  let str =
    match format_value with
    | Format_none -> "none"
    | Format_label -> "label"
    | Format_value -> "value"
    | Format_percentage -> "percentage"
  in
  Js.string str

module Inner_labels_descriptor = struct

type d3pie_inner_labels_descriptor = {
  format : d3pie_label_format_value;
  hide_when_less_than_percentage : int option;
}

end

include Inner_labels_descriptor

let create_inner_labels_descriptor format hide_when_less_than_percentage =
  let inner_labels_descriptor : innerLabelsDescriptor Js.t =
    Js.Unsafe.obj [| |]
  in
  inner_labels_descriptor##.format :=
    format;
  inner_labels_descriptor##.hideWhenLessThanPercentage :=
    hide_when_less_than_percentage;
  inner_labels_descriptor

let class_of_inner_labels_descriptor inner_labels_descriptor =
  create_inner_labels_descriptor
    (create_label_format_value inner_labels_descriptor.format)
    (js_option_of_option inner_labels_descriptor.hide_when_less_than_percentage)

let default_inner_labels_descriptor = {
  format = Format_percentage;
  hide_when_less_than_percentage = None;
}

module Outer_labels_descriptor = struct

type d3pie_outer_labels_descriptor = {
  format : d3pie_label_format_value;
  hide_when_less_than_percentage : int option;
  pie_distance : int;
}
end

include Outer_labels_descriptor


let create_outer_labels_descriptor
      format hide_when_less_than_percentage pie_distance =
  let outer_labels_descriptor : outerLabelsDescriptor Js.t =
    Js.Unsafe.obj [| |]
  in
  outer_labels_descriptor##.format :=
    format;
  outer_labels_descriptor##.hideWhenLessThanPercentage :=
    hide_when_less_than_percentage;
  outer_labels_descriptor##.pieDistance :=
    pie_distance;
  outer_labels_descriptor

let class_of_outer_labels_descriptor outer_labels_descriptor =
  create_outer_labels_descriptor
    (create_label_format_value outer_labels_descriptor.Outer_labels_descriptor.format)
    (js_option_of_option outer_labels_descriptor.hide_when_less_than_percentage)
    outer_labels_descriptor.pie_distance

let default_outer_labels_descriptor = {
  format = Format_label;
  hide_when_less_than_percentage = None;
  pie_distance = 30;
}

type d3pie_labels = {
  outer : d3pie_outer_labels_descriptor;
  inner : d3pie_inner_labels_descriptor;
}

let create_labels outer inner =
  let labels : labels Js.t =
    Js.Unsafe.obj [| |]
  in
  labels##.outer := outer;
  labels##.inner := inner;
  labels

let class_of_labels labels =
  create_labels
    (class_of_outer_labels_descriptor labels.outer)
    (class_of_inner_labels_descriptor labels.inner)

let default_labels = {
  inner = default_inner_labels_descriptor;
  outer = default_outer_labels_descriptor;
}

type d3pie_load_effect =
  | Load_effect_none
  | Load_effect_default of int

let create_load_effect effect speed =
  let load_effect : loadEffect Js.t =
    Js.Unsafe.obj [| |]
  in
  load_effect##.effect := effect;
  load_effect##.speed := speed;
  load_effect

let class_of_load_effect load_effect =
  let effect, speed =
    match load_effect with
    | Load_effect_none ->
       "none", 0
    | Load_effect_default speed ->
       "default", speed
  in
  create_load_effect
    (Js.string effect)
    speed

let default_load_effect =
  Load_effect_default 1000

type d3pie_segment_on_click_effect =
  | Segment_on_click_effect_none
  | Segment_on_click_effect_linear of int * int
  | Segment_on_click_effect_bounce of int * int
  | Segment_on_click_effect_elastic of int * int
  | Segment_on_click_effect_back of int * int

let create_segment_on_click_effect effect speed size =
  let segment_on_click_effect : pullOutSegmentOnClickEffect Js.t =
    Js.Unsafe.obj [| |]
  in
  segment_on_click_effect##.effect := effect;
  segment_on_click_effect##.speed := speed;
  segment_on_click_effect##.size := size;
  segment_on_click_effect

let class_of_segment_on_click_effect segment_on_click_effect =
  let effect, speed, size =
    match segment_on_click_effect with
    | Segment_on_click_effect_none ->
       "none", 0, 0
    | Segment_on_click_effect_linear (speed, size) ->
       "linear", speed, size
    | Segment_on_click_effect_bounce (speed, size) ->
       "bounce", speed, size
    | Segment_on_click_effect_elastic (speed, size) ->
       "elastic", speed, size
    | Segment_on_click_effect_back (speed, size) ->
       "back", speed, size
  in
  create_segment_on_click_effect
    (Js.string effect)
    speed
    size

let default_segment_on_click_effect =
  Segment_on_click_effect_linear (300, 10)

type d3pie_effects = {
  load : d3pie_load_effect;
  segment_on_click : d3pie_segment_on_click_effect;
}

let create_effects load pull_out_segment_on_click =
  let effects : effects Js.t =
    Js.Unsafe.obj [| |]
  in
  effects##.load := load;
  effects##.pullOutSegmentOnClick := pull_out_segment_on_click;
  effects

let class_of_effects effects =
  create_effects
    (class_of_load_effect effects.load)
    (class_of_segment_on_click_effect effects.segment_on_click)

let default_effects = {
  load = default_load_effect;
  segment_on_click = default_segment_on_click_effect;
}

type d3pie_tooltip_type =
  | Tooltip_caption
  | Tooltip_placeholder

let create_tooltip_type type_ =
  let str =
    match type_ with
    | Tooltip_caption -> "caption"
    | Tooltip_placeholder -> "placeholder"
  in
  Js.string str

module Tooltips = struct

type d3pie_tooltips = {
  enabled : bool;
  type_ : d3pie_tooltip_type;
}

    end
include Tooltips

let create_tooltips enabled type_ =
  let tooltips : tooltips Js.t =
    Js.Unsafe.obj [| |]
  in
  tooltips##.enabled := enabled;
  tooltips##.type_ := type_;
  tooltips

let class_of_tooltips tooltips =
  create_tooltips
    (Js.bool tooltips.Tooltips.enabled)
    (create_tooltip_type tooltips.type_)

let default_tooltips = {
  enabled = false;
  type_ = Tooltip_placeholder;
}

type d3pie_misc_colors = {
  segments : string array;
  segment_stroke : string;
}

let create_misc_colors segments segment_stroke =
  let colors : miscColors Js.t =
    Js.Unsafe.obj [| |]
  in
  colors##.segments := segments;
  colors##.segmentStroke := segment_stroke;
  colors

let class_of_misc_colors misc_colors =
  let segments = Array.map Js.string misc_colors.segments in
  create_misc_colors
    (Js.array segments)
    (Js.string misc_colors.segment_stroke)

let default_misc_colors = {
  segments = [|
    "#2484c1"; "#65a620"; "#7b6888"; "#a05d56"; "#961a1a";
    "#d8d23a"; "#e98125"; "#d0743c"; "#635222"; "#6ada6a";
    "#0c6197"; "#7d9058"; "#207f33"; "#44b9b0"; "#bca44a";
    "#e4a14b"; "#a3acb2"; "#8cc3e9"; "#69a6f9"; "#5b388f";
    "#546e91"; "#8bde95"; "#d2ab58"; "#273c71"; "#98bf6e";
    "#4daa4b"; "#98abc5"; "#cc1010"; "#31383b"; "#006391";
    "#c2643f"; "#b0a474"; "#a5a39c"; "#a9c2bc"; "#22af8c";
    "#7fcecf"; "#987ac6"; "#3d3b87"; "#b77b1c"; "#c9c2b6";
    "#807ece"; "#8db27c"; "#be66a2"; "#9ed3c6"; "#00644b";
    "#005064"; "#77979f"; "#77e079"; "#9c73ab"; "#1f79a7";
  |];
  segment_stroke = "#ffffff";
}

type d3pie_misc = {
  colors : d3pie_misc_colors;
}

let create_misc colors =
  let misc : misc Js.t =
    Js.Unsafe.obj [| |]
  in
  misc##.colors := colors;
  misc

let class_of_misc misc =
  create_misc
    (class_of_misc_colors misc.colors)

let default_misc = {
  colors = default_misc_colors;
}

type d3pie_callback_argument_data = {
  is_grouped : bool;
  label : string;
  value : int;
}

let callback_argument_data_of_class callbackArgumentData =
  {
    is_grouped = Js.to_bool (callbackArgumentData##.isGrouped);
    label = Js.to_string (callbackArgumentData##.label);
    value = callbackArgumentData##.value;
  }

type d3pie_callback_argument = {
  segment : Js_of_ocaml.Dom_html.element Js.t;
  index : int;
  expanded : bool;
  data : d3pie_callback_argument_data;
  color : string;
}

let callback_argument_of_class callbackArgument =
  {
    segment = callbackArgument##.segment;
    index = callbackArgument##.index;
    expanded = Js.to_bool (callbackArgument##.expanded);
    data = callback_argument_data_of_class (callbackArgument##.data);
    color = Js.to_string callbackArgument##.segment##.style##.fill;
  }

type d3pie_callbacks = {
  on_click_segment : d3pie_callback_argument -> unit;
  on_mouseover_segment : d3pie_callback_argument -> unit;
  on_mouseout_segment : d3pie_callback_argument -> unit;
}

let default_callback_value = fun _ -> ()

let create_callbacks on_click_segment on_mouseover_segment on_mouseout_segment =
  let callbacks : callbacks Js.t =
    Js.Unsafe.obj [| |]
  in
  callbacks##.onClickSegment := on_click_segment;
  callbacks##.onMouseoverSegment := on_mouseover_segment;
  callbacks##.onMouseoutSegment := on_mouseout_segment;
  callbacks

let class_of_callbacks callbacks =
  let on_click_segment arg =
    callbacks.on_click_segment (callback_argument_of_class arg)
  in
  let on_mouseover_segment arg =
    callbacks.on_mouseover_segment (callback_argument_of_class arg)
  in
  let on_mouseout_segment arg =
    callbacks.on_mouseout_segment (callback_argument_of_class arg)
  in
  create_callbacks
    (Js.wrap_callback on_click_segment)
    (Js.wrap_callback on_mouseover_segment)
    (Js.wrap_callback on_mouseout_segment)

let default_callbacks = {
  on_click_segment = default_callback_value;
  on_mouseover_segment = default_callback_value;
  on_mouseout_segment = default_callback_value;
}

module Settings = struct

type d3pie_settings = {
  data : d3pie_data;
  header : d3pie_header;
  size : d3pie_size;
  labels : d3pie_labels;
  effects : d3pie_effects;
  tooltips : d3pie_tooltips;
  misc : d3pie_misc;
  callbacks : d3pie_callbacks;
}
end
include Settings

let create_settings data header size labels effects tooltips misc callbacks =
  let settings : settings Js.t = Js.Unsafe.obj [||] in
  settings##.data := data;
  settings##.header := header;
  settings##.size := size;
  settings##.labels := labels;
  settings##.effects := effects;
  settings##.tooltips := tooltips;
  settings##.misc := misc;
  settings##.callbacks := callbacks;
  settings

let class_of_settings settings =
  create_settings
    (class_of_data settings.Settings.data)
    (class_of_header settings.header)
    (class_of_size settings.size)
    (class_of_labels settings.labels)
    (class_of_effects settings.effects)
    (class_of_tooltips settings.tooltips)
    (class_of_misc settings.misc)
    (class_of_callbacks settings.callbacks)

let default_settings = {
  data = default_data;
  header = default_header;
  size = default_size;
  labels = default_labels;
  effects = default_effects;
  tooltips = default_tooltips;
  misc = default_misc;
  callbacks = default_callbacks;
}

let set_data_content data_content settings =
  {
    settings with
    Settings.data = {
      settings.Settings.data with
      content = data_content;
    };
  }

let set_data_sort_order sort_order settings =
  {
    settings with
    Settings.data = {
      settings.Settings.data with
      sort_order = sort_order;
    };
  }

let set_size_canvas_height canvas_height settings =
  {
    settings with
    size = {
      settings.size with
      canvas_height = canvas_height;
    };
  }

let set_size_canvas_width canvas_width settings =
  {
    settings with
    size = {
      settings.size with
      canvas_width = canvas_width;
    };
  }

let set_size_pie_inner_radius pie_inner_radius settings =
  {
    settings with
    size = {
      settings.size with
      pie_inner_radius = pie_inner_radius;
    };
  }

let set_size_pie_outer_radius pie_outer_radius settings =
  {
    settings with
    size = {
      settings.size with
      pie_outer_radius = pie_outer_radius;
    };
  }

let set_inner_label_format format settings =
  {
    settings with
    labels = {
      settings.labels with
      inner = {
        settings.labels.inner with
        Inner_labels_descriptor.format = format;
      };
    };
  }

let set_outer_label_format format settings =
  {
    settings with
    labels = {
      settings.labels with
      outer = {
        settings.labels.outer with
        format = format;
      };
    };
  }

let set_tooltip_caption settings =
  {
    settings with
    tooltips = {
      enabled = true;
      type_ = Tooltip_caption;
    };
  }

let set_load_effect load_effect settings =
  {
    settings with
    effects = {
      settings.effects with
      load = load_effect
    };
  }

let set_segment_on_click_effect segment_on_click_effect settings =
  {
    settings with
    effects = {
      settings.effects with
      segment_on_click = segment_on_click_effect;
    };
  }

let set_on_click_callback on_click_segment settings =
  {
    settings with
    callbacks = {
      settings.callbacks with
      on_click_segment = on_click_segment;
    };
  }

let set_on_mouseover_callback on_mouseover_segment settings =
  {
    settings with
    callbacks = {
      settings.callbacks with
      on_mouseover_segment = on_mouseover_segment;
    };
  }

let set_on_mouseout_callback on_mouseout_segment settings =
  {
    settings with
    callbacks = {
      settings.callbacks with
      on_mouseout_segment = on_mouseout_segment;
    };
  }

let set_small_segment_grouping value_type value label color settings =
  {
    settings with
    Settings.data = {
      settings.Settings.data with
      small_segment_grouping = {
        Small_segment_grouping.enabled = true;
        value_type = value_type;
        value = value;
        label = label;
        color = color;
      }
    }
  }

let unset_small_segment_grouping settings =
  {
    settings with
    Settings.data = {
      settings.Settings.data with
      small_segment_grouping = {
        settings.data.small_segment_grouping with
        Small_segment_grouping.enabled = false;
      }
    }
  }

let set_segments_colors array_color settings =
  {
    settings with
    misc = {
      colors = {
        settings.misc.colors with
        segments = array_color;
      }
    }
  }

let set_segment_stroke_color color settings =
  {
    settings with
    misc = {
      colors = {
        settings.misc.colors with
        segment_stroke = color;
      }
    }
  }

let d3pie div settings =
  let d3pie_cstr = Js.Unsafe.global##._d3pie in
  let pie : pie Js.t =
    Js.Unsafe.new_obj d3pie_cstr [|
      Js.Unsafe.inject div;
      Js.Unsafe.inject (class_of_settings settings);
    |]
  in
  Js.Unsafe.global##.pie := pie; (* todo remove when lib correct *)
  pie
