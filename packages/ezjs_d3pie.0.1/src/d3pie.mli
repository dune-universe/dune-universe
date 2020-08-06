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

type d3pie_title = {
  text : string;
}

type d3pie_header = {
  title : d3pie_title;
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

type d3pie_small_segment_grouping_type =
  | Percentage_grouping
  | Value_grouping

type d3pie_small_segment_grouping = {
  enabled : bool;
  value : int;
  value_type : d3pie_small_segment_grouping_type;
  label : string;
  color : string;
}

type d3pie_data_content = {
  label : string;
  value : int;
  caption : string;
}

type d3pie_sort_order =
  | No_sort
  | Sort_by_value_asc
  | Sort_by_value_desc

type d3pie_data = {
  content : d3pie_data_content list;
  sort_order : d3pie_sort_order;
  small_segment_grouping : d3pie_small_segment_grouping;
}

type d3pie_label_format_value =
  | Format_none
  | Format_label
  | Format_value
  | Format_percentage

type d3pie_inner_labels_descriptor = {
  format : d3pie_label_format_value;
  hide_when_less_than_percentage : int option;
}

type d3pie_outer_labels_descriptor = {
  format : d3pie_label_format_value;
  hide_when_less_than_percentage : int option;
  pie_distance : int;
}

type d3pie_labels = {
  outer : d3pie_outer_labels_descriptor;
  inner : d3pie_inner_labels_descriptor;
}

type d3pie_load_effect =
  | Load_effect_none
  | Load_effect_default of int

type d3pie_segment_on_click_effect =
  | Segment_on_click_effect_none
  | Segment_on_click_effect_linear of int * int
  | Segment_on_click_effect_bounce of int * int
  | Segment_on_click_effect_elastic of int * int
  | Segment_on_click_effect_back of int * int

type d3pie_effects = {
  load : d3pie_load_effect;
  segment_on_click : d3pie_segment_on_click_effect;
}

type d3pie_tooltip_type =
  | Tooltip_caption
  | Tooltip_placeholder

type d3pie_tooltips = {
  enabled : bool;
  type_ : d3pie_tooltip_type;
}

type d3pie_misc_colors = {
  segments : string array;
  segment_stroke : string;
}

type d3pie_misc = {
  colors : d3pie_misc_colors;
}

type d3pie_callback_argument_data = {
  is_grouped : bool;
  label : string;
  value : int;
}

type d3pie_callback_argument = {
  segment : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t;
  index : int;
  expanded : bool;
  data : d3pie_callback_argument_data;
  color : string;
}

type d3pie_callbacks = {
  on_click_segment : d3pie_callback_argument -> unit;
  on_mouseover_segment : d3pie_callback_argument -> unit;
  on_mouseout_segment : d3pie_callback_argument -> unit;
}

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

val default_settings:
  d3pie_settings

val set_data_content:
  d3pie_data_content list ->
  d3pie_settings ->
  d3pie_settings

val set_data_sort_order:
  d3pie_sort_order ->
  d3pie_settings ->
  d3pie_settings

val set_size_canvas_height:
  int ->
  d3pie_settings ->
  d3pie_settings

val set_size_canvas_width:
  int ->
  d3pie_settings ->
  d3pie_settings

val set_size_pie_inner_radius:
  d3pie_size_radius_value ->
  d3pie_settings ->
  d3pie_settings

val set_size_pie_outer_radius:
  d3pie_size_radius_value ->
  d3pie_settings ->
  d3pie_settings

val set_inner_label_format:
  d3pie_label_format_value ->
  d3pie_settings ->
  d3pie_settings

val set_outer_label_format:
  d3pie_label_format_value ->
  d3pie_settings ->
  d3pie_settings

val set_tooltip_caption:
  d3pie_settings ->
  d3pie_settings

val set_load_effect:
  d3pie_load_effect ->
  d3pie_settings ->
  d3pie_settings

val set_segment_on_click_effect:
  d3pie_segment_on_click_effect ->
  d3pie_settings ->
  d3pie_settings

val set_on_click_callback:
  (d3pie_callback_argument -> unit) ->
  d3pie_settings ->
  d3pie_settings

val set_on_mouseover_callback:
  (d3pie_callback_argument -> unit) ->
  d3pie_settings ->
  d3pie_settings

val set_on_mouseout_callback:
  (d3pie_callback_argument -> unit) ->
  d3pie_settings ->
  d3pie_settings

val set_small_segment_grouping:
  d3pie_small_segment_grouping_type ->
  int ->
  string ->
  string ->
  d3pie_settings ->
  d3pie_settings

val unset_small_segment_grouping:
  d3pie_settings ->
  d3pie_settings

val set_segments_colors:
  string array ->
  d3pie_settings ->
  d3pie_settings

val set_segment_stroke_color:
  string ->
  d3pie_settings ->
  d3pie_settings

val d3pie:
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  d3pie_settings ->
  D3pie_types.pie Js_of_ocaml.Js.t
