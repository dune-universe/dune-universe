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

module Js = Js_of_ocaml.Js

(**
 Documentation : d3pie.org/#docs
 **)

class type title = object
  method text : Js.js_string Js.t Js.prop
end

class type header = object
  method title : title Js.t Js.prop
end

class type smallSegmentGrouping = object
  method enabled : bool Js.t Js.prop
  method value : int Js.prop
  method valueType : Js.js_string Js.t Js.prop
  method label : Js.js_string Js.t Js.prop
  method color : Js.js_string Js.t Js.prop
end

class type dataContent = object
  method label : Js.js_string Js.t Js.prop
  method value : int Js.prop
  method caption : Js.js_string Js.t Js.prop
end

class type data = object
  method sortOrder : Js.js_string Js.t Js.prop
  method smallSegmentGrouping : smallSegmentGrouping Js.t Js.prop
  method content : dataContent Js.t Js.js_array Js.t Js.prop
end

class type size = object
  method canvasHeight : int Js.prop
  method canvasWidth : int Js.prop
  method pieInnerRadius : Js.js_string Js.t Js.prop
  method pieOuterRadius : Js.js_string Js.t Js.opt Js.prop
end

class type innerLabelsDescriptor = object
  method format : Js.js_string Js.t Js.prop
  method hideWhenLessThanPercentage : int Js.opt Js.prop
end

class type outerLabelsDescriptor = object
  method format : Js.js_string Js.t Js.prop
  method hideWhenLessThanPercentage : int Js.opt Js.prop
  method pieDistance : int Js.prop
end

class type labels = object
  method inner : innerLabelsDescriptor Js.t Js.prop
  method outer : outerLabelsDescriptor Js.t Js.prop
end

class type loadEffect = object
  method effect : Js.js_string Js.t Js.prop
  method speed : int Js.prop
end

class type pullOutSegmentOnClickEffect = object
  method effect : Js.js_string Js.t Js.prop
  method speed : int Js.prop
  method size : int Js.prop
end

class type effects = object
  method load : loadEffect Js.t Js.prop
  method pullOutSegmentOnClick : pullOutSegmentOnClickEffect Js.t Js.prop
end

class type tooltips = object
  method enabled : bool Js.t Js.prop
  method type_ : Js.js_string Js.t Js.prop
end

class type callbackArgumentData = object
  method isGrouped : bool Js.t Js.prop
  method label : Js.js_string Js.t Js.prop
  method value : int Js.prop
end

class type miscColors = object
  method segments : Js.js_string Js.t Js.js_array Js.t Js.prop
  method segmentStroke : Js.js_string Js.t Js.prop
end

class type misc = object
  method colors : miscColors Js.t Js.prop
end

class type callbackArgument = object
  method segment : Js_of_ocaml.Dom_html.element Js.t Js.prop
  method index : int Js.prop
  method expanded : bool Js.t Js.prop
  method data : callbackArgumentData Js.t Js.prop
end

class type callbacks = object
  method onClickSegment :
    (callbackArgument Js.t -> unit) Js.callback Js.writeonly_prop
  method onMouseoverSegment :
    (callbackArgument Js.t -> unit) Js.callback Js.writeonly_prop
  method onMouseoutSegment :
    (callbackArgument Js.t -> unit) Js.callback Js.writeonly_prop
end

class type settings = object
  method header : header Js.t Js.prop
  method size : size Js.t Js.prop
  method data : data Js.t Js.prop
  method labels : labels Js.t Js.prop
  method effects : effects Js.t Js.prop
  method tooltips : tooltips Js.t Js.prop
  method misc : misc Js.t Js.prop
  method callbacks : callbacks Js.t Js.prop
end

class type pie = object
  method redraw : unit Js.meth
end
