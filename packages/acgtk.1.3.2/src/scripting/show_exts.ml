open Diagram
open Logic.Lambda.Lambda

module type Show_text_sig = sig
  val n : string -> diagram
  val b : string -> diagram
  val i : string -> diagram
end

module type Show_colors_sig = sig
  val lines : color list
  val tree : color
  val background : Rendering_config.config -> color
  val node_background : Rendering_config.config -> color
end

type pp = term -> int -> int -> env * env -> consts -> diagram * bool
type open_pp = pp -> pp
type open_pp_mod = open_pp -> open_pp

module type Show_embellish_sig = sig
  val embellishments : string -> open_pp_mod
  val embellishments_functions : string -> Rendering_config.config -> open_pp_mod
end
