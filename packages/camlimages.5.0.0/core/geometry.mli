type size = Scale of float | Pixel of int | Guess
type aspect_opts = Keep_at_most | Keep_at_least | Dont_keep
type resize_switch = Always | Bigger_only | Smaller_only
type from = TopLeft | BottomRight | Center
type position = AtPixel of from * int | AtScale of from * float
type t = { geom_width : int; geom_height : int; geom_x : int; geom_y : int; }
type spec = {
  spec_width : size;
  spec_height : size;
  spec_aspect : aspect_opts;
  spec_switch : resize_switch;
  spec_x : int;
  spec_y : int;
}
val compute : spec -> int -> int -> t
