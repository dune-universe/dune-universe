 (** Histgrapm of colors *)

type t = int array
val create : unit -> int array
val total_samples : int array -> int
val store_sample : int array -> Color.rgb -> unit
val normalize : float -> int array -> Color.rgb -> Color.rgb
val gamma :
  float ->
  < height : int; unsafe_get : int -> int -> Color.rgb; width : int; .. > ->
  OImages.rgb24
val filter :
  ('a -> int array -> Color.rgb -> Color.rgb) ->
  'a ->
  < height : int; unsafe_get : int -> int -> Color.rgb; width : int; .. > ->
  OImages.rgb24
