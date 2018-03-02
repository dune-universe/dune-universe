val load : string -> Images.load_option list -> OImages.oimage
val load_thumbnail :
  string ->
  Images.load_option list -> Geometry.spec -> int * int * OImages.oimage
val save : string -> Images.save_option list -> OImages.oimage -> unit
val save_as_cmyk :
  string ->
  Images.save_option list ->
  (Images.rgb -> int * int * int * int) -> OImages.oimage -> unit
