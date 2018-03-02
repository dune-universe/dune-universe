val load_as_rgb24 : string -> Images.load_option list -> OImages.oimage
val load : string -> Images.load_option list -> OImages.oimage
val save : string -> Images.save_option list -> < image : Images.t; .. > -> unit
