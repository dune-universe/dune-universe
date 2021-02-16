type multi_page_pic = {
  pic : Cairo.context;
  x_origin : float;
  y_origin : float;
}

val draw : multi_page_pic -> Dviinterp.command list -> unit
