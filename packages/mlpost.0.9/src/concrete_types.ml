type scolor =
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | Gray of float

type color = OPAQUE of scolor | TRANSPARENT of float * scolor
