open Gg
open Vg

module Font : sig
  type t

  val name : t -> string
  val data : t -> string
  val ascender : t -> float
  val descender : t -> float
  val xmin : t -> float
  val ymin : t -> float
  val xmax : t -> float
  val ymax : t -> float

  val load_from_string :
    string ->
    (t, [> Otfm.error]) result

  val load_from_file :
    string ->
    (t, [> Otfm.error | `Read_error of string]) result
end

module Layout : sig
  type t
  val make : Font.t -> size:float -> string -> t
  val width : t -> float
  val maxy : t -> float
  val miny : t -> float
end

(** [cut ?col ?size font text] returns an image displaying [text] with
   color [col] and size [size] using font [font], as well as a
   bounding box for the text in the image. The actual bounding box is
   driver-dependent but the result of this function should provide a
   reasonable appromixation. *)
val cut :
  ?col:Color.t ->
  Layout.t ->
  image
