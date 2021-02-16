(** Use Mlpost figures inside gtk interface. *)

open Mlpost

type auto_aspect =
  width:Num.t -> height:Num.t -> Mlpost.Picture.t -> Mlpost.Transform.t

val aa_nothing : auto_aspect

val aa_center : auto_aspect

val aa_fit_page : auto_aspect

val aa_fit_width : auto_aspect

val aa_fit_height : auto_aspect

(** GTK widget which displays an mlpost picture. *)
class mlpost_pic :
  ?width:int
  -> ?height:int
  -> ?packing:(GObj.widget -> unit)
  -> ?show:bool
  -> unit
  -> object
       inherit GObj.widget

       val obj : Gtk.widget Gtk.obj

       method pic : Mlpost.Picture.t
       (** The displayed picture *)

       method set_pic : Mlpost.Picture.t -> unit
       (** Sets the picture to display. This function doesn't refresh
       the widget. *)

       method background : GDraw.color
       (** The actual background color *)

       method set_background : GDraw.color -> unit
       (** Sets the background color *)

       method size : int * int
       (** The size of the drawing area (width,height) *)

       method set_auto_aspect : auto_aspect -> unit
       (** define the transformation used to have a good aspect of the
        picture (centered, ...) *)

       method set_show_corner : bool -> unit
     end

module Interface : sig
  (** {1 Abstract lablgtk in order to display Mlpost figures inside a very
      simple interface} *)

  type interface
  (** An interface is composed by one control window and by some
        display window *)

  val new_interface :
    ?width:int -> ?height:int -> ?title:string -> unit -> interface
  (** create a new interface with an empty control window *)

  (** {2 Interfaces} *)

  val create_text :
    interface -> ?label:string -> string -> (string -> unit) -> unit
  (** [create_text ~label get set] adds to the control window a text
        input. [get] is the initial value, [set] is called each
        times the value of the text input is changed. *)

  val create_option :
    interface -> ?label:string -> (string * (unit -> unit)) list -> unit
  (** [create_option ~label value_list] adds to the control window a
        radio menu item. [value_list] is a pair of one of the choice and
        the callback function used when this choice is selected. *)

  val remove_pic : interface -> (unit -> Mlpost.Command.t) -> unit
  (** [remove_pic gen_pic] removes a display window created by
        [add_pic gen_pic] *)

  (** {2 Required function} *)

  (** functions needed to see one mlpost picure : *)

  val add_pic :
    interface ->
    ?width:int ->
    ?height:int ->
    ?title:string ->
    ?show_corner:bool ->
    ?auto_aspect:auto_aspect ->
    (unit -> Mlpost.Command.t) ->
    unit
  (** [add_pic get_pic] add a new display window. [get_pic] is
        called each times the window must be refreshed. If the value
        of one of the interfaces is changed, the displayed picure is
        refreshed.*)

  val main : interface -> unit
  (** Start the main loop. During the main loop some texts or
        options can be added and {!add_pic} can be called *)
end
