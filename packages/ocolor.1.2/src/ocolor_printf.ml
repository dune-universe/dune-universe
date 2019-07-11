(** {1 Printf-friendly functions} *)

(** These functions are useful when trying to print with styles with Printf.
    However, they are a very bad choice. They perform a context-free formatting.
    For instance, there is no way to end the current style and restore the
    previous, like stack-base functions of {!Ocolor_format} do. They just print
    the desired style and keep no track of the current state.

    Thus, it's better to use {!Ocolor_format} as much as possible. Moreover,
    {!Format} provide a nice way of composing printers with %a and
    {!Format.asprintf}. Even if you don't like boxes, {!Format.asprintf} and
    semantic tags are cool.

    So, here are the Printf-friendly functions. Use with care, and preferably,
    not at all.
*)

(** {2 fprintf} *)
(** {3 %a} *)

let apply_style (channel: out_channel) (s: Ocolor_types.style) : unit =
  Printf.fprintf channel "%s" (Ocolor_sgr.sgr_of_style s)

let apply_styles (channel: out_channel) (s: Ocolor_types.style list) : unit =
  Printf.fprintf channel "%s" (Ocolor_sgr.styles_sgr s)

let fg_color4  (channel: out_channel) (c: Ocolor_types.color4) : unit =
  apply_style channel Ocolor_types.(Fg (C4 c))

let fg_color8 (channel: out_channel) (c: Ocolor_types.color8) : unit =
  apply_style channel Ocolor_types.(Fg (C8 c))

let fg_color24 (channel: out_channel)  (c: Ocolor_types.color24) : unit =
  apply_style channel Ocolor_types.(Fg (C24 c))

let bg_color4 (channel: out_channel) (c: Ocolor_types.color4) : unit =
  Printf.fprintf channel "%s" (Ocolor_sgr.bg_color4_sgr c)

let bg_color8 (channel: out_channel) (c: Ocolor_types.color8) : unit =
  Printf.fprintf channel "%s" (Ocolor_sgr.bg_color8_sgr c)

let bg_color24 (channel: out_channel) (c: Ocolor_types.color24) : unit =
  apply_style channel Ocolor_types.(Bg (C24 c))


(** {3 %t} *)

let default_fg (channel: out_channel) : unit =
  apply_style channel Ocolor_types.Default_fg

let fg_rgb (r: int) (g: int) (b: int) (channel: out_channel) : unit =
  apply_style channel Ocolor_types.(Fg (C24 {r24 = r; g24 = g; b24 = b}))

let default_bg (channel: out_channel) : unit =
  Printf.fprintf channel "%s" Ocolor_sgr.default_bg_sgr

let bg_rgb (r: int) (g: int) (b: int) (channel: out_channel) : unit =
  apply_style channel Ocolor_types.(Bg (C24 {r24 = r; g24 = g; b24 = b}))

let reset (channel: out_channel) : unit =
  apply_style channel Ocolor_types.Reset

(** {2 sprintf} *)

(** {3 %a} *)

let s_apply_style () (s: Ocolor_types.style): string =
  Ocolor_sgr.sgr_of_style s

let s_apply_styles () (s: Ocolor_types.style list) : string =
  Ocolor_sgr.styles_sgr s

(** {2 Printf-like} *)
(** Just like printf but perform automatic reset after printing if
    {!Ocolor_config.auto_reset} is set
*)

let fprintf (channel: out_channel) (oc : ('a, out_channel, unit) format) : 'a =
  let reset (channel: out_channel) : unit =
    if Ocolor_config.get_auto_reset () then
      apply_style channel Ocolor_types.Reset
    else
      ()
  in
  Printf.kfprintf reset channel oc

let kfprintf (k: out_channel -> unit) (channel: out_channel) (oc : ('a, out_channel, unit) format) : 'a =
  let reset (channel: out_channel) : unit =
    if Ocolor_config.get_auto_reset () then
      apply_style channel Ocolor_types.Reset;
    k channel
  in
  Printf.kfprintf reset channel oc

let sprintf (oc : ('a, unit, string, string) format4) : 'a =
  let reset (s: string) : string =
    if Ocolor_config.get_auto_reset () then
      s^Ocolor_sgr.reset_sgr
    else
      s
  in
  Printf.ksprintf reset oc

let printf (oc : ('a, out_channel, unit) format) : 'a =
  fprintf stdout oc

let kprintf (k: out_channel -> unit) (oc : ('a, out_channel, unit) format) : 'a =
  kfprintf k stdout oc

module StylingPrettyPrinters =
  Ocolor_pp.BuildPrettyPrinters
    (struct
      type formatter = out_channel
      let fprintf = Printf.fprintf
      let pp_open_styles = apply_styles
      let pp_close_styles channel () = apply_style channel Ocolor_types.Reset
    end)
include StylingPrettyPrinters

module NonStylingPrettyPrinters =
  Ocolor_pp.BuildPrettyPrinters
    (struct
      type formatter = out_channel
      let fprintf = Printf.fprintf
      let pp_open_styles = fun _ _ -> ()
      let pp_close_styles = fun _ _ -> ()
    end)
