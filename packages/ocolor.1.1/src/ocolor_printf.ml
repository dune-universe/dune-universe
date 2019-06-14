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

(** {2 Handy printers} *)

let pp_bool_generic
    ?(false_style: Ocolor_types.style list=Ocolor_types.[Bold;Fg (C4 red)])
    ?(true_style: Ocolor_types.style list=Ocolor_types.[Bold;Fg (C4 green)])
    (oc: out_channel) (b: bool) : unit =
  let style =
    if b then
      true_style
    else
      false_style
  in
  Printf.fprintf oc "%a%b%t" apply_styles style b reset

let pp_bool(oc: out_channel) (b: bool) : unit =
  pp_bool_generic oc b

let pp_list_generic (type a) ?(left="[") ?(sep="; ") ?(right="]")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style=[])
    (p: out_channel -> a -> unit) (oc: out_channel) (l: a list)
  : unit =
  let () = Printf.fprintf oc "%a%s%t" apply_styles delim_style left reset in
  match l with
  | [] -> Printf.fprintf oc "%a%s%t" apply_styles delim_style right reset
  | [e] -> Printf.fprintf oc "%a%a%t%a%s%t" apply_styles elem_style p e reset apply_styles delim_style right reset
  | t::q ->
    let () =
      Printf.fprintf oc "%a%a%t" apply_styles elem_style p t reset
    in
    let () =
      List.iter
        (fun e ->
           Printf.fprintf oc "%a%s%t%a%a%t" apply_styles sep_style sep reset apply_styles elem_style p e reset;
        )
        q
    in
    fprintf oc "%a%s%t" apply_styles delim_style right reset

let pp_list p oc l = pp_list_generic p oc l

let pp_option_generic (type a) ?(none:string="None")
    ?(none_style: Ocolor_types.style list=Ocolor_types.[Faint; Fg (C4 hi_black)])
    ?(some_style: Ocolor_types.style list=[])
    (p: out_channel -> a -> unit) (oc: out_channel) (o: a option)
  : unit =
  match o with
  | None -> Printf.fprintf oc "%a%s%t" apply_styles none_style none reset
  | Some o -> Printf.fprintf oc "%a%a%t" apply_styles some_style p o reset

let pp_option p oc o = pp_option_generic p oc o

let pp_pair_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a) (type b)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (oc: out_channel)
    (a, b : a * b)
  : unit =
  let l_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t" apply_styles delim_style left reset
  in
  let r_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t" apply_styles delim_style right reset
  in
  let sep (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t" apply_styles sep_style sep reset
  in
  let elem (type a) (p: out_channel -> a -> unit) (oc: out_channel) (a: a) : unit =
    Printf.fprintf oc "%a%a%t" apply_styles elem_style p a reset
  in
  Printf.fprintf oc "%t%a%t%a%t" l_delim (elem f) a sep (elem g) b r_delim

let pp_pair
    (type a) (type b)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (oc: out_channel)
    (p : a * b)
  : unit =
  pp_pair_generic f g oc p

let pp_3_tuple_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a) (type b) (type c)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (h: out_channel -> c -> unit)
    (oc: out_channel)
    (a, b, c : a * b * c)
  : unit =
  let l_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t" apply_styles delim_style left reset
  in
  let r_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t" apply_styles delim_style right reset
  in
  let sep (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t" apply_styles sep_style sep reset
  in
  let elem (type a) (p: out_channel -> a -> unit) (oc: out_channel) (a: a) : unit =
    Printf.fprintf oc "%a%a%t" apply_styles elem_style p a reset
  in
  Printf.fprintf oc "%t%a%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b sep (elem h) c r_delim

let pp_3_tuple (type a) (type b) (type c)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (h: out_channel -> c -> unit)
    (oc: out_channel)
    (t : a * b * c)
  : unit =
  pp_3_tuple_generic f g h oc t

let pp_4_tuple_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a) (type b) (type c) (type d)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (h: out_channel -> c -> unit)
    (i: out_channel -> d -> unit)
    (oc: out_channel)
    (a, b, c, d : a * b * c * d)
  : unit =
  let l_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t"
      apply_styles delim_style left reset
  in
  let r_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t"
      apply_styles delim_style right reset
  in
  let sep (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t"
      apply_styles sep_style sep reset
  in
  let elem (type a) (p: out_channel -> a -> unit) (oc: out_channel) (a: a) : unit =
    Printf.fprintf oc "%a%a%t" apply_styles elem_style p a reset
  in
  Printf.fprintf oc "%t%a%t%a%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b sep (elem h) c sep (elem i) d r_delim

let pp_4_tuple (type a) (type b) (type c) (type d)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (h: out_channel -> c -> unit)
    (i: out_channel -> d -> unit)
    (oc: out_channel)
    (q : a * b * c * d)
  : unit =
  pp_4_tuple_generic f g h i oc q

let pp_5_tuple_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a) (type b) (type c) (type d) (type e)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (h: out_channel -> c -> unit)
    (i: out_channel -> d -> unit)
    (j: out_channel -> e -> unit)
    (oc: out_channel)
    (a, b, c, d, e : a * b * c * d * e)
  : unit =
  let l_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t"
      apply_styles delim_style left reset
  in
  let r_delim (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t"
      apply_styles delim_style right reset
  in
  let sep (oc: out_channel) : unit =
    Printf.fprintf oc "%a%s%t"
      apply_styles sep_style sep reset
  in
  let elem (type a) (p: out_channel -> a -> unit) (oc: out_channel) (a: a) : unit =
    Printf.fprintf oc "%a%a%t" apply_styles elem_style p a reset
  in
  Printf.fprintf oc "%t%a%t%a%t%a%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b sep (elem h) c sep (elem i) d sep (elem j) e r_delim

let pp_5_tuple (type a) (type b) (type c) (type d) (type e)
    (f: out_channel -> a -> unit)
    (g: out_channel -> b -> unit)
    (h: out_channel -> c -> unit)
    (i: out_channel -> d -> unit)
    (j: out_channel -> e -> unit)
    (oc: out_channel)
    (q: a * b * c * d * e)
  : unit =
  pp_5_tuple_generic f g h i j oc q
