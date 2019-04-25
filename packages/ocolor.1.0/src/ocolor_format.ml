type seq = Ocolor_sgr.seq

(* What to do to enable or disable the style. *)
type action =
  | Do of seq (* Apply the sequence. That's all. *)
  | NothingToDo (* The new style is already enabled, so do nothing. *)
  | ResetAndApply (* Reset all attributes and apply the new current attribute. You need to update the stack before. *)

module type STACK =
  (sig
    type elem
    type t

    val init: t
    val push: elem -> t -> t * action
    val pop: t -> t * action (* Pop and yields the previous style, so, the new top of the stack. We don't care about the previous top! *)
    val current_style: t -> Ocolor_types.style option

    val analyze: string -> elem option
  end)

module type STACK_PARAMETERS =
  (sig
    type elem
    val reset: seq
    val seq_of_elem: elem -> seq
    val style_of_elem: elem -> Ocolor_types.style
    val analyze: string -> elem option
  end)

(* Signal when poping from an empty stack. Should never be raised since
   {!Format} ignore closing tags when there are none open. Thus, each pop
   (closing tag) should happen after a push (the corresponding opening tag).
*)
exception AbsurdPop

module MakeStack(P: STACK_PARAMETERS) : STACK with type elem = P.elem =
  (struct
    type elem = P.elem
    type t = elem list

    let init : t = []

    let push (c: elem) (t: t) : t * action =
      match t with
      | hd::_ when c = hd -> c::t, NothingToDo
      | _ -> c::t, Do (P.seq_of_elem c)

    let pop (t: t) : t * action =
      match t with
      | o::t::q when o = t -> t::q, NothingToDo
      | _::t::q -> t::q, Do (P.seq_of_elem t)
      | [_] -> [], Do P.reset
      | [] -> raise AbsurdPop

    let current_style (t: t) : Ocolor_types.style option =
      match t with
      | [] -> None
      | t::_ -> Some (P.style_of_elem t)

    let analyze (s: string) : elem option = P.analyze s
  end)


module type COUNTER_PARAMETERS =
  (sig
    val reset: Ocolor_types.style option
    val style: Ocolor_types.style
    val kw: string list
  end)

module MakeCounter(P: COUNTER_PARAMETERS) : STACK with type elem = unit =
  (struct
    type elem = unit
    type t = int

    let init : t = 0

    let reset : seq option =
      match P.reset with
      | None -> None
      | Some reset -> Some (Ocolor_sgr.seq_of_style reset)

    let style : seq = Ocolor_sgr.seq_of_style P.style

    let push ((): elem) (t: t) : t * action =
      match t with
      | 0 -> 1, Do style
      | n -> n+1, NothingToDo

    let pop (t: t) : t * action =
      match t with
      | 0 -> raise AbsurdPop
      | 1 ->
        let action =
          match reset with
          | Some reset -> Do reset
          | None -> ResetAndApply
        in
        0, action
      | n -> n - 1, NothingToDo

    let current_style (t: t) : Ocolor_types.style option =
      match t with
      | 0 -> None
      | _ -> Some P.style

    let analyze (s: string) : elem option =
      if List.mem s P.kw then
        Some ()
      else
        None
  end)

let color_of_string (s: string) : Ocolor_types.color option =
  match Ocolor_x11.color_of_string s with
  | None -> None
  | Some (r24, g24, b24) -> Some Ocolor_types.(C24 {r24; g24; b24})

let analyze_fg (s: string) : Ocolor_types.color option =
  match s with
  | "black" -> Some Ocolor_types.(C4 black)
  | "red" -> Some Ocolor_types.(C4 red)
  | "green" -> Some Ocolor_types.(C4 green)
  | "blue" -> Some Ocolor_types.(C4 blue)
  | "yellow" -> Some Ocolor_types.(C4 yellow)
  | "magenta" -> Some Ocolor_types.(C4 magenta)
  | "cyan" -> Some Ocolor_types.(C4 cyan)
  | "white" -> Some Ocolor_types.(C4 white)
  | "hi_black" -> Some Ocolor_types.(C4 hi_black)
  | "hi_red" -> Some Ocolor_types.(C4 hi_red)
  | "hi_green" -> Some Ocolor_types.(C4 hi_green)
  | "hi_blue" -> Some Ocolor_types.(C4 hi_blue)
  | "hi_yellow" -> Some Ocolor_types.(C4 hi_yellow)
  | "hi_magenta" -> Some Ocolor_types.(C4 hi_magenta)
  | "hi_cyan" -> Some Ocolor_types.(C4 hi_cyan)
  | "hi_white" -> Some Ocolor_types.(C4 hi_white)
  | _ ->
    match color_of_string s with
    | Some c -> Some c
    | None ->
      try
        Scanf.sscanf s "x11_%s" (fun s -> color_of_string s)
      with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ ->
      try
        Scanf.sscanf s "c6(%d,%d,%d)" (fun r6 g6 b6 -> Some Ocolor_types.(C8 (Cube6 {r6;g6;b6})))
      with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ ->
      try
        Scanf.sscanf s "gs(%d)" (fun gs -> Some Ocolor_types.(C8 (Grayscale gs)))
      with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ ->
      try
        Scanf.sscanf s "rgb(%d,%d,%d)" (fun r24 g24 b24 -> Some Ocolor_types.(C24 {r24;g24;b24}))
      with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ ->
      try
        Scanf.sscanf s "rgb(0x%x,0x%x,0x%x)" (fun r24 g24 b24 -> Some Ocolor_types.(C24 {r24;g24;b24}))
      with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ -> None

let analyze_bg (s: string) : Ocolor_types.color option =
  try
    Scanf.sscanf s "bg_%s" (fun c -> analyze_fg c)
  with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ -> None

let analyze_underlined (s: string) : bool option =
  match s with
  | "ul"
  | "under"
  | "underlined" -> Some false
  | "uul" | "dul"
  | "uunder" | "dunderlined"
  | "doubleunderlined" -> Some true
  | _ -> None

let analyze_font (s: string) : int option =
  try
    Scanf.sscanf s "font(%d)" (fun i -> Some i)
  with Scanf.Scan_failure _ | End_of_file | Failure _ | Invalid_argument _ -> None

module Fg_color_stack = MakeStack (struct type elem = Ocolor_types.color let reset = Ocolor_sgr.default_fg_seq let seq_of_elem = Ocolor_sgr.fg_color_seq let analyze = analyze_fg let style_of_elem c = Ocolor_types.Fg c end)
module Bg_color_stack = MakeStack (struct type elem = Ocolor_types.color let reset = Ocolor_sgr.default_bg_seq let seq_of_elem = Ocolor_sgr.bg_color_seq let analyze = analyze_bg let style_of_elem c = Ocolor_types.Bg c end)
module Font_stack = MakeStack (struct type elem = int let reset = Ocolor_sgr.default_font_seq let seq_of_elem = Ocolor_sgr.font_seq let analyze = analyze_font let style_of_elem i = Ocolor_types.Font i end)
module Underlined_stack = MakeStack(struct type elem = bool let reset = Ocolor_sgr.underlined_off_seq let seq_of_elem b = if b then Ocolor_sgr.double_underlined_seq else Ocolor_sgr.underlined_seq let analyze = analyze_underlined let style_of_elem b = if b then Ocolor_types.DoubleUnderlined else Ocolor_types.Underlined end)

module Bold_counter = MakeCounter(struct let style = Ocolor_types.Bold let reset = None let kw = ["b"; "bold"] end)
module Faint_counter = MakeCounter(struct let style = Ocolor_types.Faint let reset = None let kw = ["faint"] end)
module Blink_counter = MakeCounter(struct let style = Ocolor_types.Blink let reset = Some Ocolor_types.Blink_off let kw = ["blink"] end)
module Conceal_counter = MakeCounter(struct let style = Ocolor_types.Conceal let reset = Some Ocolor_types.Conceal_off let kw = ["conceal"] end)
module Fraktur_counter = MakeCounter(struct let style = Ocolor_types.Fraktur let reset = None let kw = ["frak"; "fraktur"] end)
module Reverse_counter = MakeCounter(struct let style = Ocolor_types.Reverse_video let reset = Some Ocolor_types.Reverse_video_off let kw = ["reverse"] end)
module Crossed_counter = MakeCounter(struct let style = Ocolor_types.Crossed_out let reset = Some Ocolor_types.Crossed_out_off let kw = ["crossed"] end)
module Framed_counter = MakeCounter(struct let style = Ocolor_types.Framed let reset = None let kw = ["framed"] end)
module Italic_counter = MakeCounter(struct let style = Ocolor_types.Italic let reset = None let kw = ["italic"; "it"] end)
module Encircled_counter = MakeCounter(struct let style = Ocolor_types.Encircled let reset = None let kw = ["encircled"] end)
module Overlined_counter = MakeCounter(struct let style = Ocolor_types.Overlined let reset = Some Ocolor_types.Overlined_off let kw = ["ol"; "over"; "overlined"] end)

type handled_stack = Opaque_stack : (module STACK with type t = 'a) * 'a -> handled_stack

type formatter =
  {
    fmt: Format.formatter;
    mutable stacks: handled_stack list;
  }

let init_stacks : handled_stack list =
  [
    Opaque_stack((module Fg_color_stack), Fg_color_stack.init);
    Opaque_stack((module Bg_color_stack), Bg_color_stack.init);
    Opaque_stack((module Font_stack), Font_stack.init);
    Opaque_stack((module Underlined_stack), Underlined_stack.init);
    Opaque_stack((module Bold_counter), Bold_counter.init);
    Opaque_stack((module Faint_counter), Faint_counter.init);
    Opaque_stack((module Blink_counter), Blink_counter.init);
    Opaque_stack((module Conceal_counter), Conceal_counter.init);
    Opaque_stack((module Fraktur_counter), Fraktur_counter.init);
    Opaque_stack((module Reverse_counter), Reverse_counter.init);
    Opaque_stack((module Crossed_counter), Crossed_counter.init);
    Opaque_stack((module Framed_counter), Framed_counter.init);
    Opaque_stack((module Italic_counter), Italic_counter.init);
    Opaque_stack((module Encircled_counter), Encircled_counter.init);
    Opaque_stack((module Overlined_counter), Overlined_counter.init);
  ]

let current_styles (fmt: formatter) : Ocolor_types.style list =
  let rec aux (l: handled_stack list) : Ocolor_types.style list =
    match l with
    | [] -> []
    | Opaque_stack(m, s)::q ->
      let module M = (val m) in
      match M.current_style s with
      | None -> aux q
      | Some style -> style::aux q
  in
  aux fmt.stacks

let current_state (fmt: formatter) : seq =
  fmt |> current_styles |> Ocolor_sgr.seq_of_styles

let get_current_fg_color (fmt: formatter) : Ocolor_types.color option =
  let exception Found of Ocolor_types.color in
  try
    let () = List.iter ((function Ocolor_types.Fg c -> raise (Found c) | _ -> () )[@warning "-4"]) (current_styles fmt) in
    None
  with Found c -> Some c

let get_current_bg_color (fmt: formatter) : Ocolor_types.color option =
  let exception Found of Ocolor_types.color in
  try
    let () = List.iter ((function Ocolor_types.Bg c -> raise (Found c) | _ -> () )[@warning "-4"]) (current_styles fmt) in
    None
  with Found c -> Some c

let mark_open_tag (fmt: formatter) (tag: Format.tag) : string =
  let mark_open_tag (tag: Format.tag) : string =
    let rec aux (l: handled_stack list) : action * handled_stack list =
      match l with
      | [] -> NothingToDo, []
      | Opaque_stack(m, s)::q ->
        let module M = (val m) in
        match M.analyze tag with
        | None -> let sgr, l = aux q in sgr, Opaque_stack(m, s)::l
        | Some elem ->
          let s, sgr = M.push elem s in
          sgr, Opaque_stack(m, s)::q
    in
    let seq, new_stacks = aux fmt.stacks in
    let () = fmt.stacks <- new_stacks in
    match seq with
    | Do seq -> Ocolor_sgr.sgr_of_seq seq
    | ResetAndApply -> Ocolor_sgr.reset_seq @ current_state fmt |> Ocolor_sgr.sgr_of_seq
    | NothingToDo -> ""
  in
  let tags = String.split_on_char ';' tag in
  String.concat "" (List.map mark_open_tag tags)

let mark_close_tag (fmt: formatter) (tag: Format.tag) : string =
  let mark_close_tag (tag: Format.tag) : string =
    let rec aux (l: handled_stack list) : action * handled_stack list =
      match l with
      | [] -> NothingToDo, []
      | Opaque_stack(m, s)::q ->
        let module M = (val m) in
        match M.analyze tag with
        | None -> let sgr, l = aux q in sgr, Opaque_stack(m, s)::l
        | Some _ ->
          let s, sgr = M.pop s in
          sgr, Opaque_stack(m, s)::q
    in
    let seq, new_stacks = aux fmt.stacks in
    let () = fmt.stacks <- new_stacks in
    match seq with
    | Do seq -> Ocolor_sgr.sgr_of_seq seq
    | ResetAndApply -> (Ocolor_sgr.reset_seq) @ (current_state fmt) |> Ocolor_sgr.sgr_of_seq
    | NothingToDo -> ""
  in
  let tags = String.split_on_char ';' tag in
  String.concat "" (List.map mark_close_tag tags)

let make_formatter (fmt: Format.formatter) : formatter =
  let Format.{print_open_tag; print_close_tag; _} = Format.pp_get_formatter_tag_functions fmt () in
  let stacks = init_stacks in
  let fmt = {fmt; stacks} in
  let mark_open_tag = mark_open_tag fmt in
  let mark_close_tag = mark_close_tag fmt in
  let () = Format.pp_set_formatter_tag_functions fmt.fmt Format.{mark_open_tag; mark_close_tag; print_open_tag; print_close_tag} in
  let () = Format.pp_set_mark_tags fmt.fmt true in
  fmt

let prettify_formatter (fmt: Format.formatter) : unit =
  fmt |> make_formatter |> ignore

let formatter_of_buffer (b: Buffer.t) : formatter =
  let fmt = Format.formatter_of_buffer b in
  make_formatter fmt

let raw_formatter_of_buffer (b: Buffer.t) : Format.formatter =
  let fmt = Format.formatter_of_buffer b in
  let () = prettify_formatter fmt in
  fmt

let std_formatter : formatter =
  stdout |> Format.formatter_of_out_channel |> make_formatter

let err_formatter : formatter =
  stderr |> Format.formatter_of_out_channel |> make_formatter

let unwrap_formatter (fmt: formatter) : Format.formatter =
  fmt.fmt

let raw_std_formatter : Format.formatter =
  unwrap_formatter std_formatter

let raw_err_formatter : Format.formatter =
  unwrap_formatter err_formatter

let printf (type a) (f: (a, Format.formatter, unit) format) : a =
  Format.fprintf raw_std_formatter f

let eprintf (type a) (f: (a, Format.formatter, unit) format) : a =
  Format.fprintf raw_err_formatter f

let kasprintf (type a b) (k: string -> a) (f: (b, Format.formatter, unit, a) format4) : b =
  let buf = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buf in
  let () = prettify_formatter fmt in
  Format.kfprintf (fun fmt -> Format.pp_print_flush fmt (); buf |> Buffer.contents |> k) fmt f

let asprintf (type a) (f: (a, Format.formatter, unit, string) format4) : a =
  kasprintf (fun s -> s) f

let pp_print_flush (fmt: formatter) () : unit =
  Format.pp_print_flush fmt.fmt ()

let rec string_of_color (c: Ocolor_types.color) : string =
  let open Ocolor_types in
  match c with
  | C4 ({intensity4 = true; _} as c) -> Format.sprintf "hi_%s" (string_of_color (C4 {c with intensity4 = false}))
  | C4 {r4 = false; g4 = false; b4 = false; _} -> "black"
  | C4 {r4 = false; g4 = false; b4 = true ; _} -> "blue"
  | C4 {r4 = false; g4 = true; b4 = false; _} -> "green"
  | C4 {r4 = false; g4 = true; b4 = true; _} -> "cyan"
  | C4 {r4 = true ; g4 = false; b4 = false; _} -> "red"
  | C4 {r4 = true ; g4 = false; b4 = true ; _} -> "magenta"
  | C4 {r4 = true ; g4 = true; b4 = false; _} -> "yellow"
  | C4 {r4 = true ; g4 = true; b4 = true; _} -> "white"
  | C8 (Standard c) -> string_of_color (C4 c)
  | C8 (Cube6 {r6;g6;b6}) -> Format.asprintf "c6(%d,%d,%d)" r6 g6 b6
  | C8 (Grayscale gs) -> Format.asprintf "gs(%d)" gs
  | C24 {r24; g24; b24} -> Format.asprintf "rgb(%d,%d,%d)" r24 g24 b24

let string_of_style (l: Ocolor_types.style) : string =
  let open Ocolor_types in
  match l with
  | Bold -> "bold"
  | Faint -> "faint"
  | Italic -> "it"
  | Underlined -> "ul"
  | DoubleUnderlined -> "uul"
  | Blink -> "blink"
  | Fg c -> string_of_color c
  | Bg c -> "bg_"^(string_of_color c)
  | Conceal -> "conceal"
  | Fraktur -> "frak"
  | Reverse_video -> "reverse"
  | Crossed_out -> "crossed"
  | Framed -> "framed"
  | Encircled -> "encircled"
  | Overlined -> "overlined"
  | Font i -> Format.asprintf "font(%d)" i
  (* Off *)
  | Blink_off
  | Overlined_off
  | Italic_fraktur_off
  | Faint_bold_off
  | Underlined_off
  | Reverse_video_off
  | Framed_encircled_off
  | Conceal_off
  | Crossed_out_off
  | Reset
  | Default_font
  | Default_fg
  | Default_bg
    -> ""

let string_of_styles (l: Ocolor_types.style list) : string =
  List.map string_of_style l |> String.concat ";"

let pp_open_styles (fmt: Format.formatter) (l: Ocolor_types.style list) : unit =
  string_of_styles l |> Format.pp_open_tag fmt

let pp_open_style (fmt: Format.formatter) (s: Ocolor_types.style) : unit =
  string_of_styles [s] |> Format.pp_open_tag fmt

let pp_close_styles (fmt: Format.formatter) ((): unit) : unit =
  Format.pp_close_tag fmt ()

let pp_close_style (fmt: Format.formatter) ((): unit) : unit =
  Format.pp_close_tag fmt ()

let pp_bool_generic
    ?(false_style: Ocolor_types.style list=Ocolor_types.[Bold;Fg (C4 red)])
    ?(true_style: Ocolor_types.style list=Ocolor_types.[Bold;Fg (C4 green)])
    (fmt: Format.formatter) (b: bool) : unit =
  let style =
    if b then
      string_of_styles true_style
    else
      string_of_styles false_style
  in
  Format.fprintf fmt "%a%b%a"
    Format.pp_open_tag style b Format.pp_close_tag ()


let pp_bool (fmt: Format.formatter) (b: bool) : unit =
  pp_bool_generic fmt b

let pp_list_generic
    ?(left: string="[") ?(sep: string="; ") ?(right: string="]")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a)
    (p: Format.formatter -> a -> unit) (fmt: Format.formatter) (l: a list)
  : unit =
  let delim_style = string_of_styles delim_style in
  let sep_style = string_of_styles sep_style in
  let elem_style = string_of_styles elem_style in
  let open Format in
  let () = fprintf fmt "%a%s%a" pp_open_tag delim_style left pp_close_tag () in
  match l with
  | [] -> fprintf fmt "%a%s%a" pp_open_tag delim_style right pp_close_tag ()
  | [e] -> fprintf fmt "%a%a%a%a%s%a" pp_open_tag elem_style p e pp_close_tag () pp_open_tag delim_style right pp_close_tag ()
  | t::q ->
    let () =
      fprintf fmt "%a%a%a" pp_open_tag elem_style p t pp_close_tag ()
    in
    let () =
      List.iter
        (fun e ->
           fprintf fmt "%a%s%a%a%a%a" pp_open_tag sep_style sep pp_close_tag () pp_open_tag elem_style p e pp_close_tag ();
        )
        q
    in
    fprintf fmt "%a%s%a" pp_open_tag delim_style right pp_close_tag ()

let pp_list p fmt l = pp_list_generic p fmt l

let pp_option_generic
    ?(none: string="None")
    ?(none_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(some_style: Ocolor_types.style list=[])
    (type a)
    (p: Format.formatter -> a -> unit) (fmt: Format.formatter) (o: a option)
  : unit =
  let open Format in
  match o with
  | None ->
    let none_style = string_of_styles none_style in
    fprintf fmt "%a%s%a" pp_open_tag none_style none pp_close_tag ()
  | Some o ->
    let some_style = string_of_styles some_style in
    fprintf fmt "%a%a%a" pp_open_tag some_style p o pp_close_tag ()

let pp_option p fmt o = pp_option_generic p fmt o

let pp_pair_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a b)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (fmt: Format.formatter)
    (a, b : a * b)
  : unit =
  let delim_style = string_of_styles delim_style in
  let sep_style = string_of_styles sep_style in
  let elem_style = string_of_styles elem_style in
  let l_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style left Format.pp_close_tag ()
  in
  let r_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style right Format.pp_close_tag ()
  in
  let sep (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag sep_style sep Format.pp_close_tag ()
  in
  let elem (type a) (p: Format.formatter -> a -> unit) (fmt: Format.formatter) (a: a) : unit =
    Format.fprintf fmt "%a%a%a"
      Format.pp_open_tag elem_style p a Format.pp_close_tag ()
  in
  Format.fprintf fmt "%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b r_delim

let pp_pair
    (type a b)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (fmt: Format.formatter)
    (p : a * b)
  : unit =
  pp_pair_generic f g fmt p

let pp_3_tuple_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a b c)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (h: Format.formatter -> c -> unit)
    (fmt: Format.formatter)
    (a, b, c : a * b * c)
  : unit =
  let delim_style = string_of_styles delim_style in
  let sep_style = string_of_styles sep_style in
  let elem_style = string_of_styles elem_style in
  let l_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style left Format.pp_close_tag ()
  in
  let r_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style right Format.pp_close_tag ()
  in
  let sep (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag sep_style sep Format.pp_close_tag ()
  in
  let elem (type a) (p: Format.formatter -> a -> unit) (fmt: Format.formatter) (a: a) : unit =
    Format.fprintf fmt "%a%a%a"
      Format.pp_open_tag elem_style p a Format.pp_close_tag ()
  in
  Format.fprintf fmt "%t%a%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b sep (elem h) c r_delim

let pp_3_tuple (type a b c)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (h: Format.formatter -> c -> unit)
    (fmt: Format.formatter)
    (t : a * b * c)
  : unit =
  pp_3_tuple_generic f g h fmt t

let pp_4_tuple_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a b c d)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (h: Format.formatter -> c -> unit)
    (i: Format.formatter -> d -> unit)
    (fmt: Format.formatter)
    (a, b, c, d : a * b * c * d)
  : unit =
  let delim_style = string_of_styles delim_style in
  let sep_style = string_of_styles sep_style in
  let elem_style = string_of_styles elem_style in
  let l_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style left Format.pp_close_tag ()
  in
  let r_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style right Format.pp_close_tag ()
  in
  let sep (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag sep_style sep Format.pp_close_tag ()
  in
  let elem (type a) (p: Format.formatter -> a -> unit) (fmt: Format.formatter) (a: a) : unit =
    Format.fprintf fmt "%a%a%a"
      Format.pp_open_tag elem_style p a Format.pp_close_tag ()
  in
  Format.fprintf fmt "%t%a%t%a%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b sep (elem h) c sep (elem i) d r_delim

let pp_4_tuple (type a b c d)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (h: Format.formatter -> c -> unit)
    (i: Format.formatter -> d -> unit)
    (fmt: Format.formatter)
    (q : a * b * c * d)
  : unit =
  pp_4_tuple_generic f g h i fmt q

let pp_5_tuple_generic
    ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
    ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
    ?(elem_style: Ocolor_types.style list=[])
    (type a b c d e)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (h: Format.formatter -> c -> unit)
    (i: Format.formatter -> d -> unit)
    (j: Format.formatter -> e -> unit)
    (fmt: Format.formatter)
    (a, b, c, d, e : a * b * c * d * e)
  : unit =
  let delim_style = string_of_styles delim_style in
  let sep_style = string_of_styles sep_style in
  let elem_style = string_of_styles elem_style in
  let l_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style left Format.pp_close_tag ()
  in
  let r_delim (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag delim_style right Format.pp_close_tag ()
  in
  let sep (fmt: Format.formatter) : unit =
    Format.fprintf fmt "%a%s%a"
      Format.pp_open_tag sep_style sep Format.pp_close_tag ()
  in
  let elem (type a) (p: Format.formatter -> a -> unit) (fmt: Format.formatter) (a: a) : unit =
    Format.fprintf fmt "%a%a%a"
      Format.pp_open_tag elem_style p a Format.pp_close_tag ()
  in
  Format.fprintf fmt "%t%a%t%a%t%a%t%a%t%a%t"
    l_delim (elem f) a sep (elem g) b sep (elem h) c sep (elem i) d sep (elem j) e r_delim

let pp_5_tuple (type a b c d e)
    (f: Format.formatter -> a -> unit)
    (g: Format.formatter -> b -> unit)
    (h: Format.formatter -> c -> unit)
    (i: Format.formatter -> d -> unit)
    (j: Format.formatter -> e -> unit)
    (fmt: Format.formatter)
    (q: a * b * c * d * e)
  : unit =
  pp_5_tuple_generic f g h i j fmt q
