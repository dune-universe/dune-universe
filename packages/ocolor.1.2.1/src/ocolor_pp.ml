module type PRETTY_PRINTERS =
  (sig
    type formatter
    type 'a pp = formatter -> 'a -> unit

    val pp_bool_generic:
      ?false_style:Ocolor_types.style list ->
      ?true_style:Ocolor_types.style list ->
      bool pp
    val pp_bool: bool pp

    val pp_option_generic:
      ?none:string ->
      ?none_style:Ocolor_types.style list ->
      ?some_style:Ocolor_types.style list ->
      'a pp -> 'a option pp
    val pp_option: 'a pp -> 'a option pp

    val pp_pair_generic:
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?elem_style:Ocolor_types.style list ->
      'a pp -> 'b pp -> ('a * 'b) pp
    val pp_pair: 'a pp -> 'b pp -> ('a * 'b) pp

    val pp_3_tuple_generic:
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?elem_style:Ocolor_types.style list ->
      'a pp -> 'b pp -> 'c pp -> ('a * 'b * 'c) pp
    val pp_3_tuple: 'a pp -> 'b pp -> 'c pp -> ('a * 'b * 'c) pp

    val pp_4_tuple_generic:
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?elem_style:Ocolor_types.style list ->
      'a pp -> 'b pp -> 'c pp -> 'd pp -> ('a * 'b * 'c * 'd) pp
    val pp_4_tuple: 'a pp -> 'b pp -> 'c pp -> 'd pp -> ('a * 'b * 'c * 'd) pp

    val pp_5_tuple_generic:
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?elem_style:Ocolor_types.style list ->
      'a pp -> 'b pp -> 'c pp -> 'd pp -> 'e pp -> ('a * 'b * 'c * 'd * 'e) pp
    val pp_5_tuple: 'a pp -> 'b pp -> 'c pp -> 'd pp -> 'e pp -> ('a * 'b * 'c * 'd * 'e) pp

    val pp_iterable_generic:
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?elem_style:Ocolor_types.style list ->
      (('a -> unit) -> 'b -> unit) ->
      'a pp -> 'b pp
    val pp_iterable: (('a -> unit) -> 'b -> unit) -> 'a pp -> 'b pp

    val pp_list_generic:
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?elem_style:Ocolor_types.style list ->
      'a pp -> 'a list pp
    val pp_list: 'a pp -> 'a list pp

    val pp_iterable_mapping_more_generic :
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      (('a -> 'b -> unit) -> 'c -> unit) ->
      ('a * 'b) pp -> 'c pp
    val pp_iterable_mapping_generic :
      ?left:string ->
      ?sep:string ->
      ?right:string ->
      ?mapsto:string ->
      ?delim_style:Ocolor_types.style list ->
      ?sep_style:Ocolor_types.style list ->
      ?mapsto_style:Ocolor_types.style list ->
      ?key_style:Ocolor_types.style list ->
      ?value_style:Ocolor_types.style list ->
      (('a -> 'b -> unit) -> 'c -> unit) ->
      'a pp -> 'b pp -> 'c pp
    val pp_iterable_mapping :
      (('a -> 'b -> unit) -> 'c -> unit) ->
      'a pp -> 'b pp -> 'c pp

  end)

module BuildPrettyPrinters
    (PP: sig
       type formatter
       val fprintf: formatter -> ('a, formatter, unit) format -> 'a
       val pp_open_styles: formatter -> Ocolor_types.style list -> unit
       val pp_close_styles: formatter -> unit -> unit
     end
    )
  : PRETTY_PRINTERS
    with type formatter := PP.formatter
  =
  (struct
    include PP
    type 'a pp = PP.formatter -> 'a -> unit

    let pp_bool_generic
        ?(false_style: Ocolor_types.style list=Ocolor_types.[Bold;Fg (C4 red)])
        ?(true_style: Ocolor_types.style list=Ocolor_types.[Bold;Fg (C4 green)])
        (fmt: formatter) (b: bool) : unit =
      let style =
        if b then
          true_style
        else
          false_style
      in
      fprintf fmt "%a%b%a"
        pp_open_styles style b pp_close_styles ()


    let pp_bool (fmt: formatter) (b: bool) : unit =
      pp_bool_generic fmt b

    let pp_option_generic
        ?(none: string="None")
        ?(none_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(some_style: Ocolor_types.style list=[])
        (type a)
        (p: a pp)
        (fmt: formatter)
        (o: a option)
      : unit =
      match o with
      | None ->
        fprintf fmt "%a%s%a" pp_open_styles none_style none pp_close_styles ()
      | Some o ->
        fprintf fmt "%a%a%a" pp_open_styles some_style p o pp_close_styles ()

    let pp_option p fmt o = pp_option_generic p fmt o

    let pp_pair_generic
        ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(elem_style: Ocolor_types.style list=[])
        (type a) (type b)
        (f: a pp)
        (g: b pp)
        (fmt: formatter)
        (a, b : a * b)
      : unit =
      let l_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style left pp_close_styles ()
      in
      let r_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style right pp_close_styles ()
      in
      let sep (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles sep_style sep pp_close_styles ()
      in
      let elem (type a) (p: formatter -> a -> unit) (fmt: formatter) (a: a) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles elem_style p a pp_close_styles ()
      in
      fprintf fmt "%t%a%t%a%t"
        l_delim (elem f) a sep (elem g) b r_delim

    let pp_pair
        (type a) (type b)
        (f: a pp)
        (g: b pp)
        (fmt: formatter) (p : a * b)
      : unit =
      pp_pair_generic f g fmt p

    let pp_3_tuple_generic
        ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(elem_style: Ocolor_types.style list=[])
        (type a) (type b) (type c)
        (f: a pp)
        (g: b pp)
        (h: c pp)
        (fmt: formatter)
        (a, b, c : a * b * c)
      : unit =
      let l_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style left pp_close_styles ()
      in
      let r_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style right pp_close_styles ()
      in
      let sep (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles sep_style sep pp_close_styles ()
      in
      let elem (type a) (p: formatter -> a -> unit) (fmt: formatter) (a: a) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles elem_style p a pp_close_styles ()
      in
      fprintf fmt "%t%a%t%a%t%a%t"
        l_delim (elem f) a sep (elem g) b sep (elem h) c r_delim

    let pp_3_tuple (type a) (type b) (type c)
        (f: a pp)
        (g: b pp)
        (h: c pp)
        (fmt: formatter)
        (t : a * b * c)
      : unit =
      pp_3_tuple_generic f g h fmt t

    let pp_4_tuple_generic
        ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(elem_style: Ocolor_types.style list=[])
        (type a) (type b) (type c) (type d)
        (f: a pp)
        (g: b pp)
        (h: c pp)
        (i: d pp)
        (fmt: formatter)
        (a, b, c, d : a * b * c * d)
      : unit =
      let l_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style left pp_close_styles ()
      in
      let r_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style right pp_close_styles ()
      in
      let sep (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles sep_style sep pp_close_styles ()
      in
      let elem (type a) (p: formatter -> a -> unit) (fmt: formatter) (a: a) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles elem_style p a pp_close_styles ()
      in
      fprintf fmt "%t%a%t%a%t%a%t%a%t"
        l_delim (elem f) a sep (elem g) b sep (elem h) c sep (elem i) d r_delim

    let pp_4_tuple (type a) (type b) (type c) (type d)
        (f: a pp)
        (g: b pp)
        (h: c pp)
        (i: d pp)
        (fmt: formatter)
        (q : a * b * c * d)
      : unit =
      pp_4_tuple_generic f g h i fmt q

    let pp_5_tuple_generic
        ?(left: string="(") ?(sep: string=", ") ?(right: string=")")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(elem_style: Ocolor_types.style list=[])
        (type a) (type b) (type c) (type d) (type e)
        (f: a pp)
        (g: b pp)
        (h: c pp)
        (i: d pp)
        (j: e pp)
        (fmt: formatter)
        (a, b, c, d, e : a * b * c * d * e)
      : unit =
      let l_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style left pp_close_styles ()
      in
      let r_delim (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles delim_style right pp_close_styles ()
      in
      let sep (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles sep_style sep pp_close_styles ()
      in
      let elem (type a) (p: formatter -> a -> unit) (fmt: formatter) (a: a) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles elem_style p a pp_close_styles ()
      in
      fprintf fmt "%t%a%t%a%t%a%t%a%t%a%t"
        l_delim (elem f) a sep (elem g) b sep (elem h) c sep (elem i) d sep (elem j) e r_delim

    let pp_5_tuple (type a) (type b) (type c) (type d) (type e)
        (f: a pp)
        (g: b pp)
        (h: c pp)
        (i: d pp)
        (j: e pp)
        (fmt: formatter)
        (q: a * b * c * d * e)
      : unit =
      pp_5_tuple_generic f g h i j fmt q

    let pp_iterable_generic
        (type value) (type t)
        ?(left: string="[") ?(sep: string="; ") ?(right: string="]")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(elem_style: Ocolor_types.style list=[])
        (iter: (value -> unit) -> t -> unit)
        (p: value pp)
        (fmt: formatter)
        (x: t)
      : unit =
      let () = fprintf fmt "%a%s%a" pp_open_styles delim_style left pp_close_styles () in
      let first = ref true in
      let sep (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles sep_style sep pp_close_styles ()
      in
      let value (fmt: formatter) (a: value) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles elem_style p a pp_close_styles ()
      in
      let () =
        iter
          (fun v ->
             if !first then
               first := false
             else
               sep fmt;
             value fmt v
          )
          x
      in
      let () = fprintf fmt "%a%s%a" pp_open_styles delim_style right pp_close_styles () in
      ()

    let pp_iterable (type value) (type t)
        (iter: (value -> unit) -> t -> unit)
        (p: value pp)
        (fmt: formatter)
        (x: t)
      : unit =
      pp_iterable_generic iter p fmt x

    let pp_list_generic
        ?(left: string="[") ?(sep: string="; ") ?(right: string="]")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(elem_style: Ocolor_types.style list=[])
        (type a)
        (p: a pp) (fmt: formatter) (l: a list)
      : unit =
      pp_iterable_generic ~left ~sep ~right
        ~delim_style ~sep_style ~elem_style
        List.iter p fmt l

    let pp_list (type a) (p: a pp)
        (fmt: formatter) (l: a list)
      : unit =
      pp_list_generic p fmt l

    let pp_iterable_mapping_more_generic
        (type key) (type value) (type t)
        ?(left: string="{") ?(sep: string="; ") ?(right: string="}")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        (iter: (key -> value -> unit) -> t -> unit)
        (p: (key * value) pp)
        (fmt: formatter)
        (x: t)
      : unit =
      let () = fprintf fmt "%a%s%a" pp_open_styles delim_style left pp_close_styles () in
      let first = ref true in
      let sep (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles sep_style sep pp_close_styles ()
      in
      let () =
        iter
          (fun k v ->
             if !first then
               first := false
             else
               sep fmt;
             p fmt (k, v)
          )
          x
      in
      let () = fprintf fmt "%a%s%a" pp_open_styles delim_style right pp_close_styles () in
      ()

    let pp_iterable_mapping_generic
        (type key) (type value) (type t)
        ?(left: string="{") ?(sep: string="; ") ?(right: string="}")
        ?(mapsto: string=":")
        ?(delim_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(sep_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(mapsto_style: Ocolor_types.style list=Ocolor_types.[Faint])
        ?(key_style: Ocolor_types.style list=[])
        ?(value_style: Ocolor_types.style list=[])
        (iter: (key -> value -> unit) -> t -> unit)
        (pk: key pp)
        (pv: value pp)
        (fmt: formatter)
        (x: t)
      : unit =
      let mapsto (fmt: formatter) : unit =
        fprintf fmt "%a%s%a"
          pp_open_styles mapsto_style mapsto pp_close_styles ()
      in
      let key (fmt: formatter) (a: key) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles key_style pk a pp_close_styles ()
      in
      let value (fmt: formatter) (a: value) : unit =
        fprintf fmt "%a%a%a"
          pp_open_styles value_style pv a pp_close_styles ()
      in
      let p (fmt: formatter) (k, v: key * value) : unit =
        fprintf fmt "%a%t%a" key k mapsto value v
      in
      pp_iterable_mapping_more_generic ~left ~right ~sep
        ~delim_style ~sep_style iter p fmt x

    let pp_iterable_mapping
        (type key) (type value) (type t)
        (iter: (key -> value -> unit) -> t -> unit)
        (pk: key pp)
        (pv: value pp)
        (fmt: formatter)
        (x: t)
      : unit =
      pp_iterable_mapping_generic iter pk pv fmt x
  end)