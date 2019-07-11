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
    (PP : sig
       type formatter
       val fprintf : formatter -> ('a, formatter, unit) format -> 'a
       val pp_open_styles : formatter -> Ocolor_types.style list -> unit
       val pp_close_styles : formatter -> unit -> unit
     end)
  : PRETTY_PRINTERS
  with type formatter := PP.formatter
