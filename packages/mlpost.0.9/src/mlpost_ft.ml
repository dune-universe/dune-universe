type t = Cairo.Ft.face

external ft_get_name_index : t -> string -> int = "ml_FT_Get_Name_Index"

external ft_get_char_index : t -> int -> int = "ml_FT_Get_Char_Index"

external ft_num_charmaps : t -> int = "ml_FT_num_charmaps"

external ft_set_charmap : t -> int -> int = "ml_FT_set_charmap"

let ft_set_charmap face index =
  if index < 0 || ft_num_charmaps face <= index then
    invalid_arg "ft_set_charmap : invalid charmap index";
  let r = ft_set_charmap face index in
  if r <> 0 then invalid_arg "ft_set_charmap : unsuccesful"
