open Cairo_ft
open Mlpost_ft
open Format

let () =
  let filename = Sys.argv.(1) in
  let ft = Cairo_ft.init_freetype () in
  let face = Cairo_ft.new_face ft filename in
  printf "charmap %i\n%!" (ft_num_charmaps face);
  ft_set_charmap face (int_of_string Sys.argv.(3));
  let char_index = int_of_string Sys.argv.(2) in
  printf "Index of %i : %i\n%!" char_index (ft_get_char_index face char_index)
