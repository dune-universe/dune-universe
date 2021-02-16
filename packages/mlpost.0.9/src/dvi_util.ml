module Int32 = struct
  include Int32

  let hash = to_int

  let equal x y = compare x y = 0
end

module Int32Map = Map.Make (Int32)
module Int32H = Hashtbl.Make (Int32)

type font_def = {
  checksum : int32;
  scale_factor : int32;
  design_size : int32;
  area : string;
  name : string;
}

let mk_font_def ~checksum ~scale_factor ~design_size ~area ~name =
  { checksum; scale_factor; design_size; area; name }

module Print_font = struct
  open Format

  let print_option pr ff = function
    | None -> fprintf ff "None"
    | Some a -> pr ff a

  let font_map ff font =
    fprintf ff "Tex:%s Human:%s Slant:%a Extend:%a Enc:%a Pfab:%s@."
      font.Fonts_type.tex_name font.Fonts_type.human_name
      (print_option pp_print_float)
      font.Fonts_type.slant
      (print_option pp_print_float)
      font.Fonts_type.extend
      (print_option pp_print_string)
      font.Fonts_type.enc_name font.Fonts_type.pfab_name

  let font k fmt f =
    fprintf fmt "\tFont number %ld (%s in directory [%s]) :\n" k f.name f.area;
    fprintf fmt "\t Checksum = %lx\n" f.checksum;
    fprintf fmt "\t Scale factor / Design size : %ld / %ld\n" f.scale_factor
      f.design_size
end
