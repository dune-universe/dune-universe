type ty = [`Seq | `Div | `Qual]
type yes_no_maybe = [`Yes | `No | `Maybe]

(* Single Palette *)
type t = { length: int;
           rgb: Gg.color array;
           cmyk: Gg.v4 array;
           ty: ty;
           blind: yes_no_maybe;
           print: yes_no_maybe;
           copy: yes_no_maybe;
           screen: yes_no_maybe }
