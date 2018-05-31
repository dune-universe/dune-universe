open Printf
open Stdio
module C = Configurator

let conf c =
  let cflags = [] in
  let libs = ["-lct"; "-lsybdb"] in
  let reg_row =
    let open C.C_define in
    let h = import c ~link_flags:libs ~includes:["sybdb.h"]
              [("REG_ROW", Type.Int)] in
    match List.assoc "REG_ROW" h with
    | Value.Int r -> r
    | Value.Switch _ | Value.String _ -> assert false
    | exception _ ->
       C.die "The value of REG_ROW was not found in the C hreader file. \
              Please make sure the development files of FreeTDS are \
              installed in a location where the C compiler finds them." in
  Out_channel.write_all "cppo_flags.sexp"
    ~data:(sprintf "-D\nREG_ROW %d\n" reg_row);
  let write_sexp file sexp =
    Out_channel.write_all file ~data:(Base.Sexp.to_string sexp) in
  write_sexp "c_flags.sexp" Base.(sexp_of_list sexp_of_string cflags);
  write_sexp "c_library_flags.sexp" Base.(sexp_of_list sexp_of_string libs)

let () =
  C.main ~name:"freetds" conf

