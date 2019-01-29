open Printf
module C = Configurator.V1

let conf c =
  let cflags = [] in
  let libs = ["-lct"; "-lsybdb"] in
  let reg_row =
    let open C.C_define in
    (* FIXME: "-" hack for a negative value.
       See https://github.com/ocaml/dune/pull/1334 *)
    let h = import c ~includes:["sybdb.h"] [("-REG_ROW", Type.Int)] in
    match List.assoc "-REG_ROW" h with
    | Value.Int r -> -r
    | Value.Switch _ | Value.String _ -> assert false
    | exception _ ->
       C.die "The value of REG_ROW was not found in the C hreader file. \
              Please make sure the development files of FreeTDS are \
              installed in a location where the C compiler finds them." in
  let fh = open_out "reg_row.txt" in
  fprintf fh "%d" reg_row;
  close_out fh;
  C.Flags.write_sexp "c_flags.sexp" cflags;
  C.Flags.write_sexp "c_library_flags.sexp" libs

let () =
  C.main ~name:"freetds" conf

