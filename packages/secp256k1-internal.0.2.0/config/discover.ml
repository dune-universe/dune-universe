module C = Configurator.V1

let hw = Config.hw_identifier ()
let sixtyfour = Sys.word_size = 64

let symbols = [
  (if sixtyfour then Some ("HAVE___INT128", None) else None) ;
  (if hw = "x86_64" then Some ("USE_ASM_X86_64", None) else None) ;
  Some ((if sixtyfour then "USE_SCALAR_4X64" else "USE_SCALAR_8X32"), None) ;
  Some ((if sixtyfour then "USE_FIELD_5X52" else "USE_FIELD_10X26"), None) ;
  Some ("USE_NUM_GMP", None) ;
  Some ("USE_SCALAR_INV_NUM", None) ;
  Some ("USE_FIELD_INV_NUM", None) ;
  Some ("SECP256K1_INLINE", Some "inline") ;
  Some ("SECP256K1_RESTRICT", Some "restrict") ;

  Some ("SECP256K1_TAG_PUBKEY_EVEN", Some "0x02") ;
  Some ("SECP256K1_TAG_PUBKEY_ODD", Some "0x03") ;
  Some ("SECP256K1_TAG_PUBKEY_UNCOMPRESSED", Some "0x04") ;
  Some ("SECP256K1_TAG_PUBKEY_HYBRID_EVEN", Some "0x06") ;
  Some ("SECP256K1_TAG_PUBKEY_HYBRID_ODD", Some "0x07") ;

  Some ("ENABLE_MODULE_RECOVERY", None) ;
]

let generate_defines symbols =
  let gen_symbol sym =
    match sym with
    | None -> None
    | Some (sym, None) -> Some (Printf.sprintf "-D%s" sym)
    | Some (sym, Some def) -> Some (Printf.sprintf "-D%s=%s" sym def)
  in
  List.filter_map gen_symbol symbols

let defines = generate_defines symbols

let get_config c =
  let open C.Pkg_config in
  let default = {libs = ["-lgmp"]; cflags = []} in
  let pkg_conf =
    Option.value
      (Option.bind (get c) (fun pc -> query pc ~package:"gmp"))
      ~default in
  {libs = pkg_conf.libs;
   cflags = List.append pkg_conf.cflags defines}

let () =
  C.main ~name:"discover" (fun c ->
      let conf = get_config c
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags ;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs )
