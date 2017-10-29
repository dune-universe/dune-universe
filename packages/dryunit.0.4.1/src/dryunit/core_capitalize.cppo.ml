#if OCAML_VERSION < (4, 03, 0)
  let capitalize_ascii = Bytes.capitalize
#else
  let capitalize_ascii = Bytes.capitalize_ascii
#endif
