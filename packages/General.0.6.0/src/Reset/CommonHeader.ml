#if OCAML_VERSION >= (4, 3, 0)
  #define HAS_Pervasives_result
  #define HAS_Ephemeron
  #define HAS_Uchar
#endif

#if OCAML_VERSION >= (4, 4, 0)
  #define HAS_Spacetime
#endif

#if OCAML_VERSION >= (4, 5, 0)
  #define HAS_Pervasives_bool_of_string_opt
  #define HAS_Pervasives_int_of_string_opt
  #define HAS_Pervasives_float_of_string_opt
  #define HAS_Pervasives_read_int_opt
  #define HAS_Pervasives_read_float_opt
#endif

#if OCAML_VERSION >= (4, 7, 0)
  #define HAS_Stdlib
  #define HAS_Bigarray
  #define HAS_Float
  #define HAS_Seq
#endif
