let prologue =
  "\n\
   #if defined(__MINGW32__) || defined(__MINGW64__)\n\
   #define __USE_MINGW_ANSI_STDIO 1\n\
   #include <stdio.h> /* see: https://sourceforge.net/p/mingw-w64/bugs/627/ */\n\
   #endif\n\
   #include <maxminddb.h>\n"

let () =
  print_endline prologue ;
  Cstubs.Types.write_c Format.std_formatter (module Mmdb_types_bindings.M)
