let c_headers = {|
#include <portaudio.h>
|}

let main () =
	Format.printf "%s@\n" c_headers;
	Cstubs.Types.write_c Format.std_formatter (module Stubs.Make)
;;

let () = main ()
