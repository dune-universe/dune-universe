(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

let () = Papi.init () ;;

#install_printer Papi.pp_event
#install_printer Papi.pp_error
#install_printer Papi.pp_exn_error
