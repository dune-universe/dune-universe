(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

let version v =
  try
    let p = String.index_from v 0 '.' in
    let major = int_of_string (String.sub v 0 p) in
      try
	let p2 =
          try String.index_from v (p+1) '.'
          with 
	    | Not_found -> String.length v in
	let minor = int_of_string (String.sub v (p+1) (p2 - p - 1)) in
	  try
            let len = String.length v in
            let add = int_of_string (String.sub v (p2+1) (len - p2 - 1)) in
	      (major, minor, add)
	  with
	    | _ -> (major, minor, 0)
      with
	| _ -> (major, 0, 0)
  with
    | _ -> (0,0,0)
	  

let () =
  let ref_version = ref "" in
    Arg.parse
      [ "-ref",Arg.Set_string ref_version," version_number: Check the version passed as anonymous arguments against this version_number" ]
      (fun s ->
	 let v =
	   try
	     let p = String.index s '+' in
	       String.sub s 0 p
	   with
	     | Not_found -> s in
	 let (major,minor,add) = version v in
       let (major_r, minor_r, add_r) = version !ref_version in
	 if major > major_r || (major = major_r && (minor > minor_r || (minor = minor_r && add >= add_r)))
	 then
	   let () = Printf.printf "OK\n" in exit 0
	 else
	   let () = Printf.printf "NOT OK\n" in exit 1)
      "Usage: ml_check -ref ref_version_number version_number"
