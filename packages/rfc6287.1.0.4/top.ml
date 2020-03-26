(* #use this to setup the interactive environment. *)

#require "nocrypto, nocrypto.unix, astring, hex, rresult";;

#directory "_build/lib";;
#load_rec "rfc6287.cmo";;

let _ = Nocrypto_entropy_unix.initialize ();;
(*
# let suite = Rresult.R.get_ok (Rfc6287.t_of_string "OCRA-1:HOTP-SHA1-6:C-QN08-PSHA1");;
val suite : Rfc6287.t = <abstr>
# let q = Rfc6287.challenge suite;;
val q : bytes = "11961826"
# let p = Nocrypto.Hash.SHA1.digest (Cstruct.of_string "1234");;
val p : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 20}
# let key = Hex.to_cstruct (`Hex "00112233445566778899aabbccddeeff00112233");;
val key : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 20}
# let a = Rresult.R.get_ok (Rfc6287.gen suite ~key ~q ~p:(`Digest p) ~c:22L);;
val a : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 6}
# Cstruct.to_string a;;
- : bytes = "920862"
# Rresult.R.get_ok (Rfc6287.verify suite ~key ~p:(`String "1234") ~q ~a ~c:20L ~cw:20);;
- : bool * int64 option = (true, Some 23L)
*)
