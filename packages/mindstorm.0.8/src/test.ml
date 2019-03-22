(* Test of connection to the brick (write & read) *)

open Printf


let usleep sec =
  ignore(Unix.select [] [] [] sec)

(* Add the 2 bluetooth bytes *)
let message s =
  let len = String.length s in
  let msg = String.create (len + 2) in
  msg.[0] <- Char.chr(len land 0xFF);
  msg.[1] <- Char.chr(len lsr 8);
  String.blit s 0 msg 2 len;
  msg
;;

external connect : string -> Unix.file_descr = "ocaml_mindstorm_connect"


let () =
 (* let fd = connect "00:16:53:03:A5:32" in *)
  let fd = connect "\\\\.\\COM40" in 
  printf "Connected!\n%!";

  (*   let msg = "\x02\x00\x01\x9B" in *)
(*   let msg = message "\x80\x03\xf4\x01\xf4\x01" (\* beep *\) in *)
  let msg = message(
    let fname = "Woops.rso" in
    let s = String.make 23 '\x00' in
    s.[0] <- '\x00';
    s.[1] <- '\x02';
    String.blit fname 0 s 3 (String.length fname);
    s
  ) in

(*   let msg = message "\x00\x0B" in *)
  for i = 1 to 3 do
    let w = Unix.write fd msg 0 (String.length msg) in
    printf "Wrote %i bytes\n%!" w;

    let really_read n =
      let n = n + 2 in (* 2 length bytes *)
      let ans = String.create n in
      let rec loop i n =
        let r = Unix.read fd ans i n in
        if r = n then ans
        else (
          printf "read=%i %!" r;
(*           ignore(Unix.select [fd] [] [fd] (-1.)); *)
(*           usleep 1.; *)
        (*  ignore(Unix.select [fd] [] [] 0.060);*)
          loop (i + r) (n - r)
        ) in
      loop 0 n
    in
    let ans = really_read 3 in
    printf "read %S\n%!" ans;

    usleep 1.;
  done;

  Unix.close fd
