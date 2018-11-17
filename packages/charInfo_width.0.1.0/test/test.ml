open CamomileLibraryDefault
open Printf

let msg= "a͜b͡c字符宽度"

let%expect_test _=
  let length= Camomile.UTF8.length msg in
  for i= 0 to length - 1 do
    let c= Camomile.UTF8.get msg i in
    let len= CharInfo_width.width c in
    printf " %d" len
  done;
  [%expect "1 0 1 0 1 2 2 2 2"]

