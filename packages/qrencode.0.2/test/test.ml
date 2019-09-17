open Qrencode

let _ =
  let input = QRinput.create () in
  let _rc = QRinput.append input Data "http://www.tezos.com" in
  let code = QRcode.encode input in
  QRcode.to_png code ~size:10 ~margin:4 ~outfile:"/tmp/out.png"
