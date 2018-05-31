open Hidapi

let () =
  init () ;
  let devs = Hidapi.enumerate () in
  ListLabels.iter devs ~f:begin fun { vendor_id ; product_id } ->
    Printf.printf "0x%x 0x%x\n" vendor_id product_id ;
    match open_id ~vendor_id ~product_id with
    | None ->
      Printf.printf "Impossible to open %d:%d\n" vendor_id product_id
    | Some d ->
      close d ;
      Printf.printf "Ok, opened/closed %d:%d\n" vendor_id product_id ;
  end ;
  deinit ()
