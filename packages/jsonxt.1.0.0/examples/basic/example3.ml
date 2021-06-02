module IO = struct
  type 'a t = 'a

  let return v = v
  let (>>=) v f = f v
end

module JsonIO = Jsonxt.Basic_monad.Make(IO)
open IO

let _ =
  let ic = open_in "test.json" in
  let reader buf len = return (input ic buf 0 len) in
  let writer s = return (output_string stdout s) in
  JsonIO.read_json ~reader ()
  >>= function
    | Error err -> print_endline ("ERROR: " ^ err); return ()
    | Ok json -> JsonIO.write_json_hum ~writer json
;;
