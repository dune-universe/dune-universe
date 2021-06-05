let gen_data () =
  let record = "  {
    \"foo\" : \"bar\",
    \"foo1\" : \"bar1\",
    \"float\" : 1.1,
    \"int\" : 10,
    \"int_is_float\" : 72057594037927935,
    \"int_is_int\" : 1111111111
  }"
  in
  let buf = Buffer.create 8192 in
  let add_string s = Buffer.add_string buf s in
  let () = add_string "[\n" in
  let rec loop i =
    if i <= 0 then ()
    else begin
      add_string record;
      add_string ",\n";
      loop (i - 1)
    end
  in loop 99;
  add_string record;
  add_string "\n]\n";
  Buffer.contents buf

open Core
open Core_bench

module Yj = struct
  open Yojson

  let read contents = Basic.from_string contents
  let write json = Basic.to_string json
end

let jsonxt_data = gen_data () |> Jsonxt.Basic.of_string
let yojson_data = gen_data () |> Yj.read

let () = Command.run (Bench.make_command [
    Bench.Test.create ~name:"jsonxtwr" (fun () -> Jsonxt.Basic.to_string jsonxt_data)
  ; Bench.Test.create ~name:"yjsonwr" (fun () -> Yj.write yojson_data)
  ])
