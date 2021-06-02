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
end

let contents = gen_data ()

let () = Command.run (Bench.make_command [
    Bench.Test.create ~name:"jsonxtrd" (fun () -> Jsonxt.Basic.of_string contents)
  ; Bench.Test.create ~name:"yjsonrd" (fun () -> Yj.read contents)
  ])
