open Base
open Tensorboard

let () =
  let summary_writer = Summary_writer.create "/tmp/tensorboard-logs" in
  for step = 0 to 42 do
    let value = Float.of_int step |> Float.cos in
    Summary_writer.write_value summary_writer ~step ~name:"value/3" ~value;
  done;
  Summary_writer.write_text summary_writer
    ~step:1337 ~name:"txt" ~text:"foobar";
  Summary_writer.close summary_writer
