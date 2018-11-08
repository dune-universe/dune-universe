open Base
open Stdio
module P = Tensorboard_protobuf

type t =
  { out_channel : Out_channel.t
  }

let int64_le_to_bytes i =
  let bigstring = Bigstringaf.create 8 in
  Bigstringaf.set_int64_le bigstring 0 (Int64.of_int i);
  Bigstringaf.substring bigstring ~off:0 ~len:8

let int32_le_to_bytes i =
  let bigstring = Bigstringaf.create 4 in
  Bigstringaf.set_int32_le bigstring 0 i;
  Bigstringaf.substring bigstring ~off:0 ~len:4

let crc32_mask i =
  Int32.bit_or
    (Int32.shift_right_logical i 15)
    (Int32.shift_left i 17)
  |> Int32.(+) (Int32.of_string "0xa282ead8")

let write_event t ~step ~what =
  let encoder = P.Protobuf.Encoder.create () in
  let event =
    { P.Event_types.wall_time = Unix.time ()
    ; step = Int64.of_int step
    ; what
    }
  in
  P.Event_pb.encode_event event encoder;
  let data = P.Pbrt.Encoder.to_bytes encoder |> Bytes.to_string in
  let data_length = String.length data in

  let data_length_str = int64_le_to_bytes data_length in
  let crc_data_length = Crc32c.string data_length_str 0 8 |> crc32_mask in
  let crc_data_length_str = int32_le_to_bytes crc_data_length in
  let crc_data = Crc32c.string data 0 data_length |> crc32_mask in
  let crc_data_str = int32_le_to_bytes crc_data in

  Out_channel.output_string t.out_channel data_length_str;
  Out_channel.output_string t.out_channel crc_data_length_str;
  Out_channel.output_string t.out_channel data;
  Out_channel.output_string t.out_channel crc_data_str;
  Out_channel.flush t.out_channel

let create dirname =
  if not (Caml.Sys.file_exists dirname)
  then Unix.mkdir dirname 0o755;
  if Caml.Sys.file_exists dirname && not (Caml.Sys.is_directory dirname)
  then
    raise (Invalid_argument
      (Printf.sprintf "[create_dirname]: %s is not a directory" dirname));
  let basename =
    Printf.sprintf "out.tfevents.%d.%s"
      (Unix.time () |> Float.to_int)
      (Unix.gethostname ())
  in
  let filename = Caml.Filename.concat dirname basename in
  let t =
    { out_channel = Out_channel.create filename
    }
  in
  write_event t ~step:0 ~what:(File_version "brain.Event:2");
  t

let write_value t ~step ~name ~value =
  let value =
    P.Summary_types.default_summary_value ()
      ~tag:name ~value:(Simple_value value)
  in
  let summary = P.Summary_types.default_summary ~value:[ value ] () in
  write_event t ~step ~what:(Summary summary)

let write_text t ~step ~name ~text =
  let string_tensor =
    let tensor_shape =
      let one =
        P.Tensor_shape_types.default_tensor_shape_proto_dim ()
          ~size:Int64.one
      in
      P.Tensor_shape_types.default_tensor_shape_proto ~dim:[ one ] ()
    in
    P.Tensor_types.default_tensor_proto ()
      ~string_val:[ Bytes.of_string text ]
      ~dtype:Dt_string
      ~tensor_shape:(Some tensor_shape)
  in
  let metadata =
    let plugin_data =
      P.Summary_types.default_summary_metadata_plugin_data ()
        ~plugin_name:"text"
    in
    P.Summary_types.default_summary_metadata ~plugin_data:(Some plugin_data) ()
  in
  let value =
    P.Summary_types.default_summary_value ()
      ~metadata:(Some metadata)
      ~tag:(name ^ "/text_summary") ~value:(Tensor string_tensor)
  in
  let summary = P.Summary_types.default_summary ~value:[ value ] () in
  write_event t ~step ~what:(Summary summary)


let close t =
  Out_channel.close t.out_channel
