(*{[
  open Stdcompat
  open Utils
  ]}*)

val output_line_number : out_channel -> string -> int -> unit

val output_position : out_channel -> Lexing.position -> unit

val output_channel_to_the_end :
    ?buffer_size:int -> out_channel -> in_channel -> unit

(** [output_channel_to_the_end ~buffer_size out_channel in_channel] reads
    [in_channel] up to reaching the end of the channel and copies the contents
    to [out_channel].
    If [buffer_size] is given, it sets the size of the chunks read at once
    (default: 1024).
    
    {[
let () =
  let (input_file, channel) = Filename.open_temp_file "ocamlcodoc_test" "in" in
  protect begin fun () ->
    protect begin fun () ->
      for i = 0 to 4095 do
        output_char channel (char_of_int (i mod 256))
      done
    end
    ~finally:(fun () -> close_out channel);
    let in_channel = open_in input_file in
    protect begin fun () ->
      let (output_file, out_channel) =
        Filename.open_temp_file "ocamlcodoc_test" "out" in
      protect begin fun () ->
        protect begin fun () ->
          output_channel_to_the_end out_channel in_channel
        end
        ~finally:(fun () -> close_out out_channel);
        let channel = open_in output_file in
        protect begin fun () ->
          for i = 0 to 4095 do
            assert (int_of_char (input_char channel) = i mod 256)
          done;
          try
            ignore (input_char channel);
            assert false
          with End_of_file -> ()
        end
        ~finally:(fun () -> close_in channel)
      end
      ~finally:(fun () -> Sys.remove output_file)
    end
    ~finally:(fun () -> close_in in_channel)
  end
  ~finally:(fun () -> Sys.remove input_file)
    ]}
 *)
