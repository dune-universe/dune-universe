let write ~channels ~sample_rate ~samples ~generator ~output_file =
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in

  let main_buffer_length = 1024 in
  let main_buffer = Audio.create channels main_buffer_length in

  let end_buffer_length = samples mod main_buffer_length in
  let end_buffer = Audio.create channels end_buffer_length in

  let rec write samples_left =
    if samples_left > 0 then begin
      let buffer =
        if samples_left >= main_buffer_length
        then main_buffer
        else end_buffer
      in

      generator#fill buffer;
      wav#write buffer;
      write (samples_left - (Audio.length buffer))
    end
  in

  write samples;
  Result.Ok (wav#close)
