let write ~channels ~sample_rate ~samples ~generator ~output_file =
  let wav = new Mm.Audio.IO.Writer.to_wav_file channels sample_rate output_file in

  let buffer_length = 1024 in
  let buffer = Mm.Audio.create channels buffer_length in

  let rec write samples_left =
    if samples_left > 0 then begin
      let output_buffer =
        if samples_left >= buffer_length
        then buffer
        else Mm.Audio.sub buffer 0 samples_left
      in

      generator#fill output_buffer;
      wav#write output_buffer;
      write (samples_left - (Mm.Audio.length buffer))
    end
  in

  write samples;
  Result.Ok (wav#close)
