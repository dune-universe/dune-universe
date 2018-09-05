let write ~channels ~sample_rate ~samples ~generator ~output_file =
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in
  let buffer_length = 1024 in
  let buffer = Audio.create channels buffer_length in
  for _ = 0 to samples / buffer_length - 1 do
    generator#fill buffer 0 buffer_length;
    wav#write buffer 0 buffer_length
  done;
  Result.Ok (wav#close)
