val write :
  channels:int ->
  sample_rate:int ->
  samples:int ->
  generator:Audio.Generator.t ->
  output_file:string ->
  (unit, string) Result.result
