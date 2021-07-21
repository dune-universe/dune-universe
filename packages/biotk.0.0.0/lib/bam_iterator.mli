open Biocaml_unix

val fold0 :
  bam:string ->
  bai:string ->
  loc:GLoc.t ->
  init:'a ->
  f:(Bam.Header.t -> 'a -> Bam.Alignment0.t -> 'a) ->
  ('a, [> `Msg of string]) result

val fold :
  bam:string ->
  bai:string ->
  locs:GLoc.t list ->
  init:'a ->
  f:(Bam.Header.t -> 'a -> Bam.Alignment0.t -> 'a) ->
  ('a list, [> `Msg of string]) result
