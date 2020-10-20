val (>>=) :
  ('a, 'b) Result.result ->
  ('a -> ('c, 'b) Result.result) ->
  ('c, 'b) Result.result

val (>|=) :
  ('a, 'b) Result.result ->
  ('a -> 'c) ->
  ('c, 'b) Result.result
