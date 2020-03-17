type 'a input = unit -> 'a

and 'a output = 'a -> unit

type 'a connection = ('a input, unit output) Connection.t

let make_input f = f

let make_output f = f

let make_mutator ~initial =
  let value = ref initial in
  let input () = !value and output v = value := v in
  input, output

let pure value () = value

let enumerate () =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    !index

let next stream = stream ()

let send value output_stream = output_stream value

let pipe output_stream input_stream =
  while true do
    input_stream () |> output_stream
  done

let accumulate n f init stream =
  let index = ref 0 and acc = ref init in
  while !index < n do
    index := !index + 1;
    acc := f !acc (stream ())
  done;
  !acc

let map f stream () = stream () |> f

let filter f stream () =
  let value = ref @@ stream () in
  while not @@ f !value do
    value := stream ()
  done;
  !value

let scan f init stream =
  let acc = ref init in
  fun () ->
    acc := f !acc (stream ());
    !acc
