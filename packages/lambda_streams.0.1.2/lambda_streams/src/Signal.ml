type 'a t =
  | Data of 'a
  | EndOfSignal

let pure (value : 'a) : 'a t = Data value

let empty () : 'a t = EndOfSignal

let default default_value = function
  | Data value -> value
  | EndOfSignal -> default_value

let satisfies f = function
  | Data value -> f value
  | EndOfSignal -> false

let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
  match ma with
  | Data value -> Data (f value)
  | EndOfSignal -> EndOfSignal

let filter (f : 'a -> bool) (ma : 'a t) : 'a t =
  match ma with
  | Data value when f value -> Data value
  | Data _ | EndOfSignal -> EndOfSignal

let fold (f : 'a -> 'b -> 'a) (init : 'a) (ma : 'b t) : 'a =
  match ma with
  | Data value -> f init value
  | EndOfSignal -> init

let from_option = function
  | Some value -> Data value
  | None -> EndOfSignal

let to_option = function
  | Data value -> Some value
  | EndOfSignal -> None
