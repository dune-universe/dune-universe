(*type op = Union | Inter;;*)
(*type elem = Op of op | Words of string list;;*)

let top3 stack = 
  let a = Stack.pop stack in
  let b = Stack.pop stack in
  let c = Stack.pop stack in
  (a, b, c)

let process stack = 
  let top = Stack.top stack in 
  match top with
  | Words _   -> stack
  | Op Union  -> union stack
  | Op Inter  -> inter stack
