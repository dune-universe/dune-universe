open! Base
open! Import

let negated = [%accessor Accessor.isomorphism ~get:Fn.non ~construct:Fn.non]
let result = [%accessor Accessor.mapper (fun g ~f x -> f (g x))]
let resulti = [%accessor Accessor.mapperi (fun g ~f x -> f x (g x))]
let flipped = [%accessor Accessor.isomorphism ~get:Fn.flip ~construct:Fn.flip]

include Accessor.Of_applicative2 (struct
    type ('output, 'input) t = 'input -> 'output

    let return a _ = a
    let apply t1 t2 input = t1 input (t2 input)
    let map t ~f input = f (t input)
  end)
