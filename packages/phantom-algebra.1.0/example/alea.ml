;; Random.self_init ()
let u () = Random.float 1.
let (<*>) f x () = f (x ())
let (<$>) f x () = f()(x())

open Phantom_algebra.Core

let scalar = scalar <*> u
let vec2 = vec2 <*> u <$> u
let vec3 = vec3 <*> u <$> u <$> u
let vec4 = vec4 <*> u <$> u <$> u <$> u
let mat2 = mat2 <*> vec2 <$> vec2
let mat3 = mat3 <*> vec3 <$> vec3 <$> vec3
let mat4 = mat4 <*> vec4 <$> vec4 <$> vec4 <$> vec4
