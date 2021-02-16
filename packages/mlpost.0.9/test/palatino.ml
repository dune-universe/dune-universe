open Mlpost
open Picture
open Command

let hello = tex "Hello"

let () =
  Metapost.emit "palatino"
    (seq
       [
         hello;
         shift (Point.cmp (-1., -1.)) hello;
         shift (Point.cmp (1., 1.)) hello;
         shift (Point.cmp (-1., 1.)) hello;
         shift (Point.cmp (1., -1.)) hello;
       ])
