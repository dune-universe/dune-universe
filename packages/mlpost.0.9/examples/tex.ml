open Mlpost
open Concrete
open Command
open Picture
open CPoint
open Ctypes

let s = "Bonjour les amis $42_{la reponse}$"

let p =
  let t = tex s in
  let bs = baselines s in
  let { x = left } = cpoint_of_point (west t) in
  let { x = right } = cpoint_of_point (east t) in
  let pl = List.map (fun e -> Path.path [ (left, e); (right, e) ]) bs in
  let pl = List.map Path.draw pl in
  seq (t :: pl)

let () = Metapost.emit "baselines" p
