module L : sig
  @type 'a list = [ `Nil  | `Cons of ('a * 'a list) ] with gmap,show

end = struct
  @type 'a list = [ `Nil  | `Cons of ('a * 'a list) ] with gmap,show

  let () =
    print_endline @@ show_list (Printf.sprintf "%d") @@
    gmap_list ((+)1) @@
    (`Cons (1, `Nil))
end
