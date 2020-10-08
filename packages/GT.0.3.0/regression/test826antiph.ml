include (struct
  type 'a foo = 'a [@@deriving gt ~options:{show; gmap; compare; foldl; eval; }]
end : sig
  type 'a foo = 'a [@@deriving gt ~options:{show; gmap; compare; foldl; eval; }]
end)

let () =
  print_endline @@ GT.show foo (GT.show GT.string) "asdf"
