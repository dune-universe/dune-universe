type 'a t = C of {xxx: 'a; yyy: GT.int}
[@@deriving gt ~options:{ html; fmt; gmap; show; compare; eq; foldl }]

let () =
  let x1 = C {xxx="asdf"; yyy=1} in
  GT.fmt t GT.string.plugins#fmt Format.std_formatter x1;
  Format.printf "%s" @@ HTML.toHTML @@
  GT.html t (GT.html GT.string) x1;
  ()
