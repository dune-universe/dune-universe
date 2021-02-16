open Mlpost
open Tree

let fig =
  [
    draw ~edge_style:HalfSquare
      (node "1"
         [ node "2" [ leaf "4"; leaf "5" ]; node "3" [ leaf "6"; leaf "7" ] ]);
  ]

let _ = Metapost.emit "tree6" fig
