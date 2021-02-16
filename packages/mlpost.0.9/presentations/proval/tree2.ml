open Mlpost
open Tree

let fig =
  [
    draw ~node_style:Rect ~arrow_style:Undirected
      (node "1"
         [ node "2" [ leaf "4"; leaf "5" ]; node "3" [ leaf "6"; leaf "7" ] ]);
  ]

let _ = Metapost.emit "tree2" fig
