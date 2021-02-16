open Mlpost
open Command
open Box
open Num

module Forms = struct
  let circle = draw (circle (empty ~height:(bp 5.) ~width:(bp 5.) ()))

  let rect = draw (rect (empty ~height:(bp 5.) ~width:(bp 5.) ()))

  let round_rect = draw (round_rect (empty ~height:(bp 5.) ~width:(bp 5.) ()))

  let ellipse = draw (ellipse (empty ~width:(bp 5.) ()))

  let patatoid = draw (patatoid (empty ~height:(bp 5.) ~width:(bp 10.) ()))

  let tex = draw (tex "text")
end

let brect = Box.rect (empty ~height:(bp 5.) ~width:(bp 5.) ())

module Dirs = struct
  let dot p =
    Command.draw ~pen:(Pen.scale (bp 4.) Pen.circle) (Path.pathp [ p ])

  let ctr = seq [ draw brect; dot (ctr brect) ]

  let north = seq [ draw brect; dot (north brect) ]

  let south = seq [ draw brect; dot (south brect) ]

  let west = seq [ draw brect; dot (west brect) ]

  let east = seq [ draw brect; dot (east brect) ]

  let north_west = seq [ draw brect; dot (north_west brect) ]

  let south_west = seq [ draw brect; dot (south_west brect) ]

  let north_east = seq [ draw brect; dot (north_east brect) ]

  let south_east = seq [ draw brect; dot (south_east brect) ]
end

let cpic c = Box.pic ~stroke:None (Picture.make c)

module Size = struct
  open Arrow

  let head = head_triangle_full

  let kind = add_foot ~head (add_head ~head (add_line empty))

  let dbl_arrow =
    let ar =
      Arrow.point_to_point ~kind Point.origin (Point.pt (bp 10., Num.zero))
    in
    cpic ar

  let width = Box.draw (Box.vbox [ brect; dbl_arrow ])

  let height = Box.draw (Box.hbox [ Box.rotate 90. dbl_arrow; brect ])
end

module Move = struct
  let fnstex s = Picture.tex (Format.sprintf "{\\footnotesize %s}" s)

  let shift =
    let pt = Point.pt (bp 40., bp 25.) in
    let vec =
      cpic
        (seq
           [
             Arrow.point_to_point Point.origin pt;
             Command.dotlabel ~pos:`Top (fnstex "pt") pt;
             Command.dotlabel ~pos:`Bot (fnstex "(0,0)") Point.origin;
           ])
    in
    let b = brect in
    let b' = Box.shift pt b in
    let shift =
      cpic
        (seq
           [
             Box.draw b;
             Box.draw b';
             Arrow.point_to_point (Box.ctr b) (Box.ctr b');
           ])
    in
    Box.draw (Box.hbox [ vec; shift ])

  let center =
    let pt = Point.pt (bp 40., bp 25.) in
    let vec =
      seq
        [
          Arrow.point_to_point Point.origin pt;
          Command.dotlabel ~pos:`Top (fnstex "pt") pt;
        ]
    in
    let b = brect in
    let b' = Box.center pt b in
    seq [ vec; Box.draw b; Box.draw b' ]
end

module Align = struct
  let dist = 20.

  let p1 = Point.p (-.dist, dist)

  let p2 = Point.sub Point.origin p1

  let mkb s = round_rect (tex s)

  let a, b, c =
    let a = mkb "A" and borig = mkb "B" and corig = mkb "C" in
    let b = shift p1 borig in
    let c = shift p2 corig in
    (a, b, c)

  let all = [ a; b; c ]

  let orig = group all

  let sidebyside l =
    let b = group l in
    let s = hbox ~padding:(Num.bp 50.) [ orig; b ] in
    seq
      [
        draw s;
        Helpers.box_arrow ~sep:(Num.bp 20.) ~within:s ~pen:Pen.circle
          ~color:Color.red orig b;
      ]

  let origfig = draw orig

  let halign = sidebyside (halign Num.zero all)

  let hplace = sidebyside (hplace all)

  let hbox = sidebyside (hbox_list all)
end

let _ = Metapost.emit "circle" Forms.circle

let _ = Metapost.emit "rect" Forms.rect

let _ = Metapost.emit "round_rect" Forms.round_rect

let _ = Metapost.emit "ellipse" Forms.ellipse

let _ = Metapost.emit "patatoid" Forms.patatoid

let _ = Metapost.emit "tex" Forms.tex

let _ = Metapost.emit "ctr" Dirs.ctr

let _ = Metapost.emit "north" Dirs.north

let _ = Metapost.emit "south" Dirs.south

let _ = Metapost.emit "west" Dirs.west

let _ = Metapost.emit "east" Dirs.east

let _ = Metapost.emit "north_west" Dirs.north_west

let _ = Metapost.emit "south_west" Dirs.south_west

let _ = Metapost.emit "north_east" Dirs.north_east

let _ = Metapost.emit "south_east" Dirs.south_east

let _ = Metapost.emit "width" Size.width

let _ = Metapost.emit "height" Size.height

let _ = Metapost.emit "shift" Move.shift

let _ = Metapost.emit "center" Move.center

let _ = Metapost.emit "halign" Align.halign

let _ = Metapost.emit "hplace" Align.hplace

let _ = Metapost.emit "hbox" Align.hbox
