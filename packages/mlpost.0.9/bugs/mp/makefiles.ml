open Mlpost
open Box

(* Some custom values *)

let small_padding = Num.bp 20.

let padding = Num.bp 50.

let delta = Num.bp 5.

let big_title s = tex ("\\textbf{\\Large{" ^ s ^ "}}")

let small_title s = tex ("\\textbf{\\emph{\\large{" ^ s ^ "}}}")

let plugin_color = Color.rgb8 255 165 0

let framac_color = Color.rgb8 50 205 50

(*let plugin_color = Color.lightcyan*)
let cil_color = Color.rgb8 250 128 114

let std_box color name s =
  round_rect ~name ~fill:color ~dx:delta ~dy:delta (tex s)

let config = std_box framac_color "config" "Makefile.config.in"

let framac = std_box framac_color "framac" "Makefile.in"

let plugin = std_box framac_color "plugin" "Makefile.plugin"

let dynamic = std_box framac_color "dynamic" "Makefile.dynamic"

let spec1 = std_box plugin_color "spec1" "specific Makefile for plug-in 1"

let dots = tex ~name:"dots" "$\\dots$"

let specn = std_box plugin_color "specn" "specific Makefile for plug-in $n$"

let spec_box = hbox ~padding:small_padding [ spec1; dots; specn ]

let box0 = hbox ~padding:small_padding [ framac; dots; plugin ]

let box1 = hbox ~padding [ box0; dynamic ]

let caption =
  tabularl ~pos:`Right ~hpadding:delta
    [
      [ tex "\\textbf{Caption:}"; empty () ];
      [
        hbox ~padding:small_padding
          [ tex ~name:"m1" "$m1$"; tex ~name:"m2" "$m2$" ];
        tex "Makefile $m1$ is included into Makefile $m2$";
      ];
    ]

let gen_box = vbox ~padding [ config; box1; spec_box ]

let full_box = vbox ~padding:small_padding ~pos:`Right [ gen_box; caption ]

let arrow ?outd ?style ?color src dst =
  let getf s = get s full_box in
  let src = getf src in
  let dst = getf dst in
  Helpers.box_arrow ?outd ?style ?color ~pen:Pen.circle src dst

let plugin_fc scale =
  let p1 = west (get "plugin" full_box) in
  let p2 = east (get "framac" full_box) in
  let p3 = Point.segment 0.33 p1 p2 in
  let p4 = Point.segment 0.66 p1 p2 in
  let p3 = Point.yscale (Num.bp scale) p3 in
  let p4 = Point.yscale (Num.bp scale) p4 in
  arrow (*    ~outd:(Path.vec p3 p4)*) ~style:(Path.jControls p3 p4)
    ~color:plugin_color "plugin" "framac"

let cmds =
  Command.seq
    [
      draw full_box;
      arrow ~color:framac_color "config" "framac";
      arrow ~color:framac_color "config" "dynamic";
      arrow ~color:framac_color "plugin" "dynamic";
      arrow ~color:plugin_color "dynamic" "spec1";
      (*      arrow "dynamic" "dots";*)
      arrow ~color:plugin_color "dynamic" "specn";
      (*      arrow plugin_color "plugin" "framac";*)
      plugin_fc 0.8;
      plugin_fc 1.2;
      arrow "m1" "m2";
    ]

let _ = Metapost.emit "makefiles" cmds
