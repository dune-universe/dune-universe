open Mlpost
open Box

(* Some custom values *)

let padding = Num.bp 15.

let big_padding = Num.bp 30.

let delta = Num.bp 5.

let big_delta = Num.bp 10.

let big_title s = tex ("\\textbf{\\Large{" ^ s ^ "}}")

let small_title s = tex ("\\textbf{\\emph{\\large{" ^ s ^ "}}}")

let external_color = Color.rgb8 255 165 0

let framac_color = Color.rgb8 50 205 50

let plugin_color = Color.lightcyan

let cil_color = Color.rgb8 250 128 114

let std_box ?color s = rect ~name:s ?fill:color (tex s)

let mk_services ?(big = false) ?color title b =
  round_rect ?fill:color ~name:title ~dx:padding
    ~dy:(if big then big_delta else delta)
    (vbox ~padding:big_delta
       [ (if big then big_title else small_title) title; b ])

let std_plugins =
  mk_services "Standard Plug-ins"
    (hbox ~padding [ std_box "Plug-in 1"; tex "\\dots"; std_box "Plug-in $n$" ])

let kernel_integrated_plugins =
  mk_services "Kernel-integrated Plug-ins"
    (hbox ~padding [ std_box "Plug-in 1"; tex "\\dots"; std_box "Plug-in $p$" ])

let plugins_types =
  mk_services "Kernel-integrated Plug-ins Types" ~color:plugin_color
    (hbox ~padding
       [ std_box "Plug-in types 1"; tex "\\dots"; std_box "Plug-in types $q$" ])

let kernel_frontend =
  mk_services ~color:framac_color "Plug-ins Values"
    (hbox ~padding [ std_box ~color:plugin_color "Db"; std_box "Dynamic" ])

let kernel_specific_services =
  mk_services "Specific Services"
    (vbox ~padding
       [
         hbox ~padding [ std_box "AST Manipulation"; std_box "Memory States" ];
         std_box "Abstract Interpretation Lattices";
         std_box "Utilities";
       ])

let kernel_general_services =
  mk_services "General Services"
    (vbox ~padding
       [
         std_box "Project";
         hbox ~padding [ std_box "Plugin"; std_box "Journal" ];
         std_box "Cmdline";
         hbox ~padding [ std_box "Type"; std_box "Log" ];
       ])

let cil =
  mk_services ~color:cil_color "Extended Cil"
    (vbox ~padding
       [
         std_box "Extended Cil API";
         rect ~name:"Cil Kernel"
           (vbox
              [
                tex "Extended Cil Kernel";
                tex "Lexing, Parsing, Typing, Linking";
              ]);
         std_box "Extended Cil AST";
       ])

let figure =
  vbox ~padding:big_padding
    [
      mk_services ~big:true "Plug-ins" ~color:plugin_color
        (hbox ~padding:big_padding [ std_plugins; kernel_integrated_plugins ]);
      mk_services ~big:true ~color:framac_color "Plug-ins API inside Frama-C"
        (hbox ~padding:big_padding [ kernel_frontend; plugins_types ]);
      mk_services ~big:true "Frama-C Kernel" ~color:framac_color
        (hbox ~padding:big_padding
           [ kernel_specific_services; kernel_general_services ]);
      cil;
    ]

let arrow ?(big = false) src dst =
  let getf s = get s figure in
  let src = getf src in
  let dst = getf dst in
  if big then Helpers.box_arrow ~color:Color.red ~pen:Pen.circle src dst
  else Helpers.box_arrow src dst

let cmds =
  Command.seq
    [
      draw figure;
      (* Kernel Specific Services *)
      (*
      arrow "AST Manipulation" "Abstract Interpretation Lattices";
      arrow "Memory States" "Abstract Interpretation Lattices";
      arrow "Abstract Interpretation Lattices" "Utilities";
      (* Kernel General Services *)
      arrow "Project" "Journal"; arrow "Journal" "Cmdline"; 
      arrow "Cmdline" "Type"; arrow "Plugin" "Cmdline";
      arrow "Cmdline" "Log";
      (* Extended Cil *)
      arrow "Extended Cil API" "Cil Kernel";
      arrow "Cil Kernel" "Extended Cil AST";
      (* inter-services arrow *)
      arrow ~big:true "Plug-ins" "Plug-ins API inside Frama-C";
      arrow ~big:true "Plug-ins Values" "Kernel-integrated Plug-ins Types";
      arrow ~big:true "Plug-ins API inside Frama-C" "Frama-C Kernel";
      arrow ~big:true "Specific Services" "General Services";
      arrow ~big:true "Specific Services" "Extended Cil";
      arrow ~big:true "Extended Cil" "General Services" ;
*)
    ]

let _ = Metapost.emit "architecture" cmds
