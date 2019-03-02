open! Core_kernel
open Incr_dom.Vdom

let circular =
  Node.svg
    "svg"
    [ Attr.class_ "spinner" ]
    [ Node.svg
        "circle"
        [ Attr.create "r" "30"; Attr.create "cx" "33"; Attr.create "cy" "33" ]
        []
    ]
;;

let logo =
  Node.svg
    "svg"
    [ Attr.class_ "logo-spinner" ]
    [ Node.svg
        "circle"
        [ Attr.create "r" "17"; Attr.create "cx" "33"; Attr.create "cy" "33" ]
        []
    ; Node.svg
        "circle"
        [ Attr.create "r" "23.5"; Attr.create "cx" "33"; Attr.create "cy" "33" ]
        []
    ; Node.svg
        "circle"
        [ Attr.create "r" "30"; Attr.create "cx" "33"; Attr.create "cy" "33" ]
        []
    ]
;;
