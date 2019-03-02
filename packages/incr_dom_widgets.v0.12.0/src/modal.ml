open! Core_kernel
open Incr_dom.Vdom

let modal ~on_close ~modal_body =
  Node.div
    [ Attr.classes [ "modal-overlay"; "active" ] ]
    [ Node.div
        [ Attr.class_ "modal-container" ]
        [ Node.div
            [ Attr.class_ "modal-header" ]
            [ Node.div
                [ Attr.classes [ "clickable"; "modal-close" ]
                ; Attr.on_click (fun ev -> on_close ev)
                ]
                [ Node.span [] [ Node.text "X" ] ]
            ]
        ; Node.div
            [ Attr.class_ "modal-body" ]
            [ Node.div [ Attr.class_ "center" ] [ modal_body ] ]
        ]
    ]
;;
