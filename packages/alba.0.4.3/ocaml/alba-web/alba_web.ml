open Fmlib
open Common


module Browser = Fmlib_js.Browser

module App = Web_application.Make (Browser)

module Vdom = App.Dom

module Attribute = App.Attribute

module Command = App.Command


module Program = Browser.Make (App)




module Compiler =
struct
    open Alba_core
    open Albalib

    module Expression = Ast.Expression

    module Pretty_printer =
        Pretty_printer.Pretty (String_printer)

    module Term_print =
        Context.Pretty (Pretty_printer)

    module Expression_parser =
        Parser_lang.Make (Expression)

    module Result = Monad.Result (String)



    let string_of_printer (p: Pretty_printer.t): string =
        String_printer.run (Pretty_printer.run 0 70 70 p)

    let standard_context: Context.t =
        Standard_context.make ()


    let parse (input: string): (Expression.t, string) result =
        let open Expression_parser in
        let p = run (expression ()) input in
        assert (has_ended p);
        match result p with
        | Some exp ->
            Ok exp
        | None ->
            let module Error_printer =
                Expression_parser.Error_printer (Pretty_printer)
            in
            Error (
                string_of_printer
                    (Error_printer.print_with_source input p)
            )



    let build (input: string) (evaluate: bool): (string, string) result =
        let open Result in
        parse input >>= fun exp ->
        match Build_expression.build exp standard_context with
        | Ok (term, typ) ->
            let module P = Context.Pretty (Pretty_printer) in
            Ok (
                string_of_printer
                    (
                        let term =
                            if evaluate then
                                Context.compute
                                    term
                                    standard_context
                            else
                                term
                        in
                        P.print
                            Term.(Typed (term, typ))
                            standard_context
                    )
            )
        | Error (range, description) ->
            let module PP =  Build_problem.Print (Pretty_printer) in
            let module PP0 = Printer.Make (Pretty_printer) in
            Error (
                string_of_printer
                    Pretty_printer.(
                        PP0.print_error_header "TYPE"
                        <+>
                        PP0.print_source input range []
                        <+>
                        PP.description description
                    )
            )
end



type show =
    | Output
    | Error
    | Info


type model = {
    expression: string;
    result: string;
    error: string;
    info: string;
    show: show;
}


type message =
    | Typecheck
    | Evaluate
    | Show of show
    | New_expression
    | Update_expression of string
    | Update_info of string




let init (url: string): model * message Command.t =
    { expression = ""
    ; result = ""
    ; error = ""
    ; info = "loading <" ^ url ^ ">"
    ; show = Output
    },
    Command.http_get
        url
        (function
            | Ok response ->
                Update_info response
            | Error code ->
                Update_info ("Error: " ^ string_of_int code
                            ^ "\n Cannot load <" ^ url ^ ">"))



let update (msg: message) (model: model): model * 'message Command.t =
    let build model evaluate =
        if model.expression = "" then
            model, Command.None
        else
            match Compiler.build model.expression evaluate with
            | Ok result ->
                {model with
                    result;
                    error  = "";
                    show   = Output},
                Command.None
            | Error error ->
                {model with
                    result = "";
                    error;
                    show   = Error},
                Command.None
    in
    match msg with
    | Typecheck ->
        build model false

    | Evaluate ->
        build model true

    | Show show ->
        {model with show}, Command.None

    | Update_expression expression ->
        {model with expression}, Command.None

    | New_expression ->
        {model with expression = ""}, Command.None

    | Update_info info ->
        {model with info; show = Info}, Command.None




let menu_item
    (show: show)
    (my_show: show)
    (str: string)
    : message Vdom.t
    =
    let open Vdom in
    let open Attribute in
    if show = my_show then
        li [ class_ "menu-item"
           ; onClick (Show my_show)
           ; style "background-color" "lightgrey"]
           [text str]
    else
        li [ class_ "menu-item"
           ; onClick (Show my_show)
           ]
           [text str]


let output (model: model): message Vdom.t =
    let open Vdom in
    let open Attribute in
    let class_attr, str =
        match model.show with
        | Output ->
            "output-result", model.result
        | Error ->
            "output-error" , model.error
        | Info ->
            "output-info"  , model.info
    in
    pre [class_ class_attr] [text str]




let view (model: model): message Vdom.t =
    let open Vdom in
    let open Attribute in
    div []
        [ h2 [] [ text "Alba Expression Compiler"]
        ; span  [ class_ "main-block" ]
                [ div []
                      [(* button [] [text "<"]
                      ; button [] [text ">"]
                      ;*) button [onClick New_expression] [text "New"]
                      ; button [onClick Typecheck] [text "Typecheck"]
                      ; button [onClick Evaluate]  [text "Evaluate"]
                      ]
                ; div []
                      [textarea [ class_ "editor-area"
                                ; placeholder
                                    "enter expression ... \
                                    (for examples see \"Info\" \
                                    on the right hand side)"
                                ; value model.expression
                                ; onInput (fun str -> Update_expression str)
                                ]
                                []
                      ]
                ]
        ; span  [ class_ "main-block" ]
                [ ul [ class_ "menu" ]
                     [ menu_item model.show Output "Output"
                     ; menu_item model.show Error "Error"
                     ; menu_item model.show Info "Info"
                     ]
                ; output model
                ]
        ]



let subscription (_: 'model): 'msg App.Subscription.t =
    App.Subscription.None



let _ =
    Program.element
        Browser.Decoder.string
        init
        view
        update
        subscription
