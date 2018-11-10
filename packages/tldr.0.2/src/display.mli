module Parser : sig
  type example =
    | Command of string
    | Argument of string
  
  type expression =
    | Title of string
    | Description of string
    | Example of string * (example list)

  val parse : String.t -> expression list
end

module Colors : sig
  val color_example : Parser.example -> String.t
  
  val color_expression : Parser.expression -> String.t
end

  
val display : string -> Base.unit                                        
