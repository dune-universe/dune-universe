%parameter<Parse_extended : sig
  val main: int -> int -> Ast.extended_attribute
end>

%%

%public extendedAttributeList :
    | LBRACKET extendedAttribute extendedAttributes RBRACKET 
    { (Parse_extended.main $startofs($2) $endofs($2)) :: $3 }
    | emptyExtendedAttributeList { $1 } /* support for non standard Web IDL */
    |  { [] }

extendedAttributes :
    | COMMA extendedAttribute extendedAttributes 
    { (Parse_extended.main $startofs($2) $endofs($2)) :: $3 }
    | commnaEnd { $1 } /* support for non standard Web IDL */
    |  { [] }

extendedAttribute :
    | LPAR extendedAttributeInner RPAR extendedAttributeRest { () }
    | LBRACKET extendedAttributeInner RBRACKET extendedAttributeRest { () }
    | LBRACE extendedAttributeInner RBRACE extendedAttributeRest { () }
    | other extendedAttributeRest { () }

extendedAttributeRest :
    | extendedAttribute { () }
    | { () }
    
extendedAttributeInner :
    | LPAR extendedAttributeInner RPAR extendedAttributeInner { () }
    | LBRACKET extendedAttributeInner RBRACKET extendedAttributeInner { () }
    | LBRACE extendedAttributeInner RBRACE extendedAttributeInner { () }
    | otherOrComma extendedAttributeInner { () }
    |  { () }

other :
    | INTVAL { () }
    | FLOATVAL { () }
    | IDENTIFIER { () }
    | STRING { () }
    | OTHER { () }
    | MINUS { () }
    | MINUSINFINITY { () }
    | DOT { () }
    | ELLIPSIS { () }
    | COLON { () }
    | SEMICOLON { () }
    | LT { () }
    | EQUAL { () }
    | GT { () }
    | QUESTION { () }
    | BYTESTRING { () }
    | DOMSTRING { () }
    | FROZENARRAY { () }
    | INFINITY { () }
    | NAN { () }
    | USVSTRING { () }
    | ANY { () }
    | BOOLEAN { () }
    | BYTE { () }
    | DOUBLE { () }
    | FALSE { () }
    | FLOAT { () }
    | LONG { () }
    | NULL { () }
    | OBJECT { () }
    | OCTET { () }
    | OR { () }
    | OPTIONAL { () }
    | SEQUENCE { () }
    | SHORT { () }
    | TRUE { () }
    | UNSIGNED { () }
    | VOID { () }
    | argumentNameKeyword { () }
    | bufferRelatedType { () }

otherOrComma :
    | other { () }
    | COMMA { () }



