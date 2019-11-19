
(* This file was auto-generated based on "data_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 132 | 205 | 207 ->
        "A term, an arrow ('->' or '=>'), a left parenthesis '(', or a colon ':' are expected."
    | 100 | 108 | 125 ->
        "A term, a left parenthesis '(', or a colon ':' are expected.\n"
    | 69 | 83 ->
        "A term, a left parenthesis '(', or a semi-colon ';' are expected.\n"
    | 68 | 81 ->
        "A term, an arrow ('->' or '=>'), a left parenthesis '(', or a semi-colon ';' are expected.\n"
    | 14 | 17 ->
        "A type \"<type>\" is expected.\n"
    | 0 | 65 ->
        "A term \"<term>\" is expected.\n"
    | 23 | 26 ->
        "A term or a colon ':' are expected.\n"
    | 21 | 22 ->
        "A term or a right parenthesis ')' are expected.\n"
    | 4 | 64 | 66 ->
        "An identifier (the name of a bound variable) or a dot '.' are expected.\n"
    | 6 | 3 | 63 ->
        "An identifier (the name of a bound variable) is expected.\n"
    | 5 ->
        "A term is expected.\n"
    | 56 ->
        "An identifier (i.e., a type or a term) or a symbol are expected.\n"
    | 90 ->
        "A comma ',' or an interpretation symbol ':=' are expected.\n"
    | 91 ->
        "An identifier (i.e., a type or a term) or a symbol are expected.\n"
    | 62 | 60 | 27 ->
        "A term or a type are expected.\n"
    | 82 ->
        "An arrow ('->' or '=>'), or a semi-colon are expected.\n"
    | 41 | 44 ->
        "A type or an end of input are expected (no more keyword or semi-colon or colon).\n"
    | 48 ->
        "An arrow ('->' or '=>'), a right parenthesis ')', or an end of input are expected (no more keyword or semi-colon or colon).\n"
    | 29 | 30 | 31 | 32 | 33 | 36 | 49 | 50 | 53 ->
        "An arrow ('->' or '=>'), a left parenthesis '(', or an end of input are expected (no more keyword or semi-colon or colon).\n"
    | 88 | 61 ->
        "An arrow ('->' or '=>'), a term, or a semi-colon are expected.\n"
    | 77 | 74 ->
        "A type expression is expected.\n"
    | 95 ->
        "An equality symbol '=' is expected.\n"
    | 152 | 96 | 151 ->
        "A signature entry (type declaration, type definition, term declaration, or term definition) is expected.\n"
    | 93 ->
        "A declaration of a signature (keyword 'signature') or of a lexicon (keyword 'lexicon' or 'nl_lexicon') is expected.\n"
    | 161 ->
        "An identifier (the name of a new lexicon) is expected.\n"
    | 162 ->
        "A left parenthesis '(' is expected.\n"
    | 164 ->
        "A right parenthesis ')' is expected.\n"
    | 165 | 9 ->
        "A colon ':' is expected.\n"
    | 166 ->
        "An identifier (the name of a signature) is expected.\n"
    | 167 ->
        "An equality symbol '=' is expected.\n"
    | 169 ->
        "A semi-colon ';' or the 'end' keyword are expected.\n"
    | 175 ->
        "An identifier (the name of a new lexicon) is expected\n"
    | 176 ->
        "A left parenthesis '(' is expected.\n"
    | 177 ->
        "An identifier (the name of a signature) is expected.\n"
    | 178 ->
        "A right parenthesis ')' is expected.\n"
    | 179 ->
        "A expression in the form of \": <identifier> =\" where the identifier is the name of a signature is expected.\n"
    | 180 | 163 | 94 ->
        "An identifier (the name of a signature) is expected.\n"
    | 181 ->
        "An equality symbold '=' is expected.\n"
    | 182 | 170 | 168 ->
        "A lexicon entry of the form \"<term> := <term>;\" or \"<type> := <type>\" is expected.\n"
    | 185 | 184 ->
        "An expression representing the composition of lexicons is expected.\n"
    | 190 ->
        "The composition operator '<<' or a right parenthesis ')' is expected.\n"
    | 187 | 194 ->
        "The composition operator '<<' is expected.\n"
    | 188 ->
        "An identifier (the name of a lexicon), or an expression representing the composition of lexicons is expected.\n"
    | 200 ->
        "An identifier or a keyword ('infix', 'prefix', or 'binder') is expected.\n"
    | 97 ->
        "A symbol is expected.\n"
    | 113 ->
        "An associativity specification (one of the keywords 'Left', 'Right', or 'NonAssoc') or a precedence specification in the form of \"< <sym>\" where \"<sym>\" is an other infix symbol are expected.\n"
    | 117 ->
        "A right square bracket ']' or a comma \",\" followed by a precedence specification in the form of \"< <sym>\" where \"<sym>\" is an other infix symbol are expected.\n"
    | 116 ->
        "An associativity specification (one of the keywords 'Left', 'Right', or 'NonAssoc') is expected.\n"
    | 118 ->
        "A precedence specification in the form of \"< <sym>\" where \"<sym>\" is an other infix symbol is expected.\n"
    | 115 ->
        "A right square bracket ']' or a comma ',' followed by an associativity specification (one of the keywords 'Left', 'Right', or 'NonAssoc') are expected.\n"
    | 114 ->
        "An identifier is expected.\n"
    | 105 | 122 ->
        "A symbol or a declaration of associativity an precedence property are expected.\n"
    | 106 | 98 | 123 ->
        "A typing judgement in the form of \": <type>;\" or a defintion in the form of \"= <term>: <type>;\" is expected.\n"
    | 107 | 99 | 124 ->
        "A typing judgment in the form \"<term> : <type>;\" is expected.\n"
    | 1 ->
        "A typing judgment in the form \": <type>;\" is expected.\n"
    | 111 | 109 | 103 | 101 | 126 | 128 ->
        "A type is expected after the colon ':'.\n"
    | 130 ->
        "A comma ',' or a colon ':' are expected in a type or term declaration. An equality symbol '=' is expected in a type or term definition.\n"
    | 131 ->
        "A definition in the form of \"<term> : <type>;\" or a type definition of the form \"<type> : type;\" (with the keyword 'type') is expected after a term or a type defintion, resp.\n"
    | 137 | 136 | 154 | 202 ->
        "After a term or type declaration of the form \"<ident1>, <ident2>\", a type declaration of the form \": type;\" (with the keyword 'type') or a typing judgment of the form \": <type>;\" is expected.\n"
    | 139 ->
        "An identidier (the name of the binder) is expected after the keyword 'binder'.\n"
    | 140 ->
        "A typing judgement in the form of \": <type>\" or a definition in the form of \"= <term> : <type>\" is expected after the declaration of a binder.\n"
    | 141 ->
        "A term is expected as right hand side of a term definition.\n"
    | 142 ->
        "A term, a left parenthesis '(', or a colon ':' for a typing judgment in the form of \": <type>\" is expected after defining a binder.\n"
    | 204 | 2 ->
        "A typing judgment in the form of \"<term> : <type>\" is expected.\n"
    | 8 ->
        "A typing judgement in the form of \": <type>;\", or a type definition with a colon ':' and the 'type' keyword in the form of \": type;\" are expected in a term or a type definition.\n"
    | 35 | 206 | 145 | 143 | 133 | 28 ->
        "A type expression is expected after ':'.\n"
    | _ ->
        raise Not_found
