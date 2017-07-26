%{
open! Core

%}

/*(* Standard tokens *)*/
%token <string> ERROR
%token EOF

/*(* Headers *)*/
%token <string * string> FIELD
%token HEADER_END
/**/
%token NO_HEADER_END

/*(* Body *)*/
/*(* %token <string> OCTET_STREAM *)*/
%token <int> OCTET_STREAM_OFFSET

%start message
%type <Email_grammar_types.message> message

%start only_header
%type <Email_grammar_types.header> only_header

%%

message : part EOF { $1 };

part : header HEADER_END OCTET_STREAM_OFFSET
    { `Message ($1, `Content_offset $3) }
/**/
| header NO_HEADER_END OCTET_STREAM_OFFSET
    { `Message ($1, `Bad_headers $3) }
| header
  { `Message ($1, `Truncated) }
  ;
only_header : header EOF { $1 };

header :
  FIELD header { ($1 :: $2) }
  | { [] }
;

%%


