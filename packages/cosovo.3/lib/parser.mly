%{

%}

%token <string> STRING
%token <int> NEG_INT
%token <int> POS_INT
%token <float> FLOAT

%token LCURLY
%token RCURLY
%token COMMA
%token EOL
%token EOF
%token <string> COMMENT

%start header
%start row
%start row_sans_nl

%type <Types.header option> header
%type <Types.row> row
%type <Types.row> row_sans_nl

%%

header:
| header_sans_nl EOL { $1 }
| header_sans_nl COMMENT EOL { $1 }
| newlines header_sans_nl EOL { $2 }
| newlines header_sans_nl COMMENT EOL { $2 }
| newlines EOF { None }
| EOF { None }

header_sans_nl:
| dense_header { Some (`Dense $1) }
| sparse_header { Some (`Sparse $1) }
| dense_header COMMENT { Some (`Dense $1) }
| sparse_header COMMENT { Some (`Sparse $1) }

sparse_header:
| LCURLY is_pairs RCURLY { $2 }
| LCURLY RCURLY { [] }

is_pairs:
| is_pair COMMA is_pairs { $1 :: $3 }
| is_pair { [ $1 ] }

is_pair:
| POS_INT STRING { $1, $2 }

dense_header:
| STRING COMMA dense_header { $1 :: $3 }
| STRING { [ $1 ] }

value:
| NEG_INT { (`Int $1) }
| POS_INT { (`Int $1) }
| FLOAT { (`Float $1) }
| STRING { (`String $1) }

values:
| value COMMA values { $1 :: $3 }
| value { [ $1 ] }

row:
| row_sans_nl EOL { $1 }
| row_sans_nl COMMENT EOL { $1 }
| newlines row_sans_nl EOL { $2 }
| newlines row_sans_nl COMMENT EOL { $2 }
| newlines EOF { `EOF }
| EOF { `EOF }

row_sans_nl:
| dense_row { $1 }
| sparse_row { `Sparse $1 }
| dense_row COMMENT { $1 }
| sparse_row COMMENT { `Sparse $1 }

dense_row:
| values { `Dense $1 }

sparse_row:
| LCURLY pairs RCURLY { $2 }
| LCURLY RCURLY { [] }

pairs:
| pair COMMA pairs { $1 :: $3 }
| pair { [ $1 ] }

pair:
| POS_INT value { $1, $2 }

newlines:
| EOL { () }
| EOL newlines { () }
| COMMENT newlines { () }
