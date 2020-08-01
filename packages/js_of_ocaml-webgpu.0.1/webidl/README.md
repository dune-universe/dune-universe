# Webidl
 a parser of [Web IDL](https://heycam.github.io/webidl)   
 latest version v1.4 surrpots Web IDL Editor’s Draft, 6 August 2018

## Installation
opam install webidl

## Usage example
* [gen-bs](https://github.com/0zat/gen-bs) is a sample application which uses this library
  * convert Web IDL files to a [bucklescript](https://github.com/bucklescript/bucklescript) code
* an example code is the below.  
```ocaml
let () =
  try
    Webidl.Parse.data_from_file "file.idl" (* 1. *)
    |> Webidl.Parse.show_data (* 2. *)
    |> print_string
  with
  | Webidl.Parse.Syntax_error e -> (* 3. *)
    print_string "[!]Syntax Error\nInfo:\n";
    Webidl.Parse.show_syntax_error e (* 4. *)
    |> print_string
    |> print_newline
```
1. `Webidl.Parse.data_from_file`  
This function parses webidl from file and generate OCaml data which expresses the webidl.  
The OCaml data is difined in src/lib/data.ml. Please see it if you would like to know details.  
You can access webidl by this result.

2. `Webidl.Parse.show_data`  
print parsed webidl data.

3. `Webidl.Parse.Syntax_error e`  
if there is an syntax error, `exception Syntax_error` is raised. 
`e` is a record which is defined in src/lib/parse.ml   

4. `Webidl.Parse.show_syntax_error`  
print infomation of parse error
