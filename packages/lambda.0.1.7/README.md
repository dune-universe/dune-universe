# λ-calculus ocaml library

Install with: ```opam install lambda```

Documentation is available at [https://dakk.github.com/lambda](https://dakk.github.com/lambda).


```ocaml
let t = Abs("x",Abs("y",Var "x"));; 
let f = Abs("x",Abs("y",Var "y"));;
(* (λx.(λy.((y x) y))) *)
let fand = (Abs ("x", (Abs("y", App(App(Var "y", Var "x"), Var "y"))))) in
let fand_app a b = App(App(fand, Bool.of_bool a), Bool.of_bool b) in
Printf.printf "%b and %b => %b\n" true true (Bool.to_bool @@ L.reduce_fix @@ fand_app true true);
Printf.printf "%b and %b => %b\n" true false (Bool.to_bool @@ L.reduce_fix @@ fand_app true false);
```


## License

```
Copyright (c) 2020 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```