open Clang
open Clang.Ast

let () =
  Format.printf "%a@." Clangml_printer.translation_unit [%c-tu {|int x = 4;|}]

let () =
  Format.printf "%a@." Clangml_printer.translation_unit [%c-tu {|
int f(int x) {
  x++;
  return 0;
}
|}]

let () =
  Format.printf "%a@." Clangml_printer.expr [%c-e {|1 + 1|}]

let () =
  Format.printf "%a@." Clangml_printer.expr [%c-e (int x) {|++x|}]

let () =
  Format.printf "%a@." Clangml_printer.qual_type [%c-t {|void *|}]

let () =
  Format.printf "%a@." Clangml_printer.expr [%c-e {|
    1 + [%int Clang.Ast.node (Clang.Ast.IntegerLiteral (Int 42))]
  |}]

let () =
  Format.printf "%a@." Clangml_printer.qual_type [%c-t {|
    [%typename Clang.Type.make (BuiltinType Int)] *
  |}]

let () =
  Format.printf "%a@." Clangml_printer.qual_type [%c-t {a|
    [%typename Clang.Type.make (ConstantArray { element = [%c-t {| int |}]; size = 2 })] *
  |a}]

let () =
  Format.printf "%a@." Clangml_printer.stmt [%c-s (return int) {|return 1;|}]

let () =
  match [%c-e {| 1 + 1 |}] with
  | [%c-e {| 1 + 2 |}] -> assert false
  | [%c-e {| 1 + 1 |}] -> ()
  | _ -> assert false

let () =
  match [%c-tu {| int x = 4; |}] with
  | [%c-tu {| [%typename? ty] x = [%(void *)? value]; |}] ->
      Format.printf "%a@." Clangml_printer.qual_type ty;
      Format.printf "%a@." Clangml_printer.expr value
  | _ -> assert false

let () =
  match [%c-e {| (void *) 0 |}] with
  | [%c-e {| (void *) 0 |}] -> ()
  | _ -> assert false

let () =
  match [%c-e {| (int) 'a' |}] with
  | [%c-e {| ([%typename? _]) [%(void *)? value] |}] ->
      Format.printf "%a@." Clangml_printer.expr value
  | _ -> assert false

let () =
  match [%c-e {| 0 == 1 ? "a" : "b" |}] with
  | [%c-e {| [%int? condition] ? [%(void *)? then_branch] : [%(void *)? else_branch] |}] ->
      Format.printf "%a@." Clangml_printer.expr condition;
      Format.printf "%a@." Clangml_printer.expr then_branch;
      Format.printf "%a@." Clangml_printer.expr else_branch
  | _ -> assert false
      

let () =
  Format.printf "%a@." Clangml_printer.translation_unit [%cpp-tu {|
    class X {
    public:
      int x;
      X() { x = 0; }
    };
  |}]
