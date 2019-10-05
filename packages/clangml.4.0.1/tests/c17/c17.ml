let lift_expr = new Clangml_lift.lift_expr Location.none

let check_pattern_expr (e : Clang.Ast.expr)
    (pattern : (Clang.Ast.expr, 'a) Pattern_runtime.matcher) =
  match pattern ~quoted:(lift_expr#expr e) e with
  | Ok result -> result
  | Error failure ->
      Format.fprintf Format.err_formatter "%a@."
        Pattern_runtime.pp_failure failure;
      failwith "check_pattern_expr"

let run_llvm_config llvm_config arguments =
  let command = String.concat " " (llvm_config :: arguments) in
  let output = Unix.open_process_in command in
  let result = input_line output in
  if Unix.close_process_in output <> Unix.WEXITED 0 then
    failwith (Printf.sprintf "%s: execution failed" command);
  result

let parse_string s =
  let command_line_args =
    List.map Clang.Command_line.include_directory
      (Clang.default_include_directories ()) in
  Clang.Ast.parse_string ~command_line_args s

(* 6.4.2.2 Predefined identifiers *)

let () =
  let ast = parse_string {|
    #include <stdio.h>
    void myfunc(void)
    {
      printf("%s\n", __func__ );
      /* ... */
    }
  |} in
  let f = List.hd (List.rev ast.desc.items) in
  match f with
  | [%c-d {| int printf(const char *, ...); |} {|
      void myfunc(void)
      {
        printf("%s\n", [%(char * )? e]);
        /* ... */
      }
    |}] ->
    check_pattern_expr e [%pattern? {
      desc = Predefined {
        kind = Func;
        function_name = "myfunc" } }]
  | _ -> assert false

(* 6.5 Expressions *)

(* 6.5.1.1 Generic selection *)

let () =
  let ast = parse_string {|
    #include <stdio.h>
    #include <math.h>

    #define cbrt(X) _Generic((X), \
      long double: cbrtl,         \
      default: cbrt,              \
      float: cbrtf                \
      )(X)

    int main(void)
    {
      printf("cbrt(729) = %f\n", cbrt(729));
      return 0;
    }
    |} in
  let f = List.hd (List.rev ast.desc.items) in
  match f with
  | [%c-d {|
      float cbrtf(float arg);
      double cbrt(double arg);
      long double cbrtl(long double arg);
      int printf(const char *, ...);
    |} {|
      int main(void)
      {
        printf("cbrt(729) = %f\n", [%(float(*)(float))? e](729));
        return 0;
      }
    |}] ->
    check_pattern_expr e [%pattern? {
      desc = GenericSelection {
        controlling_expr = { desc = IntegerLiteral (Int 729) };
        assocs = [
          (Some { desc = BuiltinType LongDouble }, { desc = DeclRef { name = IdentifierName "cbrtl" }});
          (None, { desc = DeclRef ({ name = IdentifierName "cbrt" })});
          (Some { desc = BuiltinType Float }, { desc = DeclRef { name = IdentifierName "cbrtf" }})]}}]
  | _ -> assert false
