[@@@ocaml.warning "-9"]

let has_errors = ref false

let check_translation_decl source target =
  let transformed = For_memcad.transform_decl source in
  if Refl.compare [%refl: Clang.Ast.decl] [] transformed target <> 0 then
    begin
      Format.eprintf "@[From:@ %a@]@.@[Got:@ %a@]@.@[Expected:@ %a@]@."
        (Refl.pp [%refl: Clang.Ast.decl] []) source
        (Refl.pp [%refl: Clang.Ast.decl] []) transformed
        (Refl.pp [%refl: Clang.Ast.decl] []) target;
      has_errors := true
    end

let check_translation source target =
  let source_items = (Clang.Ast.parse_string source).desc.items in
  let target_items = (Clang.Ast.parse_string target).desc.items in
  List.iter2 check_translation_decl source_items target_items

let () =
  match Clang.Ast.parse_string "int g(void); void f(void) { g(); }" with
  | { desc = { items = [ _; decl ]}} ->
      begin
        match For_memcad.transform_decl decl with
        | { desc = Function { name = IdentifierName "f";
            body = Some { desc = Compound [
            { desc = Decl [{ desc = Var _ }]};
            { desc = Expr { desc = BinaryOperator { kind = Assign; rhs =
              { desc = Call {
                callee = { desc = DeclRef { name = IdentifierName "g" }};
                args = []}}}}}]}}} -> ()
        | decl ->
            Format.fprintf Format.err_formatter "%a\n"
              (Refl.pp [%refl: Clang.Ast.decl] []) decl;
            assert false
      end
  | _ -> assert false

let () =
  match
    Clang.Ast.parse_string
      {|
      void f(void) {
        if (0 == 1 && 2 == 3 || 4 == 5) {
          "true";
        }
        else {
          "false";
        }
      } |}
  with
  | { desc = { items = [ decl ]}} ->
      begin
        match For_memcad.transform_decl decl with
        | { desc = Function { name = IdentifierName "f";
            body = Some { desc = Compound [
            { desc = If {
              cond = {
                desc = BinaryOperator { kind = EQ;
                lhs = { desc = IntegerLiteral (Int 0)};
                rhs = { desc = IntegerLiteral (Int 1)}}};
              then_branch = {
                desc = If {
                cond = {
                  desc = BinaryOperator { kind = EQ;
                  lhs = { desc = IntegerLiteral (Int 2)};
                  rhs = { desc = IntegerLiteral (Int 3)}}};
                then_branch = { desc = Expr {
                  desc = StringLiteral { bytes = "true" } }};
                else_branch = Some {
                  desc = If {
                  cond = {
                    desc = BinaryOperator { kind = EQ;
                    lhs = { desc = IntegerLiteral (Int 4)};
                    rhs = { desc = IntegerLiteral (Int 5)}}};
                  then_branch = { desc = Expr {
                    desc = StringLiteral { bytes = "true" }}};
                  else_branch = Some {
                    desc = Expr {
                      desc = StringLiteral { bytes = "false" }}}}}}};
              else_branch = Some {
                desc = If {
                cond =
                  { desc = BinaryOperator { kind = EQ;
                    lhs = { desc = IntegerLiteral (Int 4)};
                    rhs = { desc = IntegerLiteral (Int 5)}}};
                then_branch = { desc = Expr {
                  desc = StringLiteral { bytes = "true" }}};
                else_branch = Some
                  { desc = Expr {
                    desc = StringLiteral { bytes = "false"}}}}}}}]}}} -> ()
        | decl ->
            Format.fprintf Format.err_formatter "%a\n"
              (Refl.pp [%refl: Clang.Ast.decl] []) decl;
            assert false
      end
  | _ -> assert false

let () =
  match
    Clang.Ast.parse_string
      {|
      void f(void) {
        int *res = (int *) malloc(sizeof(int));
      } |}
  with
  | { desc = { items = [ decl ]}} ->
      begin
        match For_memcad.transform_decl decl with
        | { desc = Function { name = IdentifierName "f";
            body = Some { desc = Compound [
            { desc = Decl [{ desc = Var { var_name = "res" }}]};
            { desc = Expr { desc = BinaryOperator {
              kind = Assign;
              lhs = { desc = DeclRef { name = IdentifierName "res" }};
              rhs = { desc = Call {
                callee = { desc = DeclRef { name = IdentifierName "malloc" }};
                args = [{ desc = IntegerLiteral (Int 4)}]}}}}}]}}} -> ()
        | decl ->
            Format.fprintf Format.err_formatter "%a\n"
              (Refl.pp [%refl: Clang.Ast.decl] []) decl;
            assert false
      end
  | _ -> assert false

let () =
  check_translation
    {|
      int g(void);
      void f(void)
      {
        int res = 1 ? g() : 0;
      }
    |} {|
      int g(void);
      void f(void)
      {
        int __tmp_1;
        if (1)
          __tmp_1 = g();
        else
          __tmp_1 = 0;
        int res;
        res = __tmp_1;
      }
    |}

let () =
  check_translation
    {| 
      double *new_float (void)
      {
        return malloc(sizeof(double));
      }
    |} {| 
      double *new_float (void)
      {
        {
          double *__tmp_2;
          __tmp_2 = malloc(8);
          return __tmp_2;
       }
      }
    |}

let () =
  check_translation
    {|
      typedef enum {
        FALSE = 0,
        TRUE = 1
      } bool;
      bool f(void)
      {
        return (bool) (0 == 1);
      }
    |} {|
      typedef enum {
        FALSE = 0,
        TRUE = 1
      } bool;
      bool f(void)
      {
        {
          bool __tmp_3;
          if (0 == 1)
            __tmp_3 = 1;
          else
            __tmp_3 = 0;
          int __tmp_4;
          __tmp_4 = __tmp_3;
          return __tmp_4;
        }
      }
    |}

let () =
  check_translation
    {|
      int g(void);
      void f(void)
      {
        if (g() == 0) {
          return;
        }
      }
    |} {|
      int g(void);
      void f(void)
      {
        {
          int __tmp_5;
          __tmp_5 = g();
          if (__tmp_5 == 0)
            return;
       }
      }
    |}

let () =
  if !has_errors then
    exit 1
