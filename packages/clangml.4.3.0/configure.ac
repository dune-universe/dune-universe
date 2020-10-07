AC_INIT([[clangml]], [m4_esyscmd([cat VERSION])],
  [[Thierry.Martinez@inria.fr]], [[clangml]],
  [[https://github.com/thierry.martinez/clangml/]])
AM_INIT_AUTOMAKE([foreign]) dnl to allow README.md
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_LN_S

AC_MSG_CHECKING([for bootstrap/ sub-directory])
AS_IF([test -d $srcdir/bootstrap],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])
   AC_MSG_ERROR([bootstrap/ sub-directory not found. Please run $srcdir/bootstrap.sh first.])])

AC_DEFUN([AC_OCAMLFIND_PKG],
  [AC_MSG_CHECKING([for ocamlfind package $2])
   AS_IF([$OCAMLFIND query $2 >/dev/null 2>&1],
     [AC_MSG_RESULT([yes])
      AC_SUBST([$1], [$2])],
     [AC_MSG_RESULT([no])
      AC_SUBST([$1], [])])])

AC_SUBST([missing_packages], [])
AC_SUBST([has_missing_packages], [no])

AC_DEFUN([AC_CHECK_OCAMLFIND_PKG],
  [AC_MSG_CHECKING([for ocamlfind package $1])
   AS_IF([$OCAMLFIND query $1 >/dev/null 2>&1],
     [AC_MSG_RESULT([yes])],
     [AC_MSG_RESULT([no])
      AC_SUBST([missing_packages], [["$missing_packages $1"]])
      AS_IF([test "x$has_missing_packages" = "xno"],
        [AC_SUBST([has_missing_packages], [single])],
        [AC_SUBST([has_missing_packages], [multiple])])])])

AC_CHECK_TOOL([OCAMLFIND], [ocamlfind], [])
AS_IF([test "x$OCAMLFIND" = "x"],
  [AC_MSG_ERROR([ocamlfind is missing. Hint: opam install ocamlfind.])])

AC_SUBST([OCAMLC], [["$OCAMLFIND ocamlc"]])
AC_SUBST([OCAMLOPT], [["$OCAMLFIND ocamlopt"]])
AC_SUBST([OCAMLDOC], [["$OCAMLFIND ocamldoc"]])
AC_SUBST([OCAMLDEP], [["$OCAMLFIND ocamldep"]])
AC_SUBST([OCAMLMKLIB], [["$OCAMLFIND ocamlmklib"]])
AC_CHECK_OCAMLFIND_PKG([stdcompat])
AC_CHECK_OCAMLFIND_PKG([ppxlib])
AC_CHECK_OCAMLFIND_PKG([refl])

AS_IF([test "x$has_missing_packages" = "xsingle"],
  [AC_MSG_ERROR([Package$missing_packages not found. Hint: opam install$missing_packages.])],
  [AS_IF([test "x$has_missing_packages" = "xmultiple"],
    [AC_MSG_ERROR([Packages not found:$missing_packages. Hint: opam install$missing_packages.])])])

AS_IF([test "x$OCAMLOPT" = "x"],
  [AS_IF([test "x$OCAMLC" = "x"],
     [AC_MSG_ERROR([[No OCaml compiler available]])],
     [AC_SUBST([OCAMLBEST], [["$OCAMLC"]])
      AC_SUBST([CMOX], [["cmo"]])
      AC_SUBST([CMAX], [["cma"]])])],
  [AC_SUBST([OCAMLBEST], [["$OCAMLOPT"]])
   AC_SUBST([CMOX], [["cmx"]])
   AC_SUBST([CMAX], [["cmxa"]])])

AS_IF([test "x$OCAMLDEP" = "x"],
  [AC_MSG_ERROR([[No ocamldep available]])])

AC_MSG_CHECKING([OCaml version])
AC_SUBST([OCAMLVERSION],
  [[`$OCAMLBEST -v | sed -n 's/^.*version \(.*\)$/\1/p'`]])
AC_MSG_RESULT([$OCAMLVERSION])

AM_CONDITIONAL(OCAML_SUPPORTS_BYTECODE, [test "x$OCAMLC" != "x"])

AM_CONDITIONAL(OCAML_SUPPORTS_NATIVE, [test "x$OCAMLOPT" != "x"])

AC_CHECK_TOOL([DUNE], [dune], [])

AS_IF([test "x$DUNE" = "x"],
  [AC_MSG_ERROR([[dune not available]])])

AC_CHECK_TOOL([OCAMLCODOC], [ocamlcodoc], [])

AC_CHECK_TOOL([INSTALL_NAME_TOOL], [install_name_tool], [])

AC_CHECK_TOOL([BREW], [brew], [])

AC_DEFUN([AC_CHECK_LLVM_VERSION],
  [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-"$1"`]])
   AS_IF([test "x$LLVM_CONFIG" = "x"],
     [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-"$1".0`]])
      AS_IF([test "x$LLVM_CONFIG" = "x"],
        [AC_SUBST([LLVM_CONFIG], [[`which llvm-config"$1"0`]])
         AS_IF([test "x$LLVM_CONFIG" = "x"],
           [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-mp-"$1"`]])
            AS_IF([test "x$LLVM_CONFIG" = "x"],
              [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-mp-"$1".0`]])
               AS_IF([test "x$LLVM_CONFIG" = "x"],
                 [AC_SUBST([GENTOO_LLVM_CONFIG], [[/usr/lib64/llvm/$1/bin/llvm-config]])
                  AS_IF([$GENTOO_LLVM_CONFIG --version >/dev/null 2>/dev/null],
                    [AC_SUBST([LLVM_CONFIG], [[$GENTOO_LLVM_CONFIG]])],
                    [AC_SUBST([GENTOO_LLVM_CONFIG], [[/usr/lib/llvm/$1/bin/llvm-config]])
                     AS_IF([$GENTOO_LLVM_CONFIG --version >/dev/null 2>/dev/null],
                       [AC_SUBST([LLVM_CONFIG], [[$GENTOO_LLVM_CONFIG]])],
                       [AS_IF([test "x$BREW" != "x"],
                         AC_SUBST([BREW_CELLAR], [[`$BREW --cellar`]])
                         AC_SUBST([BREW_LLVM_CONFIG], [$BREW_CELLAR/llvm*/$1*/bin/llvm-config])
                         AS_IF([test "x$BREW_LLVM_CONFIG" != "x"],
                           AC_SUBST([LLVM_CONFIG], [[$BREW_LLVM_CONFIG]])))])])])])])])])])

AC_ARG_WITH([llvm],
  AS_HELP_STRING([--with-llvm[=VERSION]],
   [Set the version of LLVM to look for (subsumed by --with-llvm-config).]))

AC_MSG_CHECKING([for llvm-config])
AC_ARG_WITH([llvm-config],
  AS_HELP_STRING([--with-llvm-config[=PATH]],
    [Set the path to the llvm-config tool.]),
  [AC_SUBST([LLVM_CONFIG], [$with_llvm_config])])

AS_IF([test "x$LLVM_CONFIG" = "x"],
  [AS_IF([test "x$with_llvm" != "x"],
    [AC_CHECK_LLVM_VERSION([$with_llvm])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_SUBST([LLVM_CONFIG], [[`which llvm-config`]])])],
    [AC_SUBST([LLVM_CONFIG], [[`which llvm-config`]])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([10])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([9])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([8])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([7])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([6])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([5])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([4])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([3.9])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([3.8])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([3.7])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([3.6])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([3.5])
     AS_IF([test "x$LLVM_CONFIG" = "x"],
       [AC_CHECK_LLVM_VERSION([3.4])])])])])])])])])])])])])])])])

AS_IF([test "x$LLVM_CONFIG" = "x"],
  [AC_MSG_RESULT([[not found in PATH]])
   AC_MSG_ERROR(
     [[llvm-config not available (you may use --with-llvm-config=)]])],
  [AC_MSG_RESULT([[$LLVM_CONFIG]])])

AC_MSG_CHECKING([llvm-config version])
AC_SUBST([LLVM_VERSION], [[`$LLVM_CONFIG --version`]])
AS_IF([test "x$LLVM_VERSION" = "x"],
  [AC_MSG_ERROR([[unable to run $LLVM_CONFIG --version]])])
AC_MSG_RESULT([[$LLVM_VERSION]])

AS_IF([test "x$with_llvm" != "x"],
  AS_IF([echo $LLVM_VERSION | grep "^$with_llvm" >/dev/null],
    [true],
    [AC_MSG_ERROR([[LLVM version $LLVM_VERSION found but $with_llvm expected]])]))

AC_SUBST([LLVM_VERSION], [[`echo $LLVM_VERSION | sed 's/\(svn\|git\)$//'`]])

AC_SUBST([LLVM_VERSION_MAJOR], [[`echo $LLVM_VERSION | \
  sed 's/^\([[:digit:]][[:digit:]]*\)[.]\([[:digit:]][[:digit:]]*\)[.]\([[:digit:]][[:digit:]]*\)/\1/'`]])
AC_SUBST([LLVM_VERSION_MINOR], [[`echo $LLVM_VERSION | \
  sed 's/^\([[:digit:]][[:digit:]]*\)[.]\([[:digit:]][[:digit:]]*\)[.]\([[:digit:]][[:digit:]]*\)/\2/'`]])
AC_SUBST([LLVM_VERSION_SUBMINOR], [[`echo $LLVM_VERSION | \
  sed 's/^\([[:digit:]][[:digit:]]*\)[.]\([[:digit:]][[:digit:]]*\)[.]\([[:digit:]][[:digit:]]*\)/\3/'`]])

AC_SUBST([EQUIVALENT_LLVM_VERSION], [["$LLVM_VERSION"]])

# Some version equivalences
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.4"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.4.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.4.1"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.4.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.5.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.5.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.5.1"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.5.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.6.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.6.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.6.1"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.6.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.8.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.8.1]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x3.9.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[3.9.1]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x4.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[4.0.1]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x5.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[5.0.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x5.0.1"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[5.0.2]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x6.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[6.0.1]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x7.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[7.1.0]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x7.0.1"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[7.1.0]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x8.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[8.0.1]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x9.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[9.0.1]])])
AS_IF([test "x$EQUIVALENT_LLVM_VERSION" = "x10.0.0"],
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [[10.0.1]])])

AC_ARG_WITH([llvm-version],
  AS_HELP_STRING([--with-llvm-version=VERSION],
    [Sets the LLVM version.]),
  [AC_SUBST([EQUIVALENT_LLVM_VERSION], [["$with_llvm_version"]])
   AC_MSG_NOTICE([[Will consider bindings for version $EQUIVALENT_LLVM_VERSION (--with-llvm-version)]])])

AS_IF([test -d $srcdir/bootstrap/$EQUIVALENT_LLVM_VERSION],
  [true],
  [AC_MSG_ERROR([LLVM version $EQUIVALENT_LLVM_VERSION not supported. Please report: thierry.martinez@inria.fr . You may try to force another bindings version with ./configure --with-llvm-version= (to obtain the list of supported versions: ls $srcdir/bootstrap).])])

AC_DEFUN([AC_VERSION_BLOCK],
  [AX_COMPARE_VERSION([$EQUIVALENT_LLVM_VERSION], [ge], [$1.$2],
    [AC_SUBST(IF_CLANG_BEFORE_[$1]_[$2], ["(*"])
     AC_SUBST(ENDIF_CLANG_BEFORE_[$1]_[$2], ["*)"])
     AC_SUBST(IF_CLANG_FROM_[$1]_[$2], [""])
     AC_SUBST(ENDIF_CLANG_FROM_[$1]_[$2], [""])],
    [AC_SUBST(IF_CLANG_BEFORE_[$1]_[$2], [""])
     AC_SUBST(ENDIF_CLANG_BEFORE_[$1]_[$2], [""])
     AC_SUBST(IF_CLANG_FROM_[$1]_[$2], ["(*"])
     AC_SUBST(ENDIF_CLANG_FROM_[$1]_[$2], ["*)"])])])

AC_VERSION_BLOCK(3, 5)
AC_VERSION_BLOCK(3, 6)
AC_VERSION_BLOCK(3, 7)
AC_VERSION_BLOCK(3, 8)
AC_VERSION_BLOCK(3, 9)
AC_VERSION_BLOCK(4, 0)
AC_VERSION_BLOCK(5, 0)
AC_VERSION_BLOCK(6, 0)
AC_VERSION_BLOCK(7, 0)
AC_VERSION_BLOCK(8, 0)
AC_VERSION_BLOCK(9, 0)
AC_VERSION_BLOCK(10, 0)

AC_SUBST([LLVM_CFLAGS], [[`$LLVM_CONFIG --cflags`]])

AC_SUBST([LLVM_LDFLAGS], [[`$LLVM_CONFIG --ldflags`]])

AC_SUBST([LLVM_LIBS], [[`$LLVM_CONFIG --libs`]])

AC_SUBST([LLVM_LIBDIR], [[`$LLVM_CONFIG --libdir`]])

AC_SUBST([LLVM_INCLUDEDIR], [[`$LLVM_CONFIG --includedir`]])

AC_SUBST([LLVM_VERSION_MACRO],
  [[LLVM_VERSION_`echo $EQUIVALENT_LLVM_VERSION | sed 's/[.]/_/g'`]])

# These filters enable compilation with gcc.
# Filter -Wstring-conversion for OpenSUSE
# Filter -Werror=unguarded-availability-new and -Wcovered-switch-default
# (which appear with LLVM 7)
# Filter "-Wdelete-non-virtual-dtor" (warning only)
AC_SUBST([LLVM_CFLAGS], [["`echo $LLVM_CFLAGS \
  | sed s/-Wstring-conversion\\ // \
  | sed s/-Werror=unguarded-availability-new\\ // \
  | sed s/-Wcovered-switch-default\\ // \
  | sed s/-Wdelete-non-virtual-dtor\\ //`"]])

AC_SUBST([CFLAGS], [["$LLVM_CFLAGS -D$LLVM_VERSION_MACRO -I$srcdir/clangml"]])

AC_SUBST([CXXFLAGS], [["$CFLAGS --std=c++14"]])

AC_CHECK_HEADERS([clang-c/Index.h], [],
  [AC_MSG_ERROR([[Header clang-c/Index.h not found]])])

# LIBS and LDFLAGS should be changed after AC_CHECK_HEADERS
# or else configure will fail to use the C compiler even for the simplest
# tests when libclang is not available.
AC_SUBST([STRIPPED_LLVM_LDFLAGS],
  [[`echo $LLVM_LDFLAGS | sed 's/-Wl,-search_paths_first //' | sed 's/-Wl,-headerpad_max_install_names//'`]])

AC_SUBST([LDFLAGS],
  [["$STRIPPED_LLVM_LDFLAGS -Wl,-rpath,$LLVM_LIBDIR"]])

AC_CHECK_LIB([clang], [clang_createIndex], [],
  [AC_MSG_ERROR([[libclang not found]])])

AC_SEARCH_LIBS([del_curterm], [curses ncurses])

AC_SUBST([LIBS],
  [["$LLVM_LIBS -lpthread \
     -lstdc++ -ldl -lm -lz $LIBS"]])

AC_LANG_CPLUSPLUS

AC_TRY_LINK(
  [#include <llvm/ADT/APInt.h>],
  [
    llvm::APInt i;
    i.toString(10, false);
  ],
  [
    AC_MSG_CHECKING([for disabling GLIBCXX_USE_CXX11_ABI])
    AC_MSG_RESULT([not required])
  ],
  [
    AC_MSG_CHECKING([for disabling GLIBCXX_USE_CXX11_ABI])
    AC_SUBST([CXXFLAGS], [["$CXXFLAGS -D_GLIBCXX_USE_CXX11_ABI=0"]])
    AC_TRY_LINK(
      [#include <llvm/ADT/APInt.h>],
      [
        llvm::APInt i;
        i.toString(10, false);
      ],
      [AC_MSG_RESULT([required])],
      [
        AC_MSG_RESULT([cannot compile test])
        AC_MSG_ERROR([Cannot compile test for APInt::toString])
      ])
  ])

AC_SUBST([BASELIBS], [["$LIBS"]])

AC_MSG_CHECKING([if -lclangAST -lclangLex -lclangBasic is available])
AC_SUBST([LIBS], [["-lclangAST -lclangLex -lclangBasic $BASELIBS"]])
AC_TRY_LINK(
  [#include <clang/AST/Decl.h>],
  [
    clang::FunctionDecl *d;
    d->getNumParams();
  ],
  [AC_MSG_RESULT([yes])],
  [
    AC_MSG_CHECKING([if -lclang-cpp is available])
    AC_SUBST([LIBS], [["-lclang-cpp $BASELIBS"]])
    AC_TRY_LINK(
      [#include <clang/AST/Decl.h>],
      [
        clang::FunctionDecl *d;
        d->getNumParams();
      ],
      [AC_MSG_RESULT([yes])],
      [
        AC_MSG_RESULT([no])
        AC_MSG_ERROR([Neither -lclang-cpp nor -lclangAST is available])])])

AC_SUBST([CXXFLAGS], [["-fPIC $CXXFLAGS"]])

AC_MSG_NOTICE(initialize build tree)

AC_DEFUN([AC_CP],
  [AS_IF([cp $1 $2],
    [true],
    [AC_MSG_ERROR([[cannot copy file]])])])

AS_IF([test -d clangml],
  [true],
  [AS_IF([mkdir clangml],
    [AC_CP([$srcdir/dune-project], [.])
     AC_CP([$srcdir/clangml.opam.template], [.])
     AC_CP([$srcdir/clangml/clang.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang.mli], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__types.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__utils.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__utils.mli], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast_options.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast_utils.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast_utils.mli], [clangml/])
     AC_CP([$srcdir/clangml/printer.ml], [clangml/])
     AC_CP([$srcdir/clangml/printer.mli], [clangml/])
     AC_CP([$srcdir/clangml/clang__command_line.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__custom.c], [clangml/])
     AC_CP([$srcdir/clangml/clang__custom.h], [clangml/])
     AC_CP([$srcdir/clangml/clang_specials.c], [clangml/])
     AC_CP([$srcdir/clangml/special_bindings.ml], [clangml/])
     AC_CP([$srcdir/clangml/standard.ml], [clangml/])
     AC_CP([$srcdir/clangml/dune], [clangml/])
     AC_CP([$srcdir/clangml/stubgen.h], [clangml/])
     AC_CP([$srcdir/clangml/libclang_extensions.cpp], [clangml/])
     AC_CP([$srcdir/clangml/libclang_extensions.h], [clangml/])
     AC_CP([$srcdir/clangml/clangml_OperationKinds.def], [clangml/])
     AC_CP([$srcdir/clangml/ast_sig.ml], [clangml/])],
    [AC_MSG_ERROR([[cannot create directory clangml/]])])])

AS_IF([test -d ppx],
  [true],
  [AS_IF([mkdir ppx],
     AC_CP([$srcdir/ppx/clangml_ppx.ml], [ppx/])
     AC_CP([$srcdir/ppx/ppx_lexer.mll], [ppx/])
     AC_CP([$srcdir/ppx/dune], [ppx/]))])

AS_IF([test -d printer],
  [true],
  [AS_IF([mkdir printer],
     AC_CP([$srcdir/printer/clangml_printer.ml], [printer/])
     AC_CP([$srcdir/printer/dune], [printer/]))])

AS_IF([test -d tools],
  [true],
  [AS_IF([mkdir tools],
    [true])])

AS_IF([test -d tools/common],
  [true],
  [AS_IF([mkdir tools/common],
    [AC_CP([$srcdir/tools/common/dune], [tools/common/])
     AC_CP([$srcdir/tools/common/clangml_tools_common.ml], [tools/common/])
     AC_CP([$srcdir/tools/common/clangml_tools_common.mli], [tools/common/])])])

AS_IF([test -d tools/stubgen_common],
  [true],
  [AS_IF([mkdir tools/stubgen_common],
    [AC_CP([$srcdir/tools/stubgen_common/dune], [tools/stubgen_common/])
     AC_CP([$srcdir/tools/stubgen_common/stubgen_common.ml], [tools/stubgen_common/])
     AC_CP([$srcdir/tools/stubgen_common/clang_helper.ml], [tools/stubgen_common/])
     AC_CP([$srcdir/tools/stubgen_common/string_utils.ml], [tools/stubgen_common/])])])

AS_IF([test -d tools/stubgen],
  [true],
  [AS_IF([mkdir tools/stubgen],
    [AC_CP([$srcdir/tools/stubgen/dune], [tools/stubgen/])
     AC_CP([$srcdir/tools/stubgen/stubgen.ml], [tools/stubgen/])])])

AS_IF([test -d tools/norm_extractor],
  [true],
  [AS_IF([mkdir tools/norm_extractor],
     AC_CP([$srcdir/tools/norm_extractor/norm_extractor.ml], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/tex_lexer.mll], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/tex_lexer.mli], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/aux_lexer.mll], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/aux_lexer.mli], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/kmp.ml], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/kmp.mli], [tools/norm_extractor/])
     AC_CP([$srcdir/tools/norm_extractor/dune], [tools/norm_extractor/]))])

AS_IF([test -d tools/print_tree],
  [true],
  [AS_IF([mkdir tools/print_tree],
    [AC_CP([$srcdir/tools/print_tree/dune], [tools/print_tree/])
     AC_CP([$srcdir/tools/print_tree/print_tree.ml], [tools/print_tree/])])])

AS_IF([test -d tools/generate_attrs],
  [true],
  [AS_IF([mkdir tools/generate_attrs],
    [AC_CP([$srcdir/tools/generate_attrs/dune], [tools/generate_attrs/])
     AC_CP([$srcdir/tools/generate_attrs/generate_attrs.ml], [tools/generate_attrs/])])])

AS_IF([test -d tests],
  [true],
  [AS_IF([mkdir tests],
    [AS_IF([mkdir tests/ast],
      [AC_CP([$srcdir/tests/ast/dune], [tests/ast/])])]
    [AS_IF([mkdir tests/c17],
      [AC_CP([$srcdir/tests/c17/dune], [tests/c17/])
       AC_CP([$srcdir/tests/c17/c17.ml], [tests/c17/])])]
    [AS_IF([mkdir tests/cpp],
      [AC_CP([$srcdir/tests/cpp/dune], [tests/cpp/])
       AC_CP([$srcdir/tests/cpp/cpp.ml], [tests/cpp/])])]
    [AS_IF([mkdir tests/ppx],
      [AC_CP([$srcdir/tests/ppx/dune], [tests/ppx/])
       AC_CP([$srcdir/tests/ppx/ppx.ml], [tests/ppx/])])])])

AS_IF([test -d config],
  [true],
  [AS_IF([mkdir config],
     AC_CP([$srcdir/config/dune], [config/]))])

AC_CP([$srcdir/bootstrap/$EQUIVALENT_LLVM_VERSION/clang__bindings.ml],
  [clangml/clang__bindings.ml])

AC_CP([$srcdir/bootstrap/$EQUIVALENT_LLVM_VERSION/clang__bindings.mli],
  [clangml/clang__bindings.mli])

AC_CP([$srcdir/bootstrap/$EQUIVALENT_LLVM_VERSION/clang_stubs.c],
  [clangml/clang_stubs.c])

AC_CP([$srcdir/bootstrap/attributes.ml], [clangml/attributes.ml])

AC_CP(
  [$srcdir/bootstrap/libclang_extensions_attrs_headers.inc],
  [clangml/libclang_extensions_attrs_headers.inc])

AC_CP(
  [$srcdir/bootstrap/libclang_extensions_attrs.inc],
  [clangml/libclang_extensions_attrs.inc])

AC_CONFIG_FILES([
  Makefile
  config/cflags.sexp
  config/cxxflags.sexp
  config/ldflags.sexp
  config/ldflags_quoted.sexp
  config/libs.sexp
  config/libs_quoted.sexp
  config/clangml_config.ml
])

AM_MAINTAINER_MODE
AC_OUTPUT
