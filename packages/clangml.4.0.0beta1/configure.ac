AC_INIT([[clangml]], [m4_esyscmd([cat VERSION])],
  [[Thierry.Martinez@inria.fr]], [[clangml]],
  [[https://github.com/thierry.martinez/clangml/]])
AM_INIT_AUTOMAKE([foreign]) dnl to allow README.md

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
AC_CHECK_OCAMLFIND_PKG([ppx_deriving])
AC_CHECK_OCAMLFIND_PKG([visitors])

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

AC_MSG_CHECKING([for llvm-config])
AC_ARG_WITH([llvm-config],
  AS_HELP_STRING([--with-llvm-config[=PATH]],
    [Set the path to the llvm-config tool.]),
  [AC_SUBST([LLVM_CONFIG], [$with_llvm_config])
   AC_MSG_RESULT([[$LLVM_CONFIG (--with-llvm-config=)]])],
  [AC_SUBST([LLVM_CONFIG], [[`which llvm-config`]])
   AS_IF([test "x$LLVM_CONFIG" = "x"],
     [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-7`]])
      AS_IF([test "x$LLVM_CONFIG" = "x"],
     [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-7.0`]])
      AS_IF([test "x$LLVM_CONFIG" = "x"],
        [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-6`]])
         AS_IF([test "x$LLVM_CONFIG" = "x"],
        [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-6.0`]])
         AS_IF([test "x$LLVM_CONFIG" = "x"],
           [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-5`]])
            AS_IF([test "x$LLVM_CONFIG" = "x"],
           [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-5.0`]])
            AS_IF([test "x$LLVM_CONFIG" = "x"],
              [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-4`]])
               AS_IF([test "x$LLVM_CONFIG" = "x"],
              [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-4.0`]])
               AS_IF([test "x$LLVM_CONFIG" = "x"],
                 [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-3`]])
                  AS_IF([test "x$LLVM_CONFIG" = "x"],
                    [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-3.0`]])])])])])])])])])])])
   AS_IF([test "x$LLVM_CONFIG" = "x"],
     [AS_IF([test -f /usr/local/opt/llvm/bin/llvm-config],
       [AC_SUBST([LLVM_CONFIG], [[/usr/local/opt/llvm/bin/llvm-config]])
        AC_MSG_RESULT([[$LLVM_CONFIG (homebrew)]])],
       [AC_MSG_RESULT([[not found in PATH]])
        AC_MSG_ERROR(
          [[llvm-config not available (you may use --with-llvm-config=)]])])],
     [AC_MSG_RESULT([[$LLVM_CONFIG (found in PATH)]])])])

AC_MSG_CHECKING([llvm-config version])
AC_SUBST([LLVM_VERSION], [[`$LLVM_CONFIG --version`]])
AS_IF([test "x$LLVM_VERSION" = "x"],
  [AC_MSG_ERROR([[unable to run $LLVM_CONFIG --version]])])
AC_MSG_RESULT([[$LLVM_VERSION]])

AC_SUBST([LLVM_VERSION],
  [[`echo $LLVM_VERSION | sed 's/svn$//'`]])

# Some version equivalences
AS_IF([test "x$LLVM_VERSION" = "x3.4"],
  [AC_SUBST([LLVM_VERSION], [[3.4.2]])])
AS_IF([test "x$LLVM_VERSION" = "x3.4.1"],
  [AC_SUBST([LLVM_VERSION], [[3.4.2]])])
AS_IF([test "x$LLVM_VERSION" = "x3.5.0"],
  [AC_SUBST([LLVM_VERSION], [[3.5.2]])])
AS_IF([test "x$LLVM_VERSION" = "x3.5.1"],
  [AC_SUBST([LLVM_VERSION], [[3.5.2]])])
AS_IF([test "x$LLVM_VERSION" = "x3.6.0"],
  [AC_SUBST([LLVM_VERSION], [[3.6.2]])])
AS_IF([test "x$LLVM_VERSION" = "x3.6.1"],
  [AC_SUBST([LLVM_VERSION], [[3.6.2]])])
AS_IF([test "x$LLVM_VERSION" = "x3.8.0"],
  [AC_SUBST([LLVM_VERSION], [[3.8.1]])])
AS_IF([test "x$LLVM_VERSION" = "x3.9.0"],
  [AC_SUBST([LLVM_VERSION], [[3.9.1]])])
AS_IF([test "x$LLVM_VERSION" = "x4.0.0"],
  [AC_SUBST([LLVM_VERSION], [[4.0.1]])])
AS_IF([test "x$LLVM_VERSION" = "x5.0.0"],
  [AC_SUBST([LLVM_VERSION], [[5.0.2]])])
AS_IF([test "x$LLVM_VERSION" = "x5.0.1"],
  [AC_SUBST([LLVM_VERSION], [[5.0.2]])])
AS_IF([test "x$LLVM_VERSION" = "x6.0.0"],
  [AC_SUBST([LLVM_VERSION], [[6.0.1]])])
AS_IF([test "x$LLVM_VERSION" = "x7.0.0"],
  [AC_SUBST([LLVM_VERSION], [[7.0.1]])])

AC_ARG_WITH([llvm-version],
  AS_HELP_STRING([--with-llvm-version=VERSION],
    [Sets the LLVM version.]),
  [AC_SUBST([LLVM_VERSION], [["$with_llvm_version"]])
   AC_MSG_NOTICE([[Will consider bindings for version $LLVM_VERSION (--with-llvm-version)]])])

AS_IF([test -d $srcdir/bootstrap/$LLVM_VERSION],
  [true],
  [AC_MSG_ERROR([LLVM version $LLVM_VERSION not supported. Please report: thierry.martinez@inria.fr . You may try to force another bindings version with ./configure --with-llvm-version= (to obtain the list of supported versions: ls $srcdir/bootstrap).])])

AS_IF([test `printf "$LLVM_VERSION\n3.5" | sort | head -n1` = "3.5"],
  [AC_SUBST([IF_CLANG_BEFORE_3_5], ["(*"])
   AC_SUBST([ENDIF_CLANG_BEFORE_3_5], ["*)"])],
  [AC_SUBST([IF_CLANG_BEFORE_3_5], [""])
   AC_SUBST([ENDIF_CLANG_BEFORE_3_5], [""])])

AS_IF([test `printf "$LLVM_VERSION\n3.7" | sort | head -n1` = "3.7"],
  [AC_SUBST([IF_CLANG_BEFORE_3_7], ["(*"])
   AC_SUBST([ENDIF_CLANG_BEFORE_3_7], ["*)"])],
  [AC_SUBST([IF_CLANG_BEFORE_3_7], [""])
   AC_SUBST([ENDIF_CLANG_BEFORE_3_7], [""])])

AC_SUBST([LLVM_CFLAGS], [[`$LLVM_CONFIG --cflags `]])

AC_SUBST([LLVM_LDFLAGS], [[`$LLVM_CONFIG --ldflags`]])

AC_SUBST([LLVM_LIBS], [[`$LLVM_CONFIG --libs`]])

AC_SUBST([LLVM_LIBDIR], [[`$LLVM_CONFIG --libdir`]])

AC_SUBST([LLVM_VERSION_MACRO],
  [[LLVM_VERSION_`echo $LLVM_VERSION | sed 's/[.]/_/g'`]])

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

AS_IF([test "x$LLVM_VERSION" = "x3.5.2"],
  [AC_SUBST([CFLAGS], [["$CFLAGS -D_GLIBCXX_USE_CXX11_ABI=0"]])])

AC_SUBST([CPPFLAGS], [["$CFLAGS"]])

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
  [["-lclangAST -lclangLex -lclangBasic $LLVM_LIBS -lpthread \
     -lstdc++ -ldl -lm -lz $LIBS"]])

AC_CHECK_LIB([clang], [clang_createIndex], [],
  [AC_MSG_ERROR([[libclang not found]])])

AC_MSG_NOTICE(initialize build tree)

AC_DEFUN([AC_CP],
  [AS_IF([cp $1 $2],
    [true],
    [AC_MSG_ERROR([[cannot copy file]])])])

AS_IF([test -d stubgen],
  [true],
  [AS_IF([mkdir stubgen],
    [AC_CP([$srcdir/stubgen/dune], [stubgen/])
     AC_CP([$srcdir/stubgen/stubgen.ml], [stubgen/])],
    [AC_MSG_ERROR([[cannot create directory stubgen/]])])])

AS_IF([test -d clangml],
  [true],
  [AS_IF([mkdir clangml],
    [AC_CP([$srcdir/clangml/clang.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang.mli], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast.ml], [clangml/])
     AC_CP([$srcdir/clangml/dune], [clangml/])
     AC_CP([$srcdir/clangml/stubgen.h], [clangml/])
     AC_CP([$srcdir/clangml/libclang_extensions.cpp], [clangml/])
     AC_CP([$srcdir/clangml/libclang_extensions.h], [clangml/])],
    [AC_MSG_ERROR([[cannot create directory clangml/]])])])

cp $srcdir/dune-project .
cp $srcdir/clangml.opam .
cp $srcdir/clangml/clangml_OperationKinds.def clangml/

AC_CP([$srcdir/bootstrap/$LLVM_VERSION/clang__bindings.ml], [clangml/clang__bindings.ml])

AC_CP([$srcdir/bootstrap/$LLVM_VERSION/clang__bindings.mli], [clangml/clang__bindings.mli])

AC_CP([$srcdir/bootstrap/$LLVM_VERSION/clang_stubs.c], [clangml/clang_stubs.c])

AC_CONFIG_FILES([
  Makefile
  config/cflags.sexp
  config/cppflags.sexp
  config/ldflags.sexp
  config/ldflags_quoted.sexp
  config/libs.sexp
  config/libs_quoted.sexp
  clangml/clang__compat.ml
])

AC_OUTPUT
AM_MAINTAINER_MODE
