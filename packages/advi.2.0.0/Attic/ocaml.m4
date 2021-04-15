# OCaml macros for autoconf
#
# Guillaume Rousse <Guillaume.Rousse@inria.fr>
# inspired by previous work from:
# Georges Mariano
# Jean-Christophe Filliâtre
# Olivier Andrieu
# Grigory Batalov


# AC_PROG_OCAML([MINIMUM VERSION])
# --------------------------------
#  check OCaml base system, and set the following variables:
#  OCAMLC       OCaml compiler
#  OCAMLOPT     OCaml native compiler
#  OCAMLDEP     OCaml dependency generator
#  OCAMLLIB     OCaml library path
#  OCAMLVERSION OCaml version number
#  Unless --disable-opt is set by user, optimized versions are used by default.
#  Fails if no compiler is found.
AC_DEFUN([AC_PROG_OCAML], [

    # allow the user to disable the use of optimized versions
    AC_ARG_ENABLE(
	[native-tools],
	AC_HELP_STRING(
	    [--enable-native-tools],
	    [use native versions of ocaml tools (default)]
	),
	[case "$enableval" in
	    yes) ac_ocaml_enable_native_tools=$enableval;;
	    no)  ac_ocaml_enable_native_tools=$enableval;;
	    *)   AC_MSG_ERROR([bad value $enableval for --enable-native-tools]);;
	esac],
	[ac_ocaml_enable_native_tools=yes]
    )

    # Checking for OCaml compiler
    _AC_OCAML_PATH_PROG_FATAL(OCAMLC, ocamlc)

    # Checking for OCaml version
    AC_CACHE_CHECK(
	[for OCaml version],
	[ac_cv_ocaml_version],
	[ac_cv_ocaml_version=`$OCAMLC -version`]
    )
    OCAMLVERSION=$ac_cv_ocaml_version

    if test -n ["$1"]; then
        ac_ocaml_min_version=["$1"];
	# Checking for OCaml minimum version
	AC_CACHE_CHECK(
	    [whether OCaml version >= $ac_ocaml_min_version],
	    [ac_cv_ocaml_version_enough],
	    [
		ac_ocaml_min_major_version=`echo $ac_ocaml_min_version \
		    | cut -d. -f1`
		ac_ocaml_min_minor_version=`echo $ac_ocaml_min_version \
		    | cut -d. -f2`
		ac_ocaml_min_micro_version=`echo $ac_ocaml_min_version \
		    | cut -d. -f3`
		ac_ocaml_major_version=`echo ${OCAMLVERSION%%+*} | cut -d. -f1`
		ac_ocaml_minor_version=`echo ${OCAMLVERSION%%+*} | cut -d. -f2`
		ac_ocaml_micro_version=`echo ${OCAMLVERSION%%+*} | cut -d. -f3`

		if expr                                      \
		    \(                                       \
			${ac_ocaml_major_version:-0} \>      \
			${ac_ocaml_min_major_version:-0}     \
		    \) \|                                    \
		    \(                                       \
			${ac_ocaml_major_version:-0} \=      \
			${ac_ocaml_min_major_version:-0} \&  \
			${ac_ocaml_minor_version:-0} \>      \
			${ac_ocaml_min_minor_version:-0}     \
		    \) \|                                    \
		    \(                                       \
			${ac_ocaml_major_version:-0} \=      \
			${ac_ocaml_min_major_version:-0} \&  \
			${ac_ocaml_minor_version:-0} \=      \
			${ac_ocaml_min_minor_version:-0} \&  \
			${ac_ocaml_micro_version:-0} \>=     \
			${ac_ocaml_min_micro_version:-0}     \
		    \) > /dev/null; then
		    ac_cv_ocaml_version_enough=yes
		else
		    ac_cv_ocaml_version_enough=no
		fi
	    ]
	)

	if test "$ac_cv_ocaml_version_enough" = "no"; then
	    AC_MSG_ERROR([OCaml version unsufficient])
	fi
    fi

    # Checking for OCaml library path
    AC_CACHE_CHECK(
	[for OCaml library path],
	[ac_cv_ocaml_library_path],
	[ac_cv_ocaml_library_path=`$OCAMLC -where`]
    )
    OCAMLLIB=$ac_cv_ocaml_library_path

    if test "$ac_ocaml_enable_native_tools" = "yes"; then
	# Checking for ocamlc.opt
	_AC_OCAML_PATH_PROG_NONFATAL(OCAMLC_OPT, ocamlc.opt)
	if test -n "$OCAMLC_OPT"; then
	    _AC_OCAML_CHECK_VERSION_NONFATAL(OCAMLC_OPT, ocamlc.opt)
	fi
	if test -n "$OCAMLC_OPT"; then
	    OCAMLC=$OCAMLC_OPT
	fi
    fi

    # Checking for OCaml native compiler
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLOPT, ocamlopt, [Cannot find ocamlopt; bytecode compilation only])
    if test -n "$OCAMLOPT"; then
	_AC_OCAML_CHECK_VERSION_NONFATAL(OCAMLOPT, ocamlopt)
    fi
    if test -n "$OCAMLOPT"; then
        AC_CACHE_CHECK(
	    [if OCaml C compiler works],
	    [ac_cv_ocaml_c_compiler_works],
	    [
		touch conftest.c
		if $OCAMLC conftest.c >/dev/null 2>&1; then
		    ac_cv_ocaml_c_compiler_works=yes
		else
		    ac_cv_ocaml_c_compiler_works=no
		fi
		rm -f conftest.c
	    ]
	)

	if test "$ac_cv_ocaml_c_compiler_works" = "no"; then 
	    AC_MSG_WARN([bytecode compilation only])
	    unset OCAMLOPT
	fi
    fi

    if test "$ac_ocaml_enable_native_tools" = "yes"; then
	# Checking for ocamlopt.opt
	_AC_OCAML_PATH_PROG_NONFATAL(OCAMLOPT_OPT, ocamlopt.opt)
	if test -n "$OCAMLOPT_OPT"; then
	    _AC_OCAML_CHECK_VERSION_NONFATAL(OCAMLOPT_OPT, ocamlopt.opt)
	fi
	if test -n "$OCAMLOPT_OPT"; then
	    OCAMLOPT=$OCAMLOPT_OPT
	fi
    fi

    # Checking for ocamldep
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLDEP, ocamldep)

    if test "$ac_ocaml_enable_native_tools" = "yes"; then
	# Checking for ocamldep.opt
	_AC_OCAML_PATH_PROG_NONFATAL(OCAMLDEP_OPT, ocamldep.opt)
	if test -n "$OCAMLDEP_OPT"; then
	    OCAMLDEP=$OCAMLDEP_OPT
	fi
    fi

    AC_ARG_VAR([OCAMLCFLAGS], [Ocaml compiler flags [none]])

]) # AC_PROG_OCAML

# AC_PROG_OCAML_TOOL(VARIABLE, PROGRAM)
# ---------------------
#  check some additional OCaml tool, and set VARIABLE to PROGRAM if found.
#  Unless --disable-opt is set by user, optimized versions is used by default.
AC_DEFUN([AC_PROG_OCAML_TOOL], [
    AC_REQUIRE([AC_PROG_OCAML])

    # Checking for bytecode version
    _AC_OCAML_PATH_PROG_NONFATAL([$1], [$2])

    if test "$ac_ocaml_enable_native_tools" = "yes"; then
        # Checking for binary version, using AC_PATH_PROG directly
	# to avoid warnings 
	AC_PATH_PROG([$1]_OPT, [$2].opt)
	if test -n "[$$1]_OPT"; then
	    [$1]=[$$1]_OPT
	fi
    fi
]) # AC_PROG_OCAML_TOOL

# AC_PROG_CAMLP4
# --------------
# Check CamlP4 and set the following variables:
#   CAMLP4	camlp4
#   CAMLP4O	camlp4o
#   CAMLP4R	camlp4r
#   CAMLP4LIB	parser library path
#  Fails if camlp4 is not found
AC_DEFUN([AC_PROG_CAMLP4], [
    AC_REQUIRE([AC_PROG_OCAML])

    # Checking for camlp4
    _AC_OCAML_PATH_PROG_FATAL(CAMLP4, camlp4)
    _AC_OCAML_CHECK_VERSION_FATAL(CAMLP4, camlp4)

    # Checking for Camlp4o
    _AC_OCAML_PATH_PROG_NONFATAL(CAMLP4O, camlp4o)

    # Checking for Camlp4r
    _AC_OCAML_PATH_PROG_NONFATAL(CAMLP4R, camlp4r)

    # Searching for parser library path
    AC_MSG_CHECKING([for CamlP4 library path])
    CAMLP4LIB=`$CAMLP4 -where`
    AC_MSG_RESULT([$CAMLP4LIB])

]) # AC_PROG_CAMLP4

# _AC_OCAML_PATH_PROG_FATAL(VARIABLE, PROGRAM, [MESSAGE])
# -------------------------------------------------------
# wraps AC_PATH_PROG, issuing an error if PROGRAM
# is not found, otherwise affects its path to VARIABLE
AC_DEFUN([_AC_OCAML_PATH_PROG_FATAL], [
    AC_PATH_PROG([$1], [$2])
    if test -z "[$$1]"; then
	AC_MSG_ERROR([m4_default([$3], [Cannot find [$2]])])
    fi
]) # _AC_OCAML_PATH_PROG_FATAL

# _AC_OCAML_PATH_PROG_NONFATAL(VARIABLE, PROGRAM, [MESSAGE])
# ----------------------------------------------------------
# wraps AC_PATH_PROG, issuing a warning if PROGRAM
# is not found, otherwise affects its path to VARIABLE
AC_DEFUN([_AC_OCAML_PATH_PROG_NONFATAL], [
    AC_PATH_PROG([$1], [$2])
    if test -z "[$$1]"; then
	AC_MSG_WARN([m4_default([$3], [Cannot find [$2]])])
    fi
]) # _AC_OCAML_PATH_PROG_NONFATAL

# _AC_OCAML_CHECK_VERSION(VARIABLE, PROGRAM)
# ------------------------------------------
# check than PROGRAM version is the same as the OCaml compiler,
# otherwise unset VARIABLE
AC_DEFUN([_AC_OCAML_CHECK_VERSION], [
    AC_CACHE_CHECK(
	[wether [$2] version = $OCAMLVERSION],
	[ac_cv_ocaml_[$1]_version_ok],
	[
	    ac_ocaml_[$1]_version=`$[$1] -version`
	    if test "$ac_ocaml_[$1]_version" = "$OCAMLVERSION"; then
		ac_cv_ocaml_[$1]_version_ok=yes
	    else
		ac_cv_ocaml_[$1]_version_ok=no
	    fi
	]
    )

    if test "$ac_cv_ocaml_[$1]_version_ok" = "no"; then
	unset [$1]
    fi
]) # _AC_OCAML_CHECK_VERSION

# _AC_OCAML_CHECK_VERSION_NONFATAL(VARIABLE, PROGRAM)
# ------------------------------------------
# wraps _AC_OCAML_CHECK_VERSION, issuing a warning if it fails
AC_DEFUN([_AC_OCAML_CHECK_VERSION_NONFATAL], [
    _AC_OCAML_CHECK_VERSION([$1], [$2])
    if test -z ["$$1"]; then
	AC_MSG_WARN([[$2] version differs from ocamlc, discarding])
    fi
]) # _AC_OCAML_CHECK_VERSION_NONFATAL

# _AC_OCAML_CHECK_VERSION_FATAL(VARIABLE, PROGRAM)
# ------------------------------------------
# wraps _AC_OCAML_CHECK_VERSION, issuing an error if it fails
AC_DEFUN([_AC_OCAML_CHECK_VERSION_FATAL], [
    _AC_OCAML_CHECK_VERSION([$1], [$2])
    if test -z ["$$1"]; then
	AC_MSG_ERROR([[$2] version differs from ocamlc, aborting])
    fi
]) # _AC_OCAML_CHECK_VERSION_FATAL
