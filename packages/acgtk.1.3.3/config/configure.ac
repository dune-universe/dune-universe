(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

VERSION="1.3.2-20171123"

AC_INIT([ACG DTK],[$VERSION],[sylvain.pogodalla@loria.fr])

#AC_COPYRIGHT([Cecill])
m4_include(ac_lib_checking.m4)

OCAML_REQUIRED=3.07

# First look for bytecode compilers (ocamlc.opt or ocamlc) in the path
# Fail if not present
AC_CHECK_PROGS(OCAMLC,ocamlc.opt ocamlc,no)
if test "$OCAMLC" = no ; then
        AC_MSG_ERROR(Cannot find ocamlc.)
fi

# Then look at native code compilers (ocamlopt.opt or ocamlopt) in the path
# Fail if not present
# we first look for ocaml in the path; if not present, we fail
AC_CHECK_PROGS(OCAMLCOPT,ocamlopt.opt ocamlopt,no)
if test "$OCAMLCOPT" = no ; then
        AC_MSG_ERROR(Cannot find ocamlc.opt.)
fi

# Look for the interpreter
AC_CHECK_PROG(OCAML,ocaml,ocaml,no)
if test "$OCAML" = no ; then
        AC_MSG_ERROR(Cannot find ocaml.)
fi

# Look for camlp4o
AC_CHECK_PROG(OCAMLP4,camlp4o,camlp4o,no)
if test "$OCAMLP4" = no ; then
        AC_MSG_ERROR(Cannot find camlp4o.)
fi

# Look for documentation generators (ocamldoc.opt or ocamldoc)
AC_CHECK_PROGS(OCAMLDOC,ocamldoc.opt ocamldoc,no)
if test "$OCAMLDOC" = no ; then
        AC_MSG_ERROR(Cannot find ocamldoc.)
fi

# Look for dependency generator (ocamldep.opt or ocamldep)
AC_CHECK_PROGS(OCAMLDEP,ocamldep.opt ocamldep,no)
if test "$OCAMLDEP" = no ; then
        AC_MSG_ERROR(Cannot find ocamldep.)
fi

# Look for ocamllex
AC_CHECK_PROGS(OCAMLLEX,ocamllex.opt ocamllex,no)
if test "$OCAMLLEX" = no ; then
	AC_MSG_ERROR(Cannot find ocamllex)
fi

# Look for ocamlyacc
AC_CHECK_PROGS(OCAMLYACC,ocamlyacc.opt ocamlyacc,no)
if test "$OCAMLYACC" = no ; then
	AC_MSG_ERROR(Cannot find ocamlyac)
fi


# Look for ocamlfind
AC_PATH_PROG(OCAMLFIND,ocamlfind,no)

# Look for opam
AC_PATH_PROG(OPAM,opam,no)

#Look for dypgen
DYPGEN_NEEDED=20080925
AC_ARG_VAR(DYPGEN_PATH,[Directory where to find dypgen if not in a standard location])
if test -n "$DYPGEN_PATH" ; then
   AC_CHECK_PROG(DYPGEN,dypgen,$DYPGEN_PATH/dypgen,no,$DYPGEN_PATH)
else
   AC_CHECK_PROGS(DYPGEN,dypgen.opt dypgen,no)
fi
if test "$DYPGEN" = no ; then
   AC_MSG_ERROR(Cannot find dypgen)
else
	DYPGEN_VERSION=`$DYPGEN --version | grep version | sed 's/^.* \(.*\)$/\1/'`
	AC_MSG_CHECKING([for $DYPGEN version])
	if test $DYPGEN_VERSION -ge $DYPGEN_NEEDED ; then
		   AC_MSG_RESULT($DYPGEN ($DYPGEN_VERSION) is ok)
		   AC_SUBST(DYPGEN)
	else
		   AC_MSG_ERROR($DYPGEN version $DYPGEN_VERSION found ; version $DYPGEN_NEEDED or greater is needed)
	fi
fi

# Look for DYPGEN_LIB with or without ocamlfind
AC_ARG_VAR(DYPGENLIB_PATH,[Directory where to find the dypgen library if not in a standard location])
AC_LIB_CHECKING(dypgen,dyp,dyp,dyp,dypgen,Dyp,,true,$DYPGENLIB_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(DYPGEN_INCLUDE,$LIB_INCLUDE)
fi

AC_ARG_ENABLE(
	[kaputt],
	[AS_HELP_STRING([--enable-kaputt], [Compile with kaputt for unit testing])],
	[with_kaputt=$enableval],
	[with_kaputt=no])
if test "$with_kaputt" != no ; then
   # Look for KAPUTT_LIB
   AC_LIB_CHECKING(kaputt,kaputt,kaputt,kaputt,kaputt,Kaputt,,false,,)
   if test "$FOUND_LIB" != "no" ; then
      if test -f $LIB_DIR/kaputt_pp.byte ; then
      	 KAPUTT_PP="$LIB_DIR/kaputt_pp.byte"
      	 AC_MSG_RESULT(kaputt_pp.byte preprocessor is $KAPUTT_PP)
      	 AC_SUBST(KAPUTT_INCLUDE,$LIB_INCLUDE)	
      	 AC_SUBST(KAPUTT_LIB,$LIB_FILE.cma)	
      	 AC_SUBST(KAPUTT_PP)
      else
	AC_MSG_ERROR(Could not find kaputt_pp.byte preprocessor in $LIB_DIR.)
      fi
   fi
fi

# Look for BOLT_LIB with ocamlfind
AC_ARG_VAR(BOLTLIB_PATH,[Directory where to find the bolt library if not in a standard location])
AC_LIB_CHECKING(bolt,bolt,bolt,bolt,bolt,Bolt,"unix.cma dynlink.cma",true,$BOLTLIB_PATH)
if test "$FOUND_LIB" != "no" ; then
   if test -f $LIB_DIR/bolt_pp.cmo ; then
      BOLT_PP="$LIB_DIR/bolt_pp.cmo -level \$(BOLT_LEVEL)"
      AC_MSG_RESULT(bolt_pp.byte preprocessor is $BOLT_PP)
      AC_SUBST(BOLT_INCLUDE,$LIB_INCLUDE)
      AC_SUBST(BOLT_LIB,$LIB_FILE.cma)
      AC_SUBST(BOLT_PP)
   else
      AC_MSG_ERROR(Could not find bolt_pp.byte preprocessor in $LIB_DIR.)
   fi
fi

AC_ARG_ENABLE(
	[log],
	[AS_HELP_STRING([--enable-log], [Enable log information to be produced by bolt])],
	[with_log=$enableval],
	[with_log=no])
if test "$with_log" = "no" ; then
   AC_SUBST(BOLT_LEVEL,NONE)
else
   AC_SUBST(BOLT_LEVEL,TRACE)
fi
AC_MSG_RESULT(Bolt log level is $BOLT_LEVEL.)

AC_ARG_ENABLE(
	[warning-as-errors],
	[AS_HELP_STRING([--disable-warning-as-errors], [Disable dealing with compilation warnings as errors])],
       	[AC_MSG_RESULT(Compilation warnings not considered as errors.);WARN_ERROR=""],
	[AC_MSG_RESULT(Compilation warnings considered as errors.);WARN_ERROR="-warn-error A"])


# Look for ANSITerminal with ocamlfind
AC_ARG_VAR(ANSITerminal_PATH,[Directory where to find the ANSITerminal library if not in a standard location])
AC_LIB_CHECKING(ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,"unix.cma",true,$ANSITerminal_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(ANSITerminal_INCLUDE,$LIB_INCLUDE)
fi


# Look for the OCaml Cairo bindings with ocamlfind
AC_ARG_VAR(OCamlCairo2_PATH,[Directory where to find the OCaml Cairo bindings if not in a standard location])
AC_LIB_CHECKING(ocaml-cairo2,cairo2,cairo2,cairo2,cairo2,Cairo,"bigarray.cma",true,$OCamlCairo2_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(OCamlCairo2_INCLUDE,$LIB_INCLUDE)
fi

# Look for Easy-format with ocamlfind
AC_ARG_VAR(EASY_FORMAT_PATH,[Directory where to find the Easy-format library if not in a standard location])
AC_LIB_CHECKING(Easy-format,easy-format,easy_format,easy-format,easy-format,Easy_format,"",true,$EASY_FORMAT_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(EASY_FORMAT_INCLUDE,$LIB_INCLUDE)
fi

# Look for biniou with ocamlfind
AC_ARG_VAR(BINIOU_PATH,[Directory where to find the biniou library if not in a standard location])
AC_LIB_CHECKING(Biniou,biniou,biniou,biniou,biniou,Bi_io,"",true,$BINIOU_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(BINIOU_INCLUDE,$LIB_INCLUDE)
fi

# Look for Yojson with ocamlfind
AC_ARG_VAR(YOJSON_PATH,[Directory where to find the Yojson library if not in a standard location])
AC_LIB_CHECKING(Yojson,yojson,yojson,yojson,yojson,Yojson,"",true,$YOJSON_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(YOJSON_INCLUDE,$LIB_INCLUDE)
fi

# Look for ocf with ocamlfind
AC_ARG_VAR(OCF_PATH,[Directory where to find the ocf library if not in a standard location])
AC_LIB_CHECKING(ocf,ocf,ocf,ocf,ocf,Ocf,"",true,$OCF_PATH)

if test "$FOUND_LIB" != "no" ; then
   AC_SUBST(OCF_INCLUDE,$LIB_INCLUDE)
fi

# What is the OCAML version  ?
# we extract Ocaml version number
OCAML_VERSION=`$OCAMLC -version`
AC_MSG_CHECKING([for $OCAMLC version])
AC_MSG_RESULT([$OCAMLC version is $OCAML_VERSION])

# Check whether the caml version checker is provided
DIR=$( cd "$( dirname "$0" )" && pwd )
ML_CHECK_FILE=$DIR/config/ml_check.ml
AC_CHECK_FILE($ML_CHECK_FILE,,AC_MSG_ERROR(Cannot check versions))

# check that this version is at least the one we need
if ($OCAML $ML_CHECK_FILE -ref $OCAML_REQUIRED $OCAML_VERSION > /dev/null) ; then
        AC_MSG_RESULT($OCAMLC version $OCAMLVERSION is ok)
else
        AC_MSG_ERROR($OCAMLC version $OCAMLVERSION found ; version $OCAML_NEEDED or greater is needed)
fi

# check if the dtypes option is present
if ($OCAML $ML_CHECK_FILE -ref 3.07 $OCAML_VERSION> /dev/null) ; then
	AC_MSG_RESULT(Compilation will be done with the -dtypes option)
        TYPES=-dtypes
else
	AC_MSG_RESULT(Compilation will NOT be done with the -dtypes option)
	TYPES=
fi


# check if the -w yz option is present
# and set the _loc variable of campl4 to loc
if ($OCAML $ML_CHECK_FILE -ref 3.09 $OCAML_VERSION > /dev/null) ; then
        OCAML09WARNINGS="-w z"
	OCAMLP4_LOC="-loc loc"
	AC_MSG_RESULT($OCAMLP4 calls will be done with the $OCAMLP4_LOC option)
else
	OCAML09WARNINGS=
fi
AC_MSG_RESULT(Compilation will be done with the $OCAML09WARNINGS option)

if ($OCAML $ML_CHECK_FILE -ref 3.10 $OCAML_VERSION > /dev/null) ; then
	CAMLP4_LIB="camlp4lib.cma"
else
	CAMLP4_LIB="camlp4.cma"
fi
AC_MSG_RESULT($OCAMLP4 calls will be done with the $CAMLP4_LIB library)


############################
# substitutions to perform
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLCOPT)
AC_SUBST(OCAMLP4)
AC_SUBST(CAMLP4_LIB)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLDEP)

AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)




AC_SUBST(TYPES)

AC_SUBST(WARN_ERROR)

AC_SUBST(OCAML09WARNINGS)
AC_SUBST(OCAMLP4_LOC)

AC_SUBST(SET_MAKE)

AC_CONFIG_FILES([./Makefile config/Makefile src/Makefile.master src/Makefile.common src/Makefile src/utils/Makefile src/logic/Makefile src/grammars/Makefile src/acg-data/Makefile src/scripting/Makefile src/datalog.prover/Makefile src/datalog.solver/Makefile])

AC_PROG_MAKE_SET

AC_OUTPUT

