?i# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.59)
AC_INIT(advi, 1.10.2, Didier.Remy@inria.fr)
BUILD_DATE=`date +%Y-%m-%d`
BUILD_YEAR=`date +%Y`
AC_SUBST(BUILD_DATE)
AC_SUBST(BUILD_YEAR)
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE

# Checking ocaml
AC_PROG_OCAML()
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLVERSION)

# Checks other programs
AC_PROG_CC

# check for ocamlfind
AC_PATH_PROG(OCAMLFIND, ocamlfind)
if test -z "$OCAMLFIND"; then
    AC_MSG_ERROR([ocamlfind not found, check your PATH])
fi
AC_SUBST(OCAMLFIND)

# check for ocamlfind
AC_PATH_PROG(OCAMLMKLIB, ocamlmklib)
if test -z "$OCAMLMKLIB"; then
    AC_MSG_ERROR([ocamlmklib not found, check your PATH])
fi

# Checking CamlImages
AC_MSG_CHECKING([for camlimages])
CAMLIMAGES_VERSION=`$OCAMLFIND query -format '%v' camlimages`
if test -z "$CAMLIMAGES_VERSION"; then
  AC_MSG_RESULT([no])
  HAVE_CAMLIMAGES="false"
  # unquote this if camlimages may be missing
  # currently, configure will fail below if because neither native not bytecode   # versions will be built
  AC_MSG_ERROR([you need camlimages, with minimal 4.0.0 version])
else
  AC_MSG_RESULT([yes])
  HAVE_CAMLIMAGES="true"
  CAMLIMAGES_PACKAGES="camlimages.core camlimages.graphics camlimages.freetype"
  CAMLIMAGES_INCLUDES=`$OCAMLFIND query -i-format -separator ' ' $CAMLIMAGES_PACKAGES`
  CAMLIMAGES_LIBS_NATIVE=`$OCAMLFIND query -a-format -separator ' ' -recursive -predicates native $CAMLIMAGES_PACKAGES`
  CAMLIMAGES_LIBS_BYTE=`$OCAMLFIND query -a-format -separator ' ' -recursive -predicates byte $CAMLIMAGES_PACKAGES`
  CAMLIMAGES_LPATH=`$OCAMLFIND query -format -L%d camlimages`
  dnl CAMLIMAGES_MKLIBS="-lcamlimages_freetype"
  CAMLIMAGES_CCLIBS="-cclib -lcamlimages_freetype_stubs"
  AC_SUBST(CAMLIMAGES_INCLUDES)
  AC_SUBST(CAMLIMAGES_LIBS_NATIVE)
  AC_SUBST(CAMLIMAGES_LIBS_BYTE)
  AC_SUBST(CAMLIMAGES_MKLIBS)
  AC_SUBST(CAMLIMAGES_CCLIBS)
  AC_SUBST(CAMLIMAGES_LPATH)
fi
AC_SUBST(HAVE_CAMLIMAGES)

# Whether to build a dynamic library for advi
AM_CONDITIONAL([BUILD_ADVI_LIB], true)

# Check versions to build
AC_ARG_ENABLE(
    [native-program],
    AC_HELP_STRING(
	[--enable-native-program],
	[build native version of program (default)]
    ),
    [case "$enableval" in
	yes) enable_native=$enableval;;
	no)  enable_native=$enableval;;
	*)   AC_MSG_ERROR([bad value $enableval for --enable-native-program]);;
    esac],
    [enable_native=yes]
)
AC_MSG_CHECKING([wether to build native program])
build_native=no
if test -n "$OCAMLOPT" \
    && test -n "$CAMLIMAGES_LIBS_NATIVE" \
    && test "$enable_native" = yes; then
    AC_MSG_RESULT([yes])
    build_native=yes
else
    AC_MSG_RESULT([no])
    build_native=no
fi
AM_CONDITIONAL([BUILD_NATIVE], [test "$build_native" = yes])

AC_ARG_ENABLE(
    [bytecode-program],
    AC_HELP_STRING(
	[--enable-bytecode-program],
	[build bytecode version of program]
    ),
    [case "$enableval" in
	yes) enable_bytecode=$enableval;;
	no)  enable_bytecode=$enableval;;
	*)   AC_MSG_ERROR([bad value $enableval for --enable-bytecode-program]);;
    esac],
    [enable_bytecode=yes]
)
AC_MSG_CHECKING([wether to build bytecode program])
build_bytecode=no
if test -n "$OCAMLC" \
    && test -n "$CAMLIMAGES_LIBS_BYTE" \
    && test "$enable_bytecode" = yes; then
    AC_MSG_RESULT([yes])
    build_bytecode=yes
else
    AC_MSG_RESULT([no])
    build_bytecode=no
fi
AM_CONDITIONAL([BUILD_BYTECODE], [test "$build_bytecode" = yes])

if test "$build_native" = no && test "$build_bytecode" = no; then
    AC_MSG_ERROR([neither native nor bytecode program selected])
fi

# checking for X
AC_PATH_XTRA
AC_SUBST(X_LIBS)
AC_SUBST(X_PRE_LIBS)
AC_SUBST(X_EXTRA_LIBS)
AC_SUBST(X_CFLAGS)

# Checking for xinerama support
# BROKEN, optional ???
HAVE_XINERAMA="false"
AC_CHECK_HEADER(
   [X11/extensions/Xinerama.h],
   [AC_CHECK_LIB(
	[Xinerama],
	[XineramaQueryScreens],
	[HAVE_XINERAMA="true"
	 XINERAMA_CFLAGS="-DHAVE_XINERAMA"
	 XINERAMA_MKLIB="-lXinerama"
	 XINERAMA_CCLIB="-cclib -lXinerama"])]
)
AC_SUBST(HAVE_XINERAMA)
AC_SUBST(XINERAMA_CFLAGS)
AC_SUBST(XINERAMA_MKLIB)
AC_SUBST(XINERAMA_CCLIB)

# Checking TeX 
AC_PATH_PROG(KPSEXPAND, kpsexpand)
if test -z "$KPSEXPAND"; then
    AC_MSG_ERROR([kpsexpand not found, check your PATH])
fi

AC_MSG_CHECKING([TeX root path])
texdir=`$KPSEXPAND '$TEXMFMAIN'`
AC_MSG_RESULT([$texdir])
if test -z "$texdir"; then
    AC_MSG_ERROR([no TeX root path found, check your setup])
fi

AC_MSG_CHECKING([LaTeX root path])
if test -d "$texdir/tex/latex"; then
    AC_MSG_RESULT([$texdir/tex/latex])
    # this ugly stuff, shamelessly stolen from lispdir computation, is supposed
    # to find latex resources installation directory expressed in term of prefix
    # the ugly @<:@^\/@:>@ is meant to express [^\/] regexp
    latexdir=`echo $texdir/tex/latex | sed -n \
	-e 's,/$,,' \
	-e '/.*\/lib\/@<:@^\/@:>@*\/tex\/latex$/{s,.*/lib/\(@<:@^\/@:>@*\/tex\/latex\)$,${libdir}/\1,;p;q;}' \
	-e '/.*\/share\/@<:@^\/@:>@*\/tex\/latex$/{s,.*/share/\(@<:@^\/@:>@*\/tex\/latex\),${datadir}/\1,;p;q;}'`
else 
    AC_MSG_RESULT([not found])
    AC_MSG_WARN([no LaTeX root path found, check your setup])
fi
AC_SUBST(latexdir)

# # Checking gunzip and bunzip2. not mandatory. 
# AC_ARG_WITH([GUNZIP],[AS_HELP_STRING([--with-gunzip=ARG],
#                                  [Use gunzip command ARG])],
#             [],[with_GUNZIP=maybe])
# if test "$with_GUNZIP" = maybe
#    then AC_PATH_PROG([GUNZIP], [gunzip])
#    elif  test "$with_GS" = no
#    then GUNZIP=""
#    else GUNZIP="$with_GUNZIP"
# fi
# AC_SUBST(GUNZIP)

# AC_ARG_WITH([BUNZIP],[AS_HELP_STRING([--with-gunzip=ARG],
#                                  [Use bunzip2 command ARG])],
#             [],[with_BUNZIP=maybe])
# if test "$with_BUNZIP" = maybe
#    then AC_PATH_PROG([BUNZIP], [bunzip2])
#    elif  test "$with_GS" = no
#    then BUNZIP=""
#    else BUNZIP="$with_BUNZIP"
# fi
# AC_SUBST(BUNZIP)


# Checking tools needed for building documentation
# only latex is mandatory, other are optional
# as documentation is distributed anyway
AC_PATH_PROG([LATEX], [latex])
if test -z "$LATEX"; then
    AC_MSG_WARN([latex not found, you won't be able to rebuild documention])
fi
AC_PATH_PROG([MAKEINDEX], [makeindex])
if test -z "$MAKEINDEX"; then
    AC_MSG_WARN([makeindex not found, you won't be able to rebuild the index])
fi
AC_PATH_PROG([DVIPS], [dvips])
if test -z "$DVIPS"; then
    AC_MSG_WARN([dvips not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_DVIPS, [test -n "$DVIPS"])
AC_PATH_PROG([DVIPDFM], [dvipdfm])
if test -z "$DVIPDFM"; then
    AC_MSG_WARN([dvips not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_DVIPDFM, [test -n "$DVIPDFM"])
AC_PATH_PROG([HEVEA], [hevea])
if test -z "$HEVEA"; then
    AC_MSG_WARN([hevea not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_HEVEA, [test -n "$HEVEA"])
AC_PATH_PROG([HACHA], [hacha])
if test -z "$HACHA"; then
    AC_MSG_WARN([hacha not found, you won't be able to rebuild all documention])
fi
AM_CONDITIONAL(HAVE_HACHA, [test -n "$HACHA"])
AC_PATH_PROG([CONVERT], [convert])
if test -z "$CONVERT"; then
    AC_MSG_WARN([convert not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_CONVERT, [test -n "$CONVERT"])
AC_PATH_PROG([MONTAGE], [montage])
if test -z "$MONTAGE"; then
    AC_MSG_WARN([montage not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_MONTAGE, [test -n "$MONTAGE"])

# Checking gs

AC_ARG_WITH([GS],[AS_HELP_STRING([--with-gs=ARG],
                                 [Use ghostscript command ARG])],
            [],[with_GS=maybe])
HAVE_GS="false"
if test "$with_GS" = no
then :
elif test "$with_GS" = maybe
then 
   AC_PATH_PROG([GS], [gs])
   if test -n "$GS" ; then
       AC_MSG_CHECKING([if gs version >= 6.52]) 
       gs_version=`$GS -v | awk '/Ghostscript/ {print $3}'`
       gs_major_version=`echo $gs_version | cut -d. -f1`
       gs_minor_version=`echo $gs_version | cut -d. -f2`

       if expr                                \
           \( $gs_major_version \> 6 \)       \
           \|                                 \
           \(                                 \
               \( $gs_major_version \= 6 \)   \
               \&                             \
               \( $gs_minor_version \>= 52 \) \
           \) > /dev/null; then
           AC_MSG_RESULT([yes])
           HAVE_GS="true"
       else
           AC_MSG_RESULT([no])
           with_GS=""
       fi
   fi
else 
   GS="$with_GS"
   HAVE_GS=true
fi
AC_SUBST(HAVE_GS)
AC_SUBST(GS)

AC_SUBST(dvidir)
AC_SUBST(sysconfdir)

AC_CONFIG_FILES([Makefile
		 src/Makefile
		 doc/Makefile
		 doc/pngs/Makefile
		 tex/Makefile])
AC_OUTPUT()

cat <<EOF
Configuration summary

Language:    $OCAMLC version $OCAMLVERSION
Camlimages:  $HAVE_CAMLIMAGES
LablTk:      $HAVE_LABLTK
Resources:   $dvidir
Hevea:       $HEVEA
Xinerama:    $HAVE_XINERAMA
HAVE_GS:     $HAVE_GS
GS:          $GS  
gunzip:      $GUNZIP
bunzip2:     $BUNZIP
EOF
