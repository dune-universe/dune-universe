#!/bin/bash

package_version='2.0.0'

function valdef () {
    name="$1"
    value="$2"
    echo "  -e 's|@${name}@|${value}|g' \\"
}

function strdef () {
    name="$1"
    value="$2"
    echo "  -e 's|@${name}@|${value}|g' \\"
}

function warn () {
    echo $* 1>&2
}    
function error () {
    warn $*
    exit 1
}    

# FIXED by calling opam exex dune -- in Makefiles
# if test "$(opam exec which -- dune)" != "$(which dune)"
# then
#     warn 'Your PATH disagree with your opam switches'
#     warn 'Run eval $(opam env) to update the current shell environment'
#     warn 'Or wrap your <COMMAND> with `opam exec <COMMAND> --'
#     error 
# fi    


build_date=$(date +%Y-%m-%d)

if gs_path=$(which gs)
then have_gs=true
else have_gs=false
fi


function opamvar () {
    name="$1"
    var="$2"
    suffix="$3"
    unset opamdir
    opamdir=$(opam config var ${var})
    case "${opamdir}" in "") exit 1;; esac
    case "$suffix" in
        "") strdef ${name} "${opamdir}" ;;
        *) strdef ${name} "${opamdir}/${suffix}" ;;
    esac
}

case "$#" in
    0)
        exit 1;;
    *)
        target="$1" 
        exec >${target} ;;
esac

exec_prefix=undefined
libdir=${exec_prefix}/lib
datadir=${prefix}/share

# kpsewhich
if kpsewhich_path=$(which kpsewhich)
then
    if
        article=$(${kpsewhich_path} article.cls) &&
            test ! -z ${article}
    then
        :
    else
         warn 'kpsewhich command:'
         warn "    ${kpsewhiich_path} arcticle.cls"
         error  'failed, check your setup!'
    fi
else
    error 'No kpsewhich command found, check your setup!'
fi

# TEX ROOTS
texdir=''
if which kpsexpand > /dev/null
then
    texdir=$(kpsexpand '$TEXMFMAIN')
else 
    texdir=$(expr "${article}" : '\(.*\)/tex/latex/base/*$')
fi

if test -z ${texdir}
then
    warn 'No TeX root path found, check your setup'
    texdir=undefined
    latexdir=undefined
else
    latexdir="${texdir}/tex/latex"
fi


if test ! -d "${latexdir}"
then
    latexdir=undefined
    warn 'no LaTeX root path found, check your setup'
    warn 'We can compile advi, but it may not find latex files'
    warn "and we won't know where to install sty files"
fi

#### Producing configure.sh

echo '#/bin/bash'
echo
echo 'src="$1"'
echo 'dest="$2"'
echo
echo 'LC_CTYPE=C && LANG=C sed \'

opamvar ADVI_MANDIR man 
opamvar ADVI_ETCDIR etc advi
opamvar ADVI_DOCDIR doc advi
opamvar ADVI_TEXDIR share advi

strdef "KPSEWHICH" ${kpsewhich_path}
strdef "TEXDIR" ${texdir}
strdef "LATEXDIR" ${latexdir}

strdef "PACKAGE_VERSION" ${package_version}
strdef "BUILD_DATE" ${build_date}
valdef "HAVE_GS" ${have_gs}
strdef "GS_PATH" ${gs_path}

valdef HAVE_CAMLIMAGES true

echo ' $src > $dest '

