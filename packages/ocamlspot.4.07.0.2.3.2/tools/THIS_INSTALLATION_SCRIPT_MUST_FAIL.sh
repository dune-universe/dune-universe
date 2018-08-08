#!/bin/sh

# DO NOT USE THIS SCRIPT TO INSTALL OCAMLSPOT.
# IF YOU HAVE ANY PROBLEM WITH THIS SCRIPT, THIS IS JUST SINCE YOU USED IT.

# DO NOT USE THIS SCRIPT TO INSTALL OCAMLSPOT
# IF YOU HAVE ANY PROBLEM WITH THIS SCRIPT, THIS IS JUST SINCE YOU USED IT.

# DO NOT USE THIS SCRIPT TO INSTALL OCAMLSPOT
# IF YOU HAVE ANY PROBLEM WITH THIS SCRIPT, THIS IS JUST SINCE YOU USED IT.

# DO NOT USE THIS SCRIPT TO INSTALL OCAMLSPOT
# IF YOU HAVE ANY PROBLEM WITH THIS SCRIPT, THIS IS JUST SINCE YOU USED IT.

# DO NOT USE THIS SCRIPT TO INSTALL OCAMLSPOT
# IF YOU HAVE ANY PROBLEM WITH THIS SCRIPT, THIS IS JUST SINCE YOU USED IT.

set -e

echo 'Oh, you fail, because you use this script.'

# OCaml+annot
 
export OCAML_ANNOT=1

if [ ! -d mutated_ocaml ]; then
  hg clone https://bitbucket.org/camlspotter/mutated_ocaml -b annot
fi
(cd mutated_ocaml; mkdir compilerlibs; ./configure; make core coreboot world opt opt.opt install)
 
# OCamlSpot

(cd ..; make all opt install)

# I believe there is wget/omake installed.

if [ ! -f findlib-1.3.3.tar.gz ]; then
  wget http://download.camlcity.org/download/findlib-1.3.3.tar.gz
fi
tar zxvf findlib-1.3.3.tar.gz
(cd findlib-1.3.3; ./configure; make all opt install)

# SpotLib

if [ ! -d spotlib ]; then
  hg clone https://bitbucket.org/camlspotter/spotlib
fi
(cd spotlib; echo no | omake --install; omake; omake install)

# SpotInstall

if [ ! -d spotinstall ]; then
  hg clone https://bitbucket.org/camlspotter/spotinstall
fi
(cd spotinstall; echo no | omake --install; omake; omake install)

# Back to OCaml to install cmts

(cd mutated_ocaml; spotinstall ocaml)

# Something miraculous happend and you are here.

echo 'Congratulations! I never thought you would see this message!.'
echo 'I propose to set OCAML_ANNOT=1'
