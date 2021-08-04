###############################################################################
#               OCaml-RSS                                                     #
#                                                                             #
#   Copyright (C) 2013 Institut National de Recherche en Informatique         #
#   et en Automatique. All rights reserved.                                   #
#                                                                             #
#   This program is free software; you can redistribute it and/or modify      #
#   it under the terms of the GNU Lesser General Public License version       #
#   3 as published by the Free Software Foundation.                           #
#                                                                             #
#   This program is distributed in the hope that it will be useful,           #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#   GNU Library General Public License for more details.                      #
#                                                                             #
#   You should have received a copy of the GNU Library General Public         #
#   License along with this program; if not, write to the Free Software       #
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                  #
#   02111-1307  USA                                                           #
#                                                                             #
#   Contact: Maxence.Guesdon@inria.fr                                         #
#                                                                             #
#                                                                             #
###############################################################################

#
all:
	dune build

install:
	dune build @install
	dune install

doc: all
	dune build @doc

webdoc: doc
	rm -fr public
	cp -r _build/default/_doc/_html public

# archive :
###########
archive:
	git archive --prefix=ocamlrss-`git describe`/ `git describe` | gzip > /tmp/ocamlrss-`git describe`.tar.gz

# Cleaning :
############
clean:
	dune clean

# headers :
###########
HEADFILES=Makefile lib/*.ml lib/*.ml test/*.ml
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)

