##########################################################################
#  ocaml-gettext: a library to translate messages                        #
#                                                                        #
#  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 2.1 of the License, or (at your option) any later version;    #
#  with the OCaml static compilation exception.                          #
#                                                                        #
#  This library is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  Lesser General Public License for more details.                       #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with this library; if not, write to the Free Software   #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   #
#  USA                                                                   #
##########################################################################

default: build test

GENERATED_FILES=src/lib/gettext/base/gettextConfigGen.ml
$(GENERATED_FILES): configure.ml
	ocaml configure.ml

build: $(GENERATED_FILES)
	dune build @install

doc: $(GENERATED_FILES)
	dune build @doc

test: $(GENERATED_FILES)
	dune build examples/library/examplesLibrary.a
	dune build examples/gui/examplesGUI.a
	dune build examples/program/program.exe
	dune runtest

all: $(GENERATED_FILES)
	dune build @all
	dune runtest

install: build
	dune install

uninstall: all
	dune uninstall

clean:
	dune clean

bench:
	dune exec test/bench/bench.exe

headache: distclean
	headache -h .header \
		-c .headache.config \
		`find $(CURDIR)/ -type d -name .svn -prune -false -o -type f`

deploy: doc test
	dune-release lint
	dune-release tag
	git push --all
	git push --tag
	dune-release

eol:
	find ./ -name _build -prune -false -or -name "*.ml" | xargs grep -r -e "  *$$"

.PHONY: build doc test all uninstall clean install bench deploy

