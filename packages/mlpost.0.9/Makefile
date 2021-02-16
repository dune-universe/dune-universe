##########################################################################
#                                                                        #
#  Copyright (C) Johannes Kanig, Stephane Lescuyer                       #
#  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           #
#                                                                        #
#  This software is free software; you can redistribute it and/or        #
#  modify it under the terms of the GNU Library General Public           #
#  License version 2.1, with the special exception on linking            #
#  described in file LICENSE.                                            #
#                                                                        #
#  This software is distributed in the hope that it will be useful,      #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  #
#                                                                        #
##########################################################################

.PHONY: all test clean

all:
	dune build @install

test:
	dune runtest

doc:
	dune build @doc

# When adding new figures or examples
promote:
	dune build @promote --auto-promote || true  #For dune.inc
	dune build @promote --auto-promote || true  #For *.dune.inc
	dune runtest

headers:
	headache -c headache_config.txt -h header.txt \
	version/*.ml* README.txt tools/*.ml tools/*.ml[iyl] src/*.ml src/*.ml[iyl]

clean:
	dune clean

## Install should be replaced when `dune install` is improved (e.g. DESTDIR support)
install:
	dune install

release:
	@echo "Only the committed code is added in the archive, VERSION is \"$(VERSION)\""
	git archive --format=tar.gz --prefix mlpost-$(VERSION)/ -o mlpost-$(VERSION).tar.gz HEAD

fmt:
	dune build @fmt --auto-promote
