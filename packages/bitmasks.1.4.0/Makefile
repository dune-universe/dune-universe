# ################################################################################################ #
# MetaStack Solutions Ltd.                                                                         #
# ################################################################################################ #
# BitMask Sets                                                                                     #
# ################################################################################################ #
# Copyright (c) 2013-20 MetaStack Solutions Ltd.                                                   #
# ################################################################################################ #
# Author: David Allsopp                                                                            #
# 27-Dec-2013                                                                                      #
# ################################################################################################ #
# Redistribution and use in source and binary forms, with or without modification, are permitted   #
# provided that the following two conditions are met:                                              #
#     1. Redistributions of source code must retain the above copyright notice, this list of       #
#        conditions and the following disclaimer.                                                  #
#     2. Neither the name of MetaStack Solutions Ltd. nor the names of its contributors may be     #
#        used to endorse or promote products derived from this software without specific prior     #
#        written permission.                                                                       #
#                                                                                                  #
# This software is provided by the Copyright Holder 'as is' and any express or implied warranties  #
# including, but not limited to, the implied warranties of merchantability and fitness for a       #
# particular purpose are disclaimed. In no event shall the Copyright Holder be liable for any      #
# direct, indirect, incidental, special, exemplary, or consequential damages (including, but not   #
# limited to, procurement of substitute goods or services; loss of use, data, or profits; or       #
# business interruption) however caused and on any theory of liability, whether in contract,       #
# strict liability, or tort (including negligence or otherwise) arising in any way out of the use  #
# of this software, even if advised of the possibility of such damage.                             #
# ################################################################################################ #

DUNE=dune

build:
	$(DUNE) build @install

bitmasks.install:
	$(DUNE) build $@ --promote-install-files

doc: bitmasks.install
	$(DUNE) build @doc
	@sed -e '$$d' $< | fgrep -v _doc > $<.tmp
	@find _build/default/_doc/_html -type f -not -name .dune-keep | \
    sed -e 's/.*/  "\0"/' -e 's|_doc/_html/\(.*/.*\)|\0 {"\1}|' >> $<.tmp
	@echo ']' >> $<.tmp
	@mv $<.tmp $<

test:
	$(DUNE) runtest

all: build doc test

install:
	$(DUNE) install $(INSTALLFLAGS)

uninstall:
	$(DUNE) uninstall $(UNINSTALLFLAGS)

reinstall:
	$(DUNE) uninstall $(REINSTALLFLAGS)
	$(DUNE) install $(REINSTALLFLAGS)

clean:
	$(DUNE) clean

distclean:
	$(DUNE) clean

clean-working-dir:
	@test -z "$$(git status --porcelain)"

gh-pages: doc clean-working-dir
	rm -rf doc-temp
	opam-installer --prefix=doc-temp bitmasks.install
	rm -f doc-temp/doc/bitmasks/CHANGES.txt doc-temp/doc/bitmasks/README.md
	git checkout gh-pages
	cp -a doc-temp/doc/bitmasks/* .
	rm -rf doc-temp
	git add -N .

.PHONY: build doc test all install uninstall reinstall clean distclean gh-pages clean-working-dir
