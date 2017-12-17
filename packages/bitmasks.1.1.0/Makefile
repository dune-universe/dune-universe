# ################################################################################################ #
# MetaStack Solutions Ltd.                                                                         #
# ################################################################################################ #
# BitMask Sets                                                                                     #
# ################################################################################################ #
# Copyright (c) 2013-17 MetaStack Solutions Ltd.                                                   #
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

JBUILDER=jbuilder

build:
	$(JBUILDER) build @install

bitmasks.install:
	$(JBUILDER) build $@

doc: bitmasks.install
	$(JBUILDER) build @doc
	@sed -e '$$d' $< | fgrep -v _doc > $<.tmp
	@find _build/default/_doc -type f -not -name .jbuilder-keep | \
    sed -e 's/.*/  "\0"/' -e 's/_doc\/\(.*\/.*\)/\0 {"\1}/' >> $<.tmp
	@echo ']' >> $<.tmp
	@mv $<.tmp $<

test:
	$(JBUILDER) runtest

all: build doc test

install:
	$(JBUILDER) install $(INSTALLFLAGS)

uninstall:
	$(JBUILDER) uninstall $(UNINSTALLFLAGS)

reinstall:
	$(JBUILDER) uninstall $(REINSTALLFLAGS)
	$(JBUILDER) install $(REINSTALLFLAGS)

clean:
	$(JBUILDER) clean

distclean:
	$(JBUILDER) clean

.PHONY: build doc test all install uninstall reinstall clean distclean
