##########################################################################
#                                                                        #
#                 ACG development toolkit                                #
#                                                                        #
#                  Copyright 2008 INRIA                                  #
#                                                                        #
#  More information on "http://acg.gforge.inria.fr/"                     #
#  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     #
#  Authors: see the AUTHORS file                                         #
#                                                                        #
#                                                                        #
#                                                                        #
#                                                                        #
#  $Rev::                              $:  Revision of last commit       #
#  $Author::                           $:  Author of last commit         #
#  $Date::                             $:  Date of last commit           #
#                                                                        #
##########################################################################

.PHONY: byte opt clean superclean test

#SUBDIRS= utils logic grammars acg-data scripting lambda datalog
#SUBDIRS= utils logic grammars acg-data scripting datalog reduction s_datalog
#SUBDIRS= utils logic datalog.prover acg-data grammars scripting datalog.solver
SUBDIRS= utils logic datalog.prover acg-data grammars scripting

ACG_DIR=scripting
ACGC_DIR=grammars

acg acg.opt:
	$(MAKE) -r -S -C $(ACG_DIR) $@

acgc acgc.opt:
	$(MAKE) -r -S -C $(ACGC_DIR) $@

byte: acg acgc

opt: acg.opt acgc.opt

test:
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) byte;)
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) opt;)



superclean clean:
	rm -rf *~ *.log
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) $@;)

