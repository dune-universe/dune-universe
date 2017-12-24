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
#  $Rev:: 211                          $:  Revision of last commit       #
#  $Author:: pogodall                  $:  Author of last commit         #
#  $Date:: 2008-10-21 17:11:50 +0200 (#$:  Date of last commit           #
#                                                                        #
##########################################################################

include ../Makefile.master

###############################
#                             #
# Set the following variables #
#                             #
###############################

# Used libraries
LIBS += 

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR +=

WARNINGS = -w yzd -warn-error A

# Directories to which the current source files depend on
PREVIOUS_DIRS = 

# Source files in the right order of dependance

ML = datalog_signature.ml string_map.ml int_map.ml int_set.ml program.ml pmcfg.ml pmcfg_to_datalog.ml oriented_pmcfg.ml lexicalizer.ml pmcfg_syn.ml program_printer.ml prefix_correct_program.ml adornment2.ml magic_set_rewritting2.ml kanazawa_transform.ml datalog_solver.ml

EXE_SOURCES = test.ml

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

