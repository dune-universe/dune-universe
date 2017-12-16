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

include ../Makefile.master

###############################
#                             #
# Set the following variables #
#                             #
###############################

# Used libraries
LIBS += dyp.cma str.cma

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR += -I /home/pogodall/.opam/4.05.0/lib/dyp

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils ../logic ../datalog.prover

# Source files in the right order of dependance

ML = error.ml type_system.ml interface.ml signature.ml reduction.ml acg_lexicon.ml

EXE_SOURCES =

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

