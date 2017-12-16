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
LIBS += dyp.cma str.cma ANSITerminal.cma 

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR += -I /home/pogodall/.opam/4.05.0/lib/dyp  -I /home/pogodall/.opam/4.05.0/lib/ANSITerminal

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils

# Source files in the right order of dependance
#ML = error.ml abstract_syntax.ml interface.ml environment.ml entry.ml parser.ml lexer.ml data_parsing.ml
ML = abstract_syntax.ml lambda.ml type_inference.ml varUnionFind.ml typeInference.ml

#EXE_SOURCES = type_inference_test.ml typeInference_test.ml
EXE_SOURCES = typeInference_test.ml

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common
