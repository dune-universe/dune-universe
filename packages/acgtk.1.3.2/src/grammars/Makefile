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
LIBDIR += -I /home/pogodall/.opam/4.05.0/lib/dyp -I /home/pogodall/.opam/4.05.0/lib/ANSITerminal

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils ../logic ../datalog.prover ../acg-data

# Source files in the right order of dependance
#ML = error.ml interface.ml environment.ml entry.ml syntactic_data_structures.ml acg_token.ml data_lexer.ml data_parser.ml  interactive.ml 
ML = environment.ml entry.ml acg_token.ml data_lexer.ml data_parser.ml

EXE_SOURCES =   acgc.ml

DYP = data_parser.dyp
CAMLLEX = data_lexer.mll


####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

# #DYPGEN_EXE = dypgen.opt
# #DYPGEN_EXE = $(DYPPATH)/dypgen/dypgen --ocamlc "$(LIBDIR) $(I_PREVIOUS_DIRS)"
# DYPGEN_EXE = dypgen.opt --no-pp --no-obj-type --ocamlc "$(LIBDIR) $(I_PREVIOUS_DIRS)"


# data_parser.ml : data_parser.dyp
# 	 $(DYPGEN_EXE) --noemit-token-type $<

# #data_parser.ml : data_parser.dyp
# #	$(DYPGEN_EXE)  --noemit-token-type $<

# data_lexer.ml : data_lexer.mll
# 	ocamllex.opt $<

data_parser.ml: ../utils/utils.cmi acg_token.cmi \
    data_lexer.cmi ../acg-data/error.cmi environment.cmi entry.cmi \
    ../logic/abstract_syntax.cmi

data_parser.cmo: ../utils/utils.cmi acg_token.cmi \
     data_lexer.cmo ../acg-data/error.cmi environment.cmi entry.cmi \
     ../logic/abstract_syntax.cmi data_parser.cmi
data_parser.cmx: ../utils/utils.cmx acg_token.cmx \
     data_lexer.cmx ../acg-data/error.cmx environment.cmx entry.cmx \
     ../logic/abstract_syntax.cmx data_parser.cmi
data_parser.ml: ../utils/utils.cmi acg_token.cmi \
