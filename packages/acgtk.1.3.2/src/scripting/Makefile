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
LIBS += dyp.cma str.cma ANSITerminal.cma bigarray.cma cairo2.cma easy_format.cma biniou.cma yojson.cma ocf.cma

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR += -I /home/pogodall/.opam/4.05.0/lib/dyp -I /home/pogodall/.opam/4.05.0/lib/ANSITerminal -I /home/pogodall/.opam/4.05.0/lib/cairo2 -I /home/pogodall/.opam/4.05.0/lib/easy-format -I /home/pogodall/.opam/4.05.0/lib/yojson -I /home/pogodall/.opam/4.05.0/lib/biniou -I /home/pogodall/.opam/4.05.0/lib/ocf

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils ../datalog.prover ../logic ../acg-data ../grammars

# Source files in the right order of dependance

ML = scripting_errors.ml  rendering_config.ml diagram.ml tree.ml show_exts.ml show.ml show_text_dejavu.ml show_colors_solarized_dark.ml show_embellish_examples.ml functions.ml script_lexer.ml script_parser.ml

DYP = script_parser.dyp
CAMLLEX = script_lexer.mll

EXE_SOURCES = acg.ml

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

script_parser.ml: scripting_errors.cmi script_lexer.cmi functions.cmi \
    ../acg-data/error.cmi ../grammars/environment.cmi \
    ../logic/abstract_syntax.cmi

script_parser.cmo: scripting_errors.cmi script_lexer.cmo functions.cmi \
    ../acg-data/error.cmi ../grammars/environment.cmi \
    ../logic/abstract_syntax.cmi script_parser.cmi
script_parser.cmx: scripting_errors.cmx script_lexer.cmx functions.cmx \
    ../acg-data/error.cmx ../grammars/environment.cmx \
    ../logic/abstract_syntax.cmx script_parser.cmi

