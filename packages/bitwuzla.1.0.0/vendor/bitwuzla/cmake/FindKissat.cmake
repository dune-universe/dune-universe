###
# Bitwuzla: Satisfiability Modulo Theories (SMT) solver.
#
# This file is part of Bitwuzla.
#
# Copyright (C) 2007-2021 by the authors listed in the AUTHORS file.
#
# See COPYING for more information on using this software.
##
# Find Kissat
# Kissat_FOUND - found Kissat lib
# Kissat_INCLUDE_DIR - the Kissat include directory
# Kissat_LIBRARIES - Libraries needed to use Kissat

find_path(Kissat_INCLUDE_DIR NAMES kissat.h)
find_library(Kissat_LIBRARIES NAMES kissat)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Kissat
  DEFAULT_MSG Kissat_INCLUDE_DIR Kissat_LIBRARIES)

mark_as_advanced(Kissat_INCLUDE_DIR Kissat_LIBRARIES)
if(Kissat_LIBRARIES)
  message(STATUS "Found Kissat library: ${Kissat_LIBRARIES}")
endif()

