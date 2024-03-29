###
# Bitwuzla: Satisfiability Modulo Theories (SMT) solver.
#
# This file is part of Bitwuzla.
#
# Copyright (C) 2007-2021 by the authors listed in the AUTHORS file.
#
# See COPYING for more information on using this software.
##
# Find CaDiCaL
# CaDiCaL_FOUND - found CaDiCaL lib
# CaDiCaL_INCLUDE_DIR - the CaDiCaL include directory
# CaDiCaL_LIBRARIES - Libraries needed to use CaDiCaL

find_path(CaDiCaL_INCLUDE_DIR NAMES ccadical.h)
find_library(CaDiCaL_LIBRARIES NAMES cadical)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CaDiCaL
  DEFAULT_MSG CaDiCaL_INCLUDE_DIR CaDiCaL_LIBRARIES)

mark_as_advanced(CaDiCaL_INCLUDE_DIR CaDiCaL_LIBRARIES)
if(CaDiCaL_LIBRARIES)
  message(STATUS "Found CaDiCaL library: ${CaDiCaL_LIBRARIES}")
endif()
