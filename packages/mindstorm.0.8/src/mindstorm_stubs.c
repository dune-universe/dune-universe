/* File: mindstorm_stubs.c

   Copyright (C) 2014

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#if defined _WIN32 || defined WIN32 || defined(__CYGWIN__)
#include "mindstorm_win.c"
#elif defined __unix__
#include "mindstorm_unix.c"
#endif

