/*
Copyright (c) 2003 - 2014 Armin Biere, ETH Zurich, JKU Linz.

All rights reserved. Redistribution and use in source and binary forms, with
or without modification, are permitted provided that the following
conditions are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

  3. All advertising materials mentioning features or use of this software
     must display the following acknowledgement:

	  This product includes software developed by 
	  Armin Biere, Johannes Kepler University, Linz, Austria.

  4. Neither the name of the University nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.
   
THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _quantor_h_INCLUDED
#define _quantor_h_INCLUDED

/*------------------------------------------------------------------------*/

#include <stdio.h>

/*------------------------------------------------------------------------*/

enum QuantorResult
{
  QUANTOR_RESULT_UNKNOWN = 0,
  QUANTOR_RESULT_SATISFIABLE = 10,
  QUANTOR_RESULT_UNSATISFIABLE = 20,
  QUANTOR_RESULT_TIMEOUT = 30,
  QUANTOR_RESULT_SPACEOUT = 40,
};

typedef enum QuantorResult QuantorResult;

/*------------------------------------------------------------------------*/

enum QuantorQuantificationType
{
  QUANTOR_EXISTENTIAL_VARIABLE_TYPE = 0,
  QUANTOR_UNIVERSAL_VARIABLE_TYPE = 1,
};

typedef enum QuantorQuantificationType QuantorQuantificationType;

/*------------------------------------------------------------------------*/

typedef struct Quantor Quantor;

/*------------------------------------------------------------------------*/

const char *quantor_id (void);
const char *quantor_copyright (void);
const char *quantor_version (void);

/*------------------------------------------------------------------------*/

int quantor_main (int, char **);

/*------------------------------------------------------------------------*/

Quantor * quantor_new (void);
void quantor_delete (Quantor *);

/*------------------------------------------------------------------------*/

void quantor_set_log (Quantor *, FILE *);

/*------------------------------------------------------------------------*/

QuantorResult quantor_sat (Quantor *);

/*------------------------------------------------------------------------*/

void quantor_print (Quantor *, FILE *);
void quantor_stats (Quantor *, FILE *);

/*------------------------------------------------------------------------*/
/* Start a new quantifier scope. This is only allowed before any clause is
 * added.  The variables of a scope are added with 'quantor_add'.  The scope
 * is closed by adding '0' with 'quantor_add'.  The result is zero if no
 * error occured, otherwise it is a non zero error string describing the
 * problem.
 */
const char * quantor_scope (Quantor *, QuantorQuantificationType);

/*------------------------------------------------------------------------*/
/* Add positive literals to a quantifier scope or literals of a new
 * clause.  The scope and the clause are both terminated by calling
 * 'quantor_add' with a zero literal argument.  If an error occurs a non
 * zero error string is returned, otherwise zero.
 */
const char * quantor_add (Quantor *, int lit);

/*------------------------------------------------------------------------*/
/* Returns the assigned value of variable with index 'idx'.  The result is
 * '0' if the variables is assigned to false, '1' if it is assigned to true
 * or '-1' if the variable with the given index is either not an external
 * variable of the outermost scope, or if it was not assigned.
 */
int quantor_deref (Quantor *, int idx);

/*------------------------------------------------------------------------*/
/* This is a zero terminated list of literals that represent all the
 * external assigned variables of the outermost scope.  The result is valid
 * until this function is called again.
 */
const int * quantor_assignment (Quantor *);

/*------------------------------------------------------------------------*/

#endif
