/* 
 * dispRity C source code header
 *
 * This file contains structures, data definitions and function prototypes for the dispRity R package
 * 
 *
 *
 *  Parts of this script are modifications of r-source/src/library/stats/src/distance.c
 *  from https://github.com/wch/r-source by Thomas Guillerme (guillert@tcd.ie)
 *
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2016   The R Core Team
 *  Copyright (C) 2002, 2004  The R Foundation
 *
 *  GNU General Public License available at https://www.R-project.org/Licenses/
 */


// ~~~~~~~~~~~~~~~~~~~~
// Headers
// ~~~~~~~~~~~~~~~~~~~~

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <float.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

#ifdef _OPENMP
# include <R_ext/MathThreads.h>
#endif

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#ifdef R_160_and_older
#define both_non_NA both_FINITE
#else
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))
#endif

// ~~~~~~~~~~~~~~~~~~~~
// Function prototypes
// ~~~~~~~~~~~~~~~~~~~~

// char.diff
double          character_to_numeric(char c);
void            Normalise_single_character(double *vector, int count);
void            R_distance(double *x, int *nr, int *nc, double *d, int *diag, int *method);
SEXP            C_char_diff(SEXP x, SEXP smethod, SEXP attrs);

