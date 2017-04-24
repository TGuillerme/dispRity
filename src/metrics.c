/* 
 * matrix handler functions
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

#include "dispRity.h"

// Matrix SEXP handler
// SEXP matrix_handler(SEXP x, SEXP smethod, SEXP attrs)
// {
//     // Define the variable
//     SEXP result;
//     int nr = nrows(x), nc = ncols(x), method = asInteger(smethod);
//     int diag = 0;
//     R_xlen_t N;
//     N = (R_xlen_t)nr * (nr-1)/2; /* avoid int overflow for N ~ 50,000 */
//     PROTECT(result = allocVector(REALSXP, N));
//     if(TYPEOF(x) != REALSXP) x = coerceVector(x, REALSXP);
//     PROTECT(x);
    
//     // Calculate the distance matrix
//     R_distance(REAL(x), &nr, &nc, REAL(result), &diag, &method);
    
//     // Wrap up the results
//     SEXP names = getAttrib(attrs, R_NamesSymbol); // Row/column names attributes
//     for (int i = 0; i < LENGTH(attrs); i++) {
//         setAttrib(result, install(translateChar(STRING_ELT(names, i))), VECTOR_ELT(attrs, i));        
//     }

//     UNPROTECT(2);

//     return result;
// }