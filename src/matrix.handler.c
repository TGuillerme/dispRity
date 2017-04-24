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

// // Allowed methods
// enum { GOWER=1};

// // R_distance function (R::dist())
// void R_distance(double *x, int *nr, int *nc, double *d, int *diag, int *method)
// {
//     int dc, i, j;
//     size_t  ij;  /* can exceed 2^31 - 1 */
//     double (*distfun)(double*, int, int, int, int) = NULL;

//     //Open MPI
// #ifdef _OPENMP
//     int nthreads;
// #endif
//     distfun = R_Gower;

//     dc = (*diag) ? 0 : 1; /* diag=1:  we do the diagonal */

// #ifdef _OPENMP
//     if (R_num_math_threads > 0)
//     nthreads = R_num_math_threads;
//     else
//     nthreads = 1; /* for now */
//     if (nthreads == 1) {
//     /* do the nthreads == 1 case without any OMP overhead to see
//        if it matters on some platforms */
//     ij = 0;
//     for(j = 0 ; j <= *nr ; j++)
//         for(i = j+dc ; i < *nr ; i++)
//         d[ij++] = distfun(x, *nr, *nc, i, j);
//     }
//     else
//     /* This produces uneven thread workloads since the outer loop
//        is over the subdiagonal portions of columns.  An
//        alternative would be to use a loop on ij and to compute the
//        i and j values from ij. */
// #pragma omp parallel for num_threads(nthreads) default(none)    \
//     private(i, j, ij)                       \
//     firstprivate(nr, dc, d, method, distfun, nc, x)
//     for(j = 0 ; j <= *nr ; j++) {
//         ij = j * (*nr - dc) + j - ((1 + j) * j) / 2;
//         for(i = j+dc ; i < *nr ; i++)
//         d[ij++] = distfun(x, *nr, *nc, i, j);
//     }
// #else
//     ij = 0;
//     for(j = 0 ; j <= *nr ; j++)
//     for(i = j+dc ; i < *nr ; i++)
//         d[ij++] = distfun(x, *nr, *nc, i, j);
// #endif
// }

// Matrix SEXP handler
SEXP matrix_handler(SEXP x, SEXP smethod, SEXP attrs)
{
    // Define the variable
    SEXP result;
    int nr = nrows(x), nc = ncols(x), method = asInteger(smethod);
    int diag = 0;
    R_xlen_t N;
    N = (R_xlen_t)nr * (nr-1)/2; /* avoid int overflow for N ~ 50,000 */
    PROTECT(result = allocVector(REALSXP, N));
    if(TYPEOF(x) != REALSXP) x = coerceVector(x, REALSXP);
    PROTECT(x);
    
    // Calculate the distance matrix
    R_distance(REAL(x), &nr, &nc, REAL(result), &diag, &method);
    
    // Wrap up the results
    SEXP names = getAttrib(attrs, R_NamesSymbol); // Row/column names attributes
    for (int i = 0; i < LENGTH(attrs); i++) {
        setAttrib(result, install(translateChar(STRING_ELT(names, i))), VECTOR_ELT(attrs, i));        
    }

    UNPROTECT(2);

    return result;
}