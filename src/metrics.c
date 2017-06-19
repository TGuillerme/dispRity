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

// REMOVE and replace by #include "dispRity.h" after DEV
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


void dummy_mean(double *matrix, double *results, int n_rows, int n_cols)
{
    // Initialise the values
    // double results = 0;
    int i, counts = 0;

    //Go through each value of the matrix
    for(i = 0 ; i < n_rows*n_cols ; i++) {
        // Add up the results
        *results = *results + matrix[i];

        //Increment the counter
        counts++;
    }

    // divide by counts
    *results = *results/counts;

    // return(results);
}


void apply_metric(double *matrix, double *results, int *n_rows, int *n_cols, int *level, int *metric)
{
    // Initialise the function
    void (*metric_fun)(double*, double*, int, int) = NULL;
    // int row, col = 0;

    // Select the metric
    // switch(*metric) {
    //     case 0: // dummy_mean
    //         metric_fun = dummy_mean;
    //         break;
    //     case 1: // variances
    //         metric_fun = variances_fun;
    //         break;
    //     case 2: // ranges
    //         metric_fun = ranges_fun;
    //         break;
    //     case 3: // centroids
    //         metric_fun = centroids_fun;
    //         break;
    // }
    metric_fun = dummy_mean;

    // Apply by level
    // level 1
    metric_fun(matrix, results, *n_rows, *n_cols);


}

SEXP matrix_handler(SEXP matrix, SEXP level_in, SEXP metric_in)
{
    // Input
    int n_rows = nrows(matrix);
    int n_cols = ncols(matrix);
    int level = asInteger(level_in);
    int metric = asInteger(metric_in);

    // Output level
    switch(level) {
        case 1 : // level 1 output
            n_cols = n_rows = 1;
            break;
        case 2 : // level 2 output
            n_cols = 1;
            break;
        case 3 : // level 2 output
            n_rows = 1;
            break;
    }

    // Making sure the matrix is of class double
    if(TYPEOF(matrix) != REALSXP) {
        matrix = coerceVector(matrix, REALSXP);
    }
    PROTECT(matrix);

    // Allocate output memory
    SEXP output = PROTECT(allocVector(REALSXP, n_rows*n_cols));

    // Apply the metric
    apply_metric(REAL(matrix), REAL(output), &n_rows, &n_rows, &level, &metric);

    // Free memory
    UNPROTECT(2);

    // Return
    return output;
}
