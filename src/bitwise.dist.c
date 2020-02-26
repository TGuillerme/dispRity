/* 
 *  char.diff function
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

/* 
 * bitwise_compare function
 * 
 * Compares the distance between two characters
 *
 * @param char1, char2: two integers to compare (two cells in the matrix)
 * @param order: an integer used as logical whether to order characters (order != 0) or not (order == 0).
 * @returns an integer equal to the distance between both characters
 */
static inline int bitwise_compare(int char1, int char2, int order) {
    // initialise all values to 0
    int count = 0, result = 0, dist = 0, buffer = 0;
    
    // Comparing both characters
    result = char1 & char2;

    if(result == 0) {
        // If there is no union between the two characters, count the distance
        if(order == 0) {
            // printf("bitwise_compare: %i & %i = dist + %i\n", char1, char2, dist);
            // increment Fitch (easy)
            dist = 1;
        } else {
            // increment wagner
            if(char1 > char2){
                // Shift char1 until there is an intersection
                buffer = char1;
                while((buffer & char2) == 0) {
                    buffer = buffer >> 1;
                    count++;
                }
            } else {
                // Shift char2 until there is an intersection
                buffer = char2;
                while((buffer & char1) == 0) {
                    buffer = buffer >> 1;
                    count++;
                }
            }
            dist = count;
            // printf("bitwise_compare: %i & %i = dist + %i\n", char1, char2, dist);
        }
    }

    return dist;
}


// Calculating the hamming character distance
static double R_hamming(int *x, int nr, int nc, int i1, int i2, int translate, int order)
{

    // Declaring variables (result is int)    
    int vector1[nc], vector2[nc];
    int count = 0, i = 0, k = 0, diff = 0, dist = 0;
    double result = 0;

    //Isolating the two comparable characters
    for(i = 0 ; i < nc ; i++) {
        //if(both_non_NA(x[i1], x[i2])) {
        // if(!NA_INTEGER(x[i1]) && !NA_INTEGER(x[i2])) {
        if(x[i1] != NA_INTEGER && x[i2] != NA_INTEGER) {
            
            // Create the vectors
            vector1[count] = x[i1];
            vector2[count] = x[i2];

            //Increment the counter
            count++;
        }
        i1 += nr;
        i2 += nr;
    }

    // int loop;
    // printf("vector1 = ");
    // for(loop = 0; loop < count; loop++) {
    //     printf("%d ", vector1[loop]);
    // }
    // printf("\n");
    // printf("vector2 = ");
    // for(loop = 0; loop < count; loop++) {
    //     printf("%d ", vector2[loop]);
    // }
    // printf("\n");

    for(k = 0 ; k < count ; k++) {
        diff = bitwise_compare(vector1[k], vector2[k], order);
        dist = dist + diff;
    }

    if(count == 0) {
        return NA_REAL;
    } else {
        if(translate == 0) {
            // Return the hamming distance differences/counts
            result = (double)dist/count;
        } else {
            // Return the corrected distance (differences / (counts - 1))
            result = (double)dist/(count-1);
        }
        return result;
    }
}

// Calculating the hamming character distance
static double R_raw(int *x, int nr, int nc, int i1, int i2, int translate, int order)
{

    // Declaring variables (result is int)    
    int vector1[nc], vector2[nc];
    int count = 0, i = 0, k = 0, diff = 0, dist = 0;
    double result = 0;

    //Isolating the two comparable characters
    for(i = 0 ; i < nc ; i++) {
        //if(both_non_NA(x[i1], x[i2])) {
        // if(!NA_INTEGER(x[i1]) && !NA_INTEGER(x[i2])) {
        if(x[i1] != NA_INTEGER && x[i2] != NA_INTEGER) {
            
            // Create the vectors
            vector1[count] = x[i1];
            vector2[count] = x[i2];

            //Increment the counter
            count++;
        }
        i1 += nr;
        i2 += nr;
    }

    for(k = 0 ; k < count ; k++) {
        diff = bitwise_compare(vector1[k], vector2[k], order);
        dist = dist + diff;
    }

    if(count == 0) {
        return NA_REAL;
    } else {
        result = (double)dist;
        return result;
    }
}

// Calculating the hamming character distance
// static double R_raw(int *x, int nr, int nc, int i1, int i2, int translate, int order)
// {

//     // Declaring variables (result is int)    
//     int vector1[nc], vector2[nc];
//     int count = 0, i = 0, k = 0;
//     double result = 0;

//     //Isolating the two comparable characters
//     for(i = 0 ; i < nc ; i++) {
//         //if(both_non_NA(x[i1], x[i2])) {
//         // if(!NA_INTEGER(x[i1]) && !NA_INTEGER(x[i2])) {
//         if(x[i1] != NA_INTEGER && x[i2] != NA_INTEGER) {
            
//             // Create the vectors
//             vector1[count] = x[i1];
//             vector2[count] = x[i2];

//             //Increment the counter
//             count++;
//         }
//         i1 += nr;
//         i2 += nr;
//     }

//     if(count == 0) {
//         return NA_REAL;
//     } else {
//         result = (double)count;
//         return result;
//     }
// }


// R_distance_bitwise function (R::dist())
void R_distance_bitwise(int *x, int *nr, int *nc, double *d, int *diag, int *method, int *translate, int *order)
{
    int dc, i, j;
    size_t ij;  /* can exceed 2^31 - 1 */
    // double (*distfun)(int*, int, int, int, int) = NULL;
    double (*distfun)(int*, int, int, int, int, int, int) = NULL;
    // distfun(matrix, nrows, ncolumns, i1, i2, translate, order)

    //Open MPI
#ifdef _OPENMP
    int nthreads;
#endif
    // distfun = R_hamming;

    switch(*method) {
        case 1:
            distfun = R_hamming;
        break;
        case 2:
            distfun = R_raw;
        break;
        // case 3:
        //     distfun = R_comparable;
        // break;
        // default:
        //     error(_("distance(): invalid distance"));
    }

    dc = (*diag) ? 0 : 1; /* diag=1:  we do the diagonal */

#ifdef _OPENMP
    if (R_num_math_threads > 0)
    nthreads = R_num_math_threads;
    else
    nthreads = 1; /* for now */
    if (nthreads == 1) {
    /* do the nthreads == 1 case without any OMP overhead to see
       if it matters on some platforms */
    ij = 0;
    for(j = 0 ; j <= *nr ; j++)
        for(i = j+dc ; i < *nr ; i++)
        d[ij++] = distfun(x, *nr, *nc, i, j, *translate, *order);
    }
    else
    /* This produces uneven thread workloads since the outer loop
       is over the subdiagonal portions of columns.  An
       alternative would be to use a loop on ij and to compute the
       i and j values from ij. */
#pragma omp parallel for num_threads(nthreads) default(none)    \
    private(i, j, ij)                       \
    firstprivate(nr, dc, d, method, distfun, nc, x)
    for(j = 0 ; j <= *nr ; j++) {
        ij = j * (*nr - dc) + j - ((1 + j) * j) / 2;
        for(i = j+dc ; i < *nr ; i++)
        d[ij++] = distfun(x, *nr, *nc, i, j, *translate, *order);
    }
#else
    ij = 0;
    for(j = 0 ; j <= *nr ; j++)
    for(i = j+dc ; i < *nr ; i++)
        d[ij++] = distfun(x, *nr, *nc, i, j, *translate, *order);
#endif
}


// R/C interface (former Diff)
SEXP C_bitwisedist(SEXP x, SEXP smethod, SEXP stranslate, SEXP sorder, SEXP attrs) //, SEXP sorder
{
    // Define the variable
    SEXP result;
    int nr = nrows(x), nc = ncols(x), method = asInteger(smethod), translate = asInteger(stranslate), order = asInteger(sorder);
    int diag = 0;
    R_xlen_t N;
    N = (R_xlen_t)nr * (nr-1)/2; /* avoid int overflow for N ~ 50,000 */
    PROTECT(result = allocVector(REALSXP, N));
    if(TYPEOF(x) != INTSXP) x = coerceVector(x, INTSXP);
    PROTECT(x);
    
    // Calculate the distance matrix
    R_distance_bitwise(INTEGER(x), &nr, &nc, REAL(result), &diag, &method, &translate, &order);
    
    // Wrap up the results
    SEXP names = PROTECT(getAttrib(attrs, R_NamesSymbol)); // Row/column names attributes
    for (int i = 0; i < LENGTH(attrs); i++) {
        setAttrib(result, install(translateChar(STRING_ELT(names, i))), VECTOR_ELT(attrs, i));
    }

    UNPROTECT(3);

    return result;
}
