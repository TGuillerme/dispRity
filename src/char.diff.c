/* 
 * char.diff function
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

// #################
// Character difference (Gower) logic
// #################

// Convert character into number
double character_to_numeric(char c)
{
    double n = -1;
    static const char * const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    char *p = strchr(alphabet, (unsigned char)c);

    if (p) {
        n = p - alphabet;
    }

    return n;
}

// Normalise a single numeric character
void Normalise_single_character(double *vector, int count) {
    int element, i, j, k;
    char vector_char[count], element_char;
    char alphabet[] = { 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','\0'};

    // Getting each unique element of the vector and translating them into standardised characters
    element = 0;

    for (i = 0; i < count; ++i) {
        for (j = 0; j < i; ++j) {
            if (vector[i] == vector[j])
               break;
        }

        if (i == j) {
            // If encountering unique characters, attribute the first letter of the alphabet and so on
            element_char = alphabet[element];
            for(k = 0; k < count; k++) {
                if(vector[k] == vector[i]) {
                    vector_char[k] = element_char;
                }
            }
            element ++;
        }
    }

    // Transforming the character back to numeric (double)
    for(k = 0; k < count ; k++) {
        vector[k] = character_to_numeric(vector_char[k]);
    }

}


// Calculating the Gower character distance
static double R_Gower(double *x, int nr, int nc, int i1, int i2)
{
    double diff, dist, vector1[nc], vector2[nc];
    int count, i, k;

    //Initialise the values
    count= 0;
    dist = 0;

    //Isolating the two comparable characters
    for(i = 0 ; i < nc ; i++) {
        if(both_non_NA(x[i1], x[i2])) {
            
            // Create the vectors
            vector1[count] = x[i1];
            vector2[count] = x[i2];

            //Increment the counter
            count++;
        }
        i1 += nr;
        i2 += nr;
    }

    // Normalising the characters
    Normalise_single_character(vector1, count);
    Normalise_single_character(vector2, count);

    for(k = 0 ; k < count ; k++) {
         diff = fabs(vector1[k] - vector2[k]);
        // Normalise the difference (Fitch like)
        if(diff > 1) {
            diff = 1;   
        }
        // Increment the differences
        if(!ISNAN(diff)) {
            dist += diff;
        }        
    }

    if(count == 0) {
        return NA_REAL;
    } else {
        dist = dist/count;
        return dist;
    }
}

// Allowed methods
enum { GOWER=1};
/* == 1,2,..., defined by order in the R function dist */

// R_distance function (R::dist())
void R_distance(double *x, int *nr, int *nc, double *d, int *diag, int *method)
{
    int dc, i, j;
    size_t  ij;  /* can exceed 2^31 - 1 */
    double (*distfun)(double*, int, int, int, int) = NULL;

    //Open MPI
#ifdef _OPENMP
    int nthreads;
#endif
    distfun = R_Gower;

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
        d[ij++] = distfun(x, *nr, *nc, i, j);
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
        d[ij++] = distfun(x, *nr, *nc, i, j);
    }
#else
    ij = 0;
    for(j = 0 ; j <= *nr ; j++)
    for(i = j+dc ; i < *nr ; i++)
        d[ij++] = distfun(x, *nr, *nc, i, j);
#endif
}