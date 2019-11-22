
#include "dispRity.h"

// #################
// Character difference logic
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
void Normalise_single_character(double *vector, int count)
{
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


// This should contain the generic distance method as follow:
// But I can't get the switch function to work!

// Allowed methods
// enum { HAMMING=1};
//* == 1,2,..., defined by order in the R function dist */

// R_distance function (R::dist())
// void R_distance(double *x, int *nr, int *nc, double *d, int *diag, int *method)
// {
//     int dc, i, j;
//     size_t  ij;  /* can exceed 2^31 - 1 */
//     double (*distfun)(double*, int, int, int, int) = NULL;

//     //Open MPI
// #ifdef _OPENMP
//     int nthreads;
// #endif

//     switch(*method) {
//     case HAMMING:
//         distfun = R_Hamming;
//         break;
//     case MAXIMUM:
//         distfun = R_maximum;
//         break;
//     case MANHATTAN:
//         distfun = R_manhattan;
//         break;
//     case CANBERRA:
//         distfun = R_canberra;
//         break;
//     case BINARY:
//         distfun = R_dist_binary;
//         break;
//     case MINKOWSKI:
//         if(!R_FINITE(*p) || *p <= 0)
//             error(_("distance(): invalid p"));
//         break;
//     default:
//         error(_("distance(): invalid distance"));
//     }

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



// // R/C interface (former Diff)
// SEXP C_diff_hamming(SEXP x, SEXP smethod, SEXP attrs)
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
//     SEXP names = PROTECT(getAttrib(attrs, R_NamesSymbol)); // Row/column names attributes
//     for (int i = 0; i < LENGTH(attrs); i++) {
//         setAttrib(result, install(translateChar(STRING_ELT(names, i))), VECTOR_ELT(attrs, i));
//     }

//     UNPROTECT(3);

//     return result;
// }
