#include <R.h>
#include <Rinternals.h>

/* viper:
define access(a) forall j: Int :: 0 <= j && j < len(a) ==> acc(loc(a, j).val)
*/

SEXP add(SEXP a, SEXP b)
// foo bar
{
  SEXP output = PROTECT(allocVector(REALSXP, 1));
  //REAL(output)[0] = asReal(a) + asReal(b);
  //UNPROTECT(1);

  //return output;
}