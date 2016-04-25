#include <R.h>
#include <Rinternals.h>

SEXP add(SEXP a, SEXP b)
{
  SEXP output = PROTECT(allocVector(REALSXP, 1));
  REAL(output)[0] = asReal(a) + asReal(b);
  UNPROTECT(1);
  return output;
}