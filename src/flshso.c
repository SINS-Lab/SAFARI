#include <stdio.h>

/*  A procedure to let FORTRAN call fflush, which accounts for the
    foreshortened name: FLuSHStdOut.  */
flshso()
{
fflush(stdout);
}
