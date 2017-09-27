#include "correction.h"


/* call ede; 
   invdist 	the minimum inversion distance,
   ngene 	the number of genes. 
   The function returns the EDE distance */

int
ede ( int invdist, int ngene )
{
    double ll, tt, kk, pp, dval;
    int newvalue;

    kk = invdist / ( ngene + 0.0 );

    if ( kk >= 0.999999999999 )
    {                           /* the distance correction has singularity at 1 */
        kk = 0.999999999999;
    }
    if ( kk <= 1 - ede_c )
        return invdist;

    ll = ede_c * kk - ede_a;
    tt = 4 * ede_a * ( 1 - kk ) * kk + ll * ll;
    tt = ll + sqrt ( tt );
    pp = tt / ( 2 * ( 1 - kk ) );
    pp *= ngene;

    dval = pp;
    newvalue = ( int ) ceil ( dval );
    /*if (newvalue-dval > 0) return newvalue-1; */
    return newvalue;
}

/*********************************
	IEBP
*/

/* Call iebp:
   Bisection method to find the root
   
   bpdist 	the breakpoint distance
   ngene 	the number of genes
   iwt      not seen used, get rid, may need, who knows.
   twt		Pr(an event is a transposition)
   		(alpha in the IEBP paper)
   vwt		Pr(an event is an inverted transposition)
   		(beta in the IEBP paper)
   signedmode	=1 if signed, 0 if unsigned 
   circularmode	=1 if circular, 0 if linear
      
   Returns the IEBP distance correction.
*/

int
iebp ( int bpdist, int ngene, double twt, double vwt,
       int signedmode, int circularmode )
{
    int xl, xr, xm, yl, yr, ym;
    /* what's for xm0 */

    xl = 0;
    yl = ( int ) iebpcurve ( xl, ngene, twt, vwt, signedmode, circularmode );
    xr = XMAX * ngene;
    yr = ( int ) iebpcurve ( xr, ngene, twt, vwt, signedmode, circularmode );
    xm = ( xl + xr ) / 2;
    ym = ( int ) iebpcurve ( xm, ngene, twt, vwt, signedmode, circularmode );

    if ( bpdist == yl )
    {
        return xl;
    }
    if ( bpdist >= yr )
    {
        return xr;
    }

    while ( ( xr - xl ) >= 1 )
    {
        if ( ( ym - xm ) * ( yl - xm ) < 0 )
        {                       /*change dval to xm */
            xr = xm;
            yr = ym;
        }
        else
        {
            xl = xm;
            yl = ym;
        }
        xm = ( xl + xr ) / 2;
        ym = ( int ) iebpcurve ( xm, ngene, twt, vwt, signedmode,
                                 circularmode );
    }

    return ( int ) xm;
}

/* The IEBP approximation: F_k 
   Do not call it directly.  Call iebp instead.
*/

double
iebpcurve ( int bpdist, int ngene, double twt, double vwt,
            int signmode, int circularmode )
{
    double n, k, z, z0 = 0, ul, uh, ul0 = 0, uh0 = 0, el, eh, el0, eh0, f;

    k = bpdist;
    n = ngene;

    if ( circularmode == 1 )
    {
        if ( signmode == 1 )
        {
            z = ( 2 + twt + vwt ) / n;
            ul = 0;

            uh = ( 2 * ( n - 2 ) + 4 * twt * ( n - 2 ) + 2 * vwt * ( n - 2 ) )
                / ( n * ( n - 1 ) * ( n - 2 ) );
        }
        else
        {
            z = ( 2 * ( n - 3 ) + twt * ( n - 3 ) + vwt * n )
                / ( ( n - 1 ) * ( n - 2 ) );
            ul = ( 4 * ( n - 3 ) + 2 * twt * ( n - 3 ) + 2 * vwt * n )
                / ( ( n - 1 ) * ( n - 2 ) * ( n - 3 ) );
            uh = ( 4 * ( n - 3 ) + 2 * twt * ( n - 3 ) + 2 * vwt * n )
                / ( ( n - 1 ) * ( n - 2 ) * ( n - 3 ) );
        }
    }
    else
    {
        if ( signmode == 1 )
        {
            z = ( 2 + twt + vwt ) / ( n + 1 );
            ul = 0;

            uh = ( 2 * ( n - 1 ) + 4 * twt * ( n - 1 ) + vwt * ( n + 2 ) )
                / ( ( n + 1 ) * n * ( n - 1 ) );

            z0 = ( 2 + twt + vwt ) / ( n + 1 );
            ul0 = 0;

            uh0 = ( 2 + 4 * twt + vwt ) / ( ( n + 1 ) * n );
        }
        else
        {
            z = ( 2 * ( n - 2 ) + twt * ( n - 2 ) + vwt * ( n - 5 ) )
                / ( n * ( n - 1 ) );
            ul = ( 2 * ( 2 * n - 4 ) + 2 * twt * ( n - 2 ) +
                   2 * vwt * ( n + 1 ) ) / ( n * ( n - 1 ) * ( n - 2 ) );
            uh = ( 2 * ( 2 * n - 4 ) + 2 * twt * ( n - 2 ) +
                   2 * vwt * ( n + 1 ) ) / ( n * ( n - 1 ) * ( n - 2 ) );
            z0 = ( 2 * ( n + 1 ) + twt * ( n - 2 ) +
                   vwt * ( n + 1 ) ) / ( n * ( n + 1 ) );
            ul0 =
                ( 2 * ( n + 1 ) * ( n - 2 ) -
                  2 * twt * ( n - 2 ) * ( n - 2 ) - vwt * ( n +
                                                            1 ) * ( 2 * n -
                                                                    7 ) ) /
                ( ( n + 1 ) * n * ( n - 1 ) * ( n - 2 ) );
            /* The RS&A draft (as of 2/26/01) has error: 
               in u0 min (ul0), the sign before beta should be minus.
             */
            uh0 =
                ( 2 * ( n + 1 ) + 4 * twt * ( n - 2 ) +
                  4 * vwt * ( n + 1 ) ) / ( ( n + 1 ) * n * ( n - 1 ) );
        }
    }

    el = z * ( 1 - pow ( 1 - z - ul, k ) ) / ( z + ul );
    eh = z * ( 1 - pow ( 1 - z - uh, k ) ) / ( z + uh );

    if ( circularmode == 1 )
    {
        f = n * ( el + eh ) / 2;
    }
    else
    {
        el0 = z0 * ( 1 - pow ( 1 - z0 - ul0, k ) ) / ( z0 + ul0 );
        eh0 = z0 * ( 1 - pow ( 1 - z0 - uh0, k ) ) / ( z0 + uh0 );
        f = ( n - 1 ) * ( el + eh ) / 2 + 2 * ( el0 + eh0 ) / 2;
    }

    return f;
}
