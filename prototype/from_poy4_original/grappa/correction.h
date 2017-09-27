#include "math.h"

/* EDE and IEBP distance corrections, 2/26/2001 Li-San Wang */

#define ACC 	1e-6            /* Accuracy for distance correction */
#define XMAX	4               /* Maximum relative number of events to correct */

/*********************************
	EDE 
*/

#define ede_a	0.595639
#define ede_c	0.457748

int ede ( int invdist, int ngene );

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

int iebp ( int bpdist, int ngene, double twt, double vwt,
           int signedmode, int circularmode );
/* The IEBP approximation: F_k 
   Do not call it directly.  Call iebp instead.
*/

double iebpcurve ( int bpdist, int ngene, double twt, double vwt,
                   int signmode, int circularmode );
