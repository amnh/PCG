#ifndef _CPP_A_
#define _CPP_A_

#include "A.h"

#ifdef __cplusplus
extern "C"
{
typedef A *APtr;
#else
typedef void *APtr;
#endif

APtr tstMakeA(int a);
void tstFreeA(APtr aptr);
void tstRunA(APtr aptr, int b);

#ifdef __cplusplus
}
#endif

#endif
