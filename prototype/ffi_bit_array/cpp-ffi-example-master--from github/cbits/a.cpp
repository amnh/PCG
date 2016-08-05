#include "a.h"

APtr tstMakeA(int a)
{
  return new A(a);
}

void tstFreeA(APtr aptr)
{
  delete aptr;
}

void tstRunA(APtr aptr, int b)
{
  aptr->run(b);
}
