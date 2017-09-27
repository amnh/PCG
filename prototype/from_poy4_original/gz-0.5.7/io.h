/* Taken from byterun/io.h in the OCAML v3.00 source code (©1996 INRIA) */

#include "caml/mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 4096
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  long offset;                  /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

