#include <stdio.h>
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/alloc.h"
#include "config.h"
#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>

static char *line_read = (char *) NULL;

char * rl_gets (void) {
    if (line_read) {
        free (line_read);
        line_read = (char *) NULL;
    }

    line_read = readline ("poy> ");

    if (line_read && *line_read)
        add_history (line_read);
    return (line_read);
}

value
rl_CAML_gets (value u) {
    CAMLparam1 (u);
    char *res;
    res = rl_gets ();
    CAMLreturn (caml_copy_string(res));
}
#endif
