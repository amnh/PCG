/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/alloc.h"
#include "caml/callback.h"

#ifndef _WIN32
#include <sys/time.h>
#include <sys/resource.h>

value
CAML_getrusage (value v) {
    CAMLparam1(v);
    CAMLlocal1(res);
    struct rusage holder;
    float ru_utime, ru_stime;
    getrusage(Int_val(v), &holder);
    ru_utime =
        ((float) holder.ru_utime.tv_sec) +
        ((float) holder.ru_utime.tv_usec / ((float) 1000000));
    ru_stime = holder.ru_stime.tv_sec + (holder.ru_stime.tv_usec / 10000);
    res = caml_alloc_tuple(16);
    Store_field(res, 0, caml_copy_double(ru_utime));
    Store_field(res, 1, caml_copy_double(ru_stime));
    Store_field(res, 2, Val_long(holder.ru_maxrss));
    Store_field(res, 3, Val_long(holder.ru_ixrss));
    Store_field(res, 4, Val_long(holder.ru_idrss));
    Store_field(res, 5, Val_long(holder.ru_isrss));
    Store_field(res, 6, Val_long(holder.ru_minflt));
    Store_field(res, 7, Val_long(holder.ru_majflt));
    Store_field(res, 8, Val_long(holder.ru_nswap));
    Store_field(res, 9, Val_long(holder.ru_inblock));
    Store_field(res, 10, Val_long(holder.ru_oublock));
    Store_field(res, 11, Val_long(holder.ru_msgsnd));
    Store_field(res, 12, Val_long(holder.ru_msgrcv));
    Store_field(res, 13, Val_long(holder.ru_nsignals));
    Store_field(res, 14, Val_long(holder.ru_nvcsw));
    Store_field(res, 15, Val_long(holder.ru_nivcsw));
    CAMLreturn(res);
}
#endif /* _WIN32 */
