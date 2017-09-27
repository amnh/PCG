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

#import <Cocoa/Cocoa.h>
#import "PoyController.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/custom.h"

value
cocoa_CAML_output (value str) {
    CAMLparam1(str);
    [[NSApp delegate] appendCString:(String_val(str)) where:OUTPUT];
    CAMLreturn(Val_unit);
}

cocoa_CAML_status (value str) {
    CAMLparam1(str);
    [[NSApp delegate] displayCString:(String_val(str)) where:STATUS];
    CAMLreturn(Val_unit);
}

value
cocoa_CAML_main (value u) {
    CAMLparam1(u);
    const char *pname = "PoyInterface";
    NSApplicationMain(1,  &pname);
    CAMLreturn(Val_unit);
}

void
cocoa_CAML_run_command (NSString *str) {
    static value *closure_f = NULL;
    if (NULL == closure_f)
        closure_f = (value *) caml_named_value ("cocoa_interpreter");
    caml_callback (*closure_f,
            caml_copy_string([str cStringUsingEncoding:NSASCIIStringEncoding]));
    return;
}
