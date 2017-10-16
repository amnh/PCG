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

#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#endif

value
sleep_CAML_nanosleep (value vsec, value vnan) {
    CAMLparam2(vsec, vnan);
#ifdef _WIN32
	/* Windows sleep command only accepts milliseconds so convert accordingly *
	 * of course we loose some accuracy but atleast poy was using usually     *
	 * about 25ms sleep times.												  */
	Sleep((DWORD)(Int_val(vsec)*1000+((float)Double_val(vnan))/1000000));
#else
    struct timespec sleep;
    sleep.tv_sec = Int_val (vsec);
    sleep.tv_nsec = (float) Double_val (vnan);
    nanosleep (&sleep, NULL);
#endif
    CAMLreturn(Val_unit);
}
