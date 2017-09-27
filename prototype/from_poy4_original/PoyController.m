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

#import "PoyController.h"

void
cocoa_CAML_run_command (NSString *);

@implementation PoyController

- (PoyStatus *)choose:(enum box) item
{
    switch (item) {
        case OUTPUT:
            return output;
        case STATUS:
            return status;
    }
    return output;
}

- (void) appendCString:(const char *) string where:(enum box) where 
{
    PoyStatus *myWhere = [self choose:where];
    [myWhere appendCString:string];
    return;
}

- (void) appendString:(NSString *) string where:(enum box) where 
{
    PoyStatus *myWhere = [self choose:where];
    [myWhere appendString:string];
    return;
}

- (void) displayCString:(const char *) string where:(enum box) where 
{
    PoyStatus *myWhere = [self choose:where];
    [myWhere displayCString:string];
    return;
}

- (void) displayString:(NSString *) string where:(enum box) where 
{
    PoyStatus *myWhere = [self choose:where];
    [myWhere displayString:string];
    return;
}

- (IBAction) displayOutput:(id) caller 
{
    cocoa_CAML_run_command ([[command textStorage] string]); 
}

@end
