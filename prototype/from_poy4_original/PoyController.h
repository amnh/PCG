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

/* PoyController */

#import <Cocoa/Cocoa.h>
#include "PoyStatus.h"

enum box { OUTPUT, STATUS };

@interface PoyController : NSObject
{
    IBOutlet id command;
    IBOutlet PoyStatus *output;
    IBOutlet id searchstatus;
    IBOutlet PoyStatus *status;
	NSRange lastRange;
}

-(PoyStatus *) choose:(enum box) item;
-(void) appendCString:(const char *) string  where:(enum box) where;
-(void) appendString:(NSString *) string  where:(enum box) where;
-(void) displayCString:(const char *) string  where:(enum box) where;
-(void) displayString:(NSString *) string  where:(enum box) where;
-(IBAction) displayOutput:(id) caller;

@end
