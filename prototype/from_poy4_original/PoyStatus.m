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

#import "PoyStatus.h"

@implementation PoyStatus

-(void) displayString:(NSString *)string 
{
    NSRange curRange;
    curRange.length = [[view textStorage] length];
    curRange.location = 0;
    [view replaceCharactersInRange:curRange withString:string];
    return;
}

-(void) displayCString:(const char *)string
{
    [self displayString:[NSString stringWithCString:string 
               encoding:NSASCIIStringEncoding]];
    return;
}

-(void) appendString:(NSString *)string 
{
    NSRange curRange;
    curRange.length = 0;
    curRange.location = [[view textStorage] length];
    [view replaceCharactersInRange:curRange withString:string];
    return;
}

-(void) appendCString:(const char *)string
{
    [self appendString:[NSString stringWithCString:string 
               encoding:NSASCIIStringEncoding]];
    return;
}


@end
