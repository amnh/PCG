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

#ifndef SANKOFF_H

#define SANKOFF_H 1

#include "cm.h"

/*
 * Performs a downpass operation in a single character.
 * @param ch1 An array of a_sz states of the character in the first child of p.
 * @param ch2 Same as ch1 for the second child of p.
 * @param p An array of a_sz states of the character in the node to be
 * calculated (the parent of ch1 and ch2).
 * @param tcm a pointer to a three dimensional cost matrix as produced by the cm
 * library.
 * @param a_sz is the total number of possible states of the character. 
 */
inline void
sankoff_sc_down_pre (const int *ch1, const int *ch2, int *p, const int *tcm, \
        int a_sz); 

/*
 * Performs a full downpass on a set of Sankoff characters. 
 * @param ch1 is the character set of the first child of p.
 * @param ch2 is the character set of the second child of p.
 * @param p is the set of characters to be calculated using the information of
 * ch1 and ch2. p represents the set of characters of the parent of ch1 and ch2.
 * @param tcm is the transformation cost matrix for the characters.
 * @param a_sz is the number of possible states of the character.
 * @param n is the total number of characters in the set.
 */
inline void
sankoff_down_pre (struct storage ch1, struct storage ch2, struct storage p, \
        int *tcm, int a_sz, int n); 

#endif /* SANKOFF_H */
