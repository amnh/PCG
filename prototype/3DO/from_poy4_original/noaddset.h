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

#ifndef NOADDSET_H

#define NOADDSET_H 1

struct storage {
    int num_chrs;
    unsigned int *preliminary;
    unsigned int *final;
    unsigned int *my_cost;
    unsigned int *my_weight;
    unsigned int *total_cost;
    unsigned int *codes;
};

#ifdef _WIN32
__inline int
#else
inline int
#endif
noaddset_downpass (struct storage ch1, struct storage ch2, struct storage p); 

/*
 * Create a structure using the ocaml passed information. 
 */
#ifdef _WIN32
__inline struct storage 
#else
inline struct storage 
#endif
noaddset_create_structure (value mtx, value number); 

#ifdef _WIN32
__inline void
#else
inline void
#endif
noaddset_uppass (struct storage ch1, struct storage ch2, struct storage gp, \
        struct storage p);

#ifdef _WIN32
__inline void
#else
inline void
#endif
noaddset_set_preliminary_as_final (struct storage ch1); 

#endif /* NOADDSET_H */
