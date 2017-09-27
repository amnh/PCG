/* $Id: all_sorting_reversals.c 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Summer 2001
   Copyright 2001, Adam Siepel */

/* Code to enumerate all sorting reversals of one signed permutation
   with respect to another.  Algorithm is derived in Siepel, A., "An
   Algorithm to Find All Sorting Reversals," RECOMB 2002, and in
   Siepel, A., "Exact Algorithms for the Reversal Median Problem,"
   Master's Thesis, University of New Mexico, 2001. */

#include "all_sorting_reversals.h"
#include <stdlib.h>
#include "simpleio.h"
#include "assert.h"
#include "lists.h"

/* Pass genome_structs for permutation "pi" and target "origin", with
   accompanying "ngenes".  Calling code is expected to allocate and
   initialize the lists l and m, in which this function will record
   sorting and neutral reversals, respectively (items in lists will be
   of type "Reversal").  If NULL is passed for "m", then neutral
   reversals will not be enumerated.  Calling code may either pass a
   "ReversalSortingMemory" struct, or pass NULL, in which case a new
   one will be created. */
void
find_all_sorting_reversals ( List * l, List * m, struct genome_struct *pi,
                             struct genome_struct *origin, int ngenes,
                             ReversalSortingMemory * rsm )
{

    int nhurdles, nsuperhurdles, nfortresses, nuoc;
    int i, n, j, c, k, h1, h2, ncomp, npseudohurdles, neutral_mode;
    List uoc_list;
    Reversal *rev;
    ReversalSortingMemory *mem;

    if ( rsm == NULL )
        mem = new_reversal_sorting_memory ( ngenes );
    else
        mem = rsm;

    if ( m != NULL )
        neutral_mode = 1;
    else
        neutral_mode = 0;

    /* convert to frame of reference in which origin = identity;
       simultaneously expand to equiv unsigned perm */
    for ( i = 0; i < ngenes; i++ )
    {
        if ( origin->genes[i] < 0 )
            mem->inv[abs ( origin->genes[i] )] = -1 * ( i + 1 );
        else
            mem->inv[origin->genes[i]] = i + 1;
    }

    n = 2 * ngenes + 2;
    mem->upi[0] = 0;
    mem->upi[n - 1] = n - 1;
    for ( i = 0; i < ngenes; i++ )
    {
        int tmp;
        if ( pi->genes[i] > 0 )
            tmp = mem->inv[pi->genes[i]];
        else
            tmp = -1 * mem->inv[abs ( pi->genes[i] )];
        if ( tmp > 0 )
        {
            mem->upi[2 * i + 1] = 2 * tmp - 1;
            mem->upi[2 * i + 2] = 2 * tmp;
        }
        else
        {
            mem->upi[2 * i + 1] = 2 * abs ( tmp );
            mem->upi[2 * i + 2] = 2 * abs ( tmp ) - 1;
        }
    }

    /* find cycles and relative orientations */

    for ( i = 0; i < n; i++ )
    {
        if ( i % 2 == 0 )
            mem->be[i / 2] = mem->cycle[i / 2] = -1;
        mem->inv2[mem->upi[i]] = i;
    }

    c = 0;
    for ( i = 0; i < n; i += 2 )
    {
        if ( mem->be[i / 2] != -1 )
            continue;
        j = i;
        while ( mem->be[j / 2] == -1 )
        {
            if ( j % 2 != 0 )
            {                   /* (odd index) */
                j--;            /* follow black edge */
                mem->be[j / 2] = 1; /* record orientation */
                mem->cycle[j / 2] = c;  /* mark with cycle number */
            }
            else
            {                   /* (even index) */
                mem->be[j / 2] = 0; /* record orientation */
                mem->cycle[j / 2] = c;  /* mark with cycle number */
                j++;            /* follow black edge */
            }
            if ( mem->upi[j] % 2 == 0 ) /* follow gray edge */
                j = mem->inv2[mem->upi[j] + 1];
            else
                j = mem->inv2[mem->upi[j] - 1];
        }
        c++;
    }


    /* create a struct representing each connected component */
    /* first get labels describing membership of gray edges in connected
       components */
    find_connected_components ( 0, n, mem->inv2, mem->upi,
                                mem->comp_label_by_pos, NULL, -1, mem );

    /* get the component to which each cycle belongs */
    ncomp = 0;
    for ( i = 0; i <= ngenes; i++ )
    {
        mem->comp_label_by_cycle[mem->cycle[i]] =
            mem->comp_label_by_pos[i * 2];
        /* note black edge i corresponds to
           position 2*i, given our
           conventions here */
        if ( mem->comp_label_by_cycle[mem->cycle[i]] >= ncomp )
            ncomp = mem->comp_label_by_cycle[mem->cycle[i]] + 1;
    }

    /* create lists of edges of each orientation for each cycle */
    for ( i = 0; i <= c; i++ )
    {
        clear_list ( &mem->cyclelist[i] );
        clear_list ( &mem->cyclelist[c + 1 + i] );
    }
    for ( i = 0; i <= ngenes; i++ )
    {
        push ( &mem->cyclelist[mem->be[i] * ( c + 1 ) + mem->cycle[i]],
               ( void * ) i );
    }

    /* initialize connected component structs */
    for ( i = 0; i < ncomp; i++ )
    {
        clear_list ( &mem->conn_comp[i].cyclelist );
        clear_list ( &mem->conn_comp[i].double_superhurdle_partners );
        mem->conn_comp[i].type = SIMPLEHURDLE;  /* start with this assumption;
                                                   we will correct as we
                                                   proceed */
        mem->conn_comp[i].protecting_superhurdle =
            mem->conn_comp[i].protected_pseudohurdle = -1;
        mem->conn_comp[i].chain = mem->conn_comp[i].anchor = i;
        mem->conn_comp[i].mult_protected = 0;
    }

    /* add each cycle to the list of the component to which it
       belongs; simultaneously update orientations of components */
    for ( i = 0; i < c; i++ )
    {
        push ( &mem->conn_comp[mem->comp_label_by_cycle[i]].cyclelist,
               ( void * ) i );
        if ( mem->conn_comp[mem->comp_label_by_cycle[i]].type != ORIENTED
             && list_size ( &mem->cyclelist[i] ) > 0
             && list_size ( &mem->cyclelist[c + 1 + i] ) > 0 )
            mem->conn_comp[mem->comp_label_by_cycle[i]].type = ORIENTED;
    }

    /* identify trivial components */
    for ( i = 0; i < ncomp; i++ )
    {
        if ( list_size ( &mem->conn_comp[i].cyclelist ) == 1 )
        {
            j = ( int ) list_get ( &mem->conn_comp[i].cyclelist, 0 );
            if ( list_size ( &mem->cyclelist[j] ) +
                 list_size ( &mem->cyclelist[c + 1 + j] ) == 1 )
                mem->conn_comp[i].type = TRIVIAL;
        }
    }

    /* build overlap matrices for oriented components; simultaneously
       count unoriented components */
    for ( i = 0, nuoc = 0; i < ncomp; i++ )
    {
        if ( mem->conn_comp[i].type == ORIENTED )
        {
#ifdef BITWISE_DETECT
            build_overlap_matrix ( &mem->conn_comp[i], mem->upi, mem->inv2,
                                   n, c, mem->cyclelist );
#endif
        }
        else if ( mem->conn_comp[i].type != TRIVIAL )
            nuoc++;
    }

    nsuperhurdles = 0;
    nhurdles = nuoc;
    if ( nuoc >= 3 )
    {                           /* if two or less, all must be simple
                                   hurdles */

        /* first find sequence in which unoriented components appear; 
           reduce repeated appearances to single instances */
        init_list ( &uoc_list, n, sizeof ( int ) );
        build_uoc_list ( &uoc_list, mem->conn_comp, mem->comp_label_by_pos,
                         n );

        /* now find and label superhurdles and double superhurdles */
        find_superhurdles ( &uoc_list, mem->conn_comp, nuoc, ncomp,
                            &nhurdles, &nsuperhurdles );

        free_list ( &uoc_list );
    }

    if ( nsuperhurdles == nhurdles && nhurdles % 2 == 1 )
        nfortresses = 1;
    else
        nfortresses = 0;

    /* I have not added code to enumerate neutral reversals when there
       exists a fortresses.  This omission is of little practical
       importance because fortresses almost never are encountered except
       in contrived test cases; however, if one should appear, we need
       to make it abundantly clear to the user, because in such a case
       the current code would return an answer certain to be very wrong */
    if ( neutral_mode == 1 && nfortresses == 1 )
    {
        fprintf ( stderr,
                  "ERROR: Can't handle fortresses in neutral mode.\n" );
        assert ( 0 );
    }

#ifdef DEBUG
    printf ( "distance = %d (c = %d; h = %d; f = %d)\n", ngenes + 1 - c +
             nhurdles + nfortresses, c, nhurdles, nfortresses );

    for ( i = 0; i < ncomp; i++ )
    {
        int j;
        printf ( "Component %d is ", i );
        if ( mem->conn_comp[i].type == ORIENTED )
            printf ( "ORIENTED" );
        else if ( mem->conn_comp[i].type == TRIVIAL )
            printf ( "TRIVIAL" );
        else if ( mem->conn_comp[i].type == SIMPLEHURDLE )
            printf ( "a SIMPLEHURDLE" );
        else if ( mem->conn_comp[i].type == SUPHURDLE )
            printf ( "a SUPHURDLE" );
        else if ( mem->conn_comp[i].type == PROTNHURDLE )
            printf ( "a PROTNHURDLE" );
        else if ( mem->conn_comp[i].type == PSEUDOHURDLE )
            printf ( "a PSEUDOHURDLE" );
        if ( list_size ( &mem->conn_comp[i].double_superhurdle_partners ) >
             0 )
        {
            printf ( " (double superhurdle with %d",
                     ( int ) list_get ( &mem->conn_comp[i].
                                        double_superhurdle_partners, 0 ) );
            if ( list_size ( &mem->conn_comp[i].double_superhurdle_partners )
                 > 1 )
                printf ( " and %d",
                         ( int ) list_get ( &mem->conn_comp[i].
                                            double_superhurdle_partners,
                                            1 ) );
            printf ( ")" );
        }
        if ( mem->conn_comp[i].protecting_superhurdle != -1 )
            printf ( " (protecting superhurdle is %d)",
                     mem->conn_comp[i].protecting_superhurdle );

        printf ( ", spans %d to %d,", mem->conn_comp[i].begidx,
                 mem->conn_comp[i].endidx );
        printf ( " and contains the following cycles:\n" );
        for ( j = 0; j < list_size ( &mem->conn_comp[i].cyclelist ); j++ )
        {
            int x;
            x = ( int ) list_get ( &mem->conn_comp[i].cyclelist, j );
            printf ( "(%d: %d x 0, %d x 1)\n", x,
                     list_size ( &mem->cyclelist[x] ),
                     list_size ( &mem->cyclelist[c + 1 + x] ) );
        }
    }
#endif

    /* enumerate sorting reversals that split cycles */
    for ( i = 0; i <= c; i++ )
    {
        for ( j = 0; j < list_size ( &mem->cyclelist[i] ); j++ )
        {
            for ( k = 0; k < list_size ( &mem->cyclelist[c + 1 + i] ); k++ )
            {
                int a, b, nhf;
                rev = ( Reversal * ) malloc ( sizeof ( Reversal ) );
                a = ( int ) list_get ( &mem->cyclelist[i], j );
                b = ( int ) list_get ( &mem->cyclelist[c + 1 + i], k );
                rev->start = ( a <= b ? a : b );
                rev->stop = ( a <= b ? b : a );


#ifdef BITWISE_DETECT
                nhf =
                    new_nhurdles_plus_nfortresses ( mem->
                                                    comp_label_by_cycle[i],
                                                    rev, n, ncomp, nuoc,
                                                    nhurdles, nfortresses,
                                                    mem );
#else
                nhf =
                    new_nhurdles_plus_nfortresses_cc ( mem->
                                                       comp_label_by_cycle[i],
                                                       rev, n, ncomp, nuoc,
                                                       nhurdles, nfortresses,
                                                       mem );
#endif

                if ( nhf <= nhurdles + nfortresses )
                    push ( l, rev );
                else if ( neutral_mode && nhf == nhurdles + nfortresses + 1 )
                    push ( m, rev );
                else
                    free ( rev );
            }
        }
    }

    /* enumerate neutral reversals that act on convergent edges of the
       same cycle */
    if ( neutral_mode )
    {
        for ( i = 0; i <= c; i++ )
        {
            int i2;
            for ( i2 = 0; i2 < 2; i2++ )
            {
                for ( j = 0;
                      j < list_size ( &mem->cyclelist[i2 * ( c + 1 ) + i] );
                      j++ )
                {
                    for ( k = j + 1;
                          k <
                          list_size ( &mem->cyclelist[i2 * ( c + 1 ) + i] );
                          k++ )
                    {
                        int a, b, nhf;
                        rev = ( Reversal * ) malloc ( sizeof ( Reversal ) );
                        a = ( int ) list_get ( &mem->
                                               cyclelist[i2 * ( c + 1 ) + i],
                                               j );
                        b = ( int ) list_get ( &mem->
                                               cyclelist[i2 * ( c + 1 ) + i],
                                               k );
                        rev->start = ( a <= b ? a : b );
                        rev->stop = ( a <= b ? b : a );

                        if ( mem->conn_comp[mem->comp_label_by_cycle[i]].
                             type == ORIENTED )
                        {
#ifdef BITWISE_DETECT
                            nhf =
                                new_nhurdles_plus_nfortresses ( mem->
                                                                comp_label_by_cycle
                                                                [i], rev, n,
                                                                ncomp, nuoc,
                                                                nhurdles,
                                                                nfortresses,
                                                                mem );
#else
                            nhf =
                                new_nhurdles_plus_nfortresses_cc ( mem->
                                                                   comp_label_by_cycle
                                                                   [i], rev,
                                                                   n, ncomp,
                                                                   nuoc,
                                                                   nhurdles,
                                                                   nfortresses,
                                                                   mem );
#endif
                            if ( nhf == nhurdles + nfortresses )
                                push ( m, rev );
                            else
                                free ( rev );
                        }
                        else if ( mem->conn_comp[mem->comp_label_by_cycle[i]].
                                  type != SIMPLEHURDLE )
                            /* in the case of a trivial comp, we
                               can't get this far */
                            push ( m, rev );
                        else
                            free ( rev );
                    }
                }
            }
        }
    }

    /* enumerate sorting reversals that cut simple hurdles */
    npseudohurdles = 0;
    if ( nsuperhurdles < nhurdles || nfortresses == 1 )
    {
        /* only bother if there exists at
           least one simple hurdle; in the
           case of a fortress, however, we
           have to go on because the cutting
           rules are changed */

        if ( nfortresses == 1 || nsuperhurdles < 3 || nsuperhurdles % 2 == 0
             || nhurdles - nsuperhurdles > 1 )
        {
            /* avoid walking into a fortress! */
            for ( i = 0; i < ncomp; i++ )
            {
                if ( mem->conn_comp[i].type == SIMPLEHURDLE )
                    add_all_cutting_reversals ( l, &mem->conn_comp[i],
                                                mem->cyclelist );
                else if ( nfortresses == 1
                          && mem->conn_comp[i].type == PSEUDOHURDLE )
                {
                    npseudohurdles++;
                    add_all_cutting_reversals ( l, &mem->conn_comp[i],
                                                mem->cyclelist );
                    add_all_cutting_reversals ( l,
                                                &mem->conn_comp[mem->
                                                                conn_comp[i].
                                                                protecting_superhurdle],
                                                mem->cyclelist );
                }
            }
        }
    }

    /* enumerate neutral reversals that act on different cycles of
       simple hurdles */
    if ( neutral_mode && nsuperhurdles < nhurdles )
    {
        if ( nfortresses == 1 || nsuperhurdles < 3 || nsuperhurdles % 2 == 0
             || nhurdles - nsuperhurdles > 1 )
        {
            for ( i = 0; i < ncomp; i++ )
            {
                if ( mem->conn_comp[i].type == SIMPLEHURDLE )
                {
                    int j2, k2, a, b;
                    List *edges1, *edges2;
                    Reversal *rev;
                    for ( j = 0;
                          j < list_size ( &mem->conn_comp[i].cyclelist );
                          j++ )
                    {
                        for ( j2 = j + 1;
                              j2 < list_size ( &mem->conn_comp[i].cyclelist );
                              j2++ )
                        {
                            edges1 =
                                &mem->
                                cyclelist[( int )
                                          list_get ( &mem->conn_comp[i].
                                                     cyclelist, j )];
                            edges2 =
                                &mem->
                                cyclelist[( int )
                                          list_get ( &mem->conn_comp[i].
                                                     cyclelist, j2 )];
                            for ( k = 0; k < list_size ( edges1 ); k++ )
                            {
                                for ( k2 = 0; k2 < list_size ( edges2 );
                                      k2++ )
                                {
                                    a = ( int ) list_get ( edges1, k );
                                    b = ( int ) list_get ( edges2, k2 );
                                    rev =
                                        ( Reversal * )
                                        malloc ( sizeof ( Reversal ) );
                                    rev->start = ( a <= b ? a : b );
                                    rev->stop = ( a <= b ? b : a );
                                    push ( m, rev );
                                }
                            }
                        }
                    }
                }
            }
        }
    }


    /* build list of hurdles to save time in subsequent loops */
    clear_list ( mem->mhurdles );
    for ( i = 0; i < ncomp; i++ )
    {
        if ( mem->conn_comp[i].type == SIMPLEHURDLE ||
             mem->conn_comp[i].type == SUPHURDLE )
            push ( mem->mhurdles, ( void * ) i );
    }


    /* The following steps all relate to merging of separate components,
       in one form or another.  The handling here is not as clean as the
       corresponding theory (the code was developed before the theory
       was complete), but I believe a close reading will show the code
       and the theory to be equivalent. */

    /* enumerate sorting reversals that merge hurdles */
    if ( nhurdles >= 2 )
    {

        for ( i = 0; i < list_size ( mem->mhurdles ); i++ )
        {
            for ( j = i + 1; j < list_size ( mem->mhurdles ); j++ )
            {
                h1 = ( int ) list_get ( mem->mhurdles, i );
                h2 = ( int ) list_get ( mem->mhurdles, j );

                if ( nfortresses == 0 &&
                     eliminating_components_creates_fortress ( h1, h2,
                                                               mem->conn_comp,
                                                               nhurdles,
                                                               nsuperhurdles ) )
                    continue;

                if ( form_double_superhurdle ( h1, h2, mem->conn_comp ) &&
                     nfortresses == 0 )
                    continue;   /* avoid merging a double superhurdle */

                add_all_merging_reversals ( l, &mem->conn_comp[h1],
                                            &mem->conn_comp[h2],
                                            mem->cyclelist, c );

            }
        }
    }

    /* enumerate sorting reversals that merge oriented components (with
       other oriented components or with unoriented components) and
       simultaneously orient hurdles */
    if ( neutral_mode || nhurdles >= 2 )
    {
        int is_separator[ncomp], sepclass[ncomp], bc_hurdchain[ncomp],
            lidx[ncomp], llabel[nhurdles], is_separated[ncomp];
        int nseparators = 0, h;
        List *ocs = NULL;

        /* find all separating hurdles and the benign components that they
           separate */
        find_separating_hurdles ( is_separator, sepclass, bc_hurdchain,
                                  &nseparators, n, ncomp, mem, &ocs, lidx,
                                  llabel, is_separated, neutral_mode,
                                  nfortresses );

        if ( neutral_mode || nseparators > 0 || nfortresses > 0 )
        {

            /* merge each of the identified ocs with every hurdle EXCEPT the
               one that separates it */
            for ( i = 0; i < nseparators; i++ )
            {
                int separator, oc;
                separator = llabel[i];
                for ( k = 0; k < list_size ( &ocs[i] ); k++ )
                {
                    oc = ( int ) list_get ( &ocs[i], k );
                    for ( j = 0; j < list_size ( mem->mhurdles ); j++ )
                    {

                        h = ( int ) list_get ( mem->mhurdles, j );

                        if ( h == separator )
                            continue;

                        if ( nfortresses == 0 &&
                             eliminating_components_creates_fortress ( h,
                                                                       separator,
                                                                       mem->
                                                                       conn_comp,
                                                                       nhurdles,
                                                                       nsuperhurdles ) )
                            continue;

                        if ( form_double_superhurdle
                             ( h, separator, mem->conn_comp )
                             && nfortresses == 0 )
                            continue;   /* avoid eliminating a double superhurdle */

                        add_all_merging_reversals ( l, &mem->conn_comp[oc],
                                                    &mem->conn_comp[h],
                                                    mem->cyclelist, c );
                    }
                }
            }

            /* merge pairs of ocs that are separated by different hurdles */
            for ( i = 0; i < nseparators; i++ )
            {
                int h1 = llabel[i];
                for ( j = i + 1; j < nseparators; j++ )
                {
                    int k, k2;
                    int h2 = llabel[j];

                    if ( nfortresses == 0 &&
                         eliminating_components_creates_fortress ( h1, h2,
                                                                   mem->
                                                                   conn_comp,
                                                                   nhurdles,
                                                                   nsuperhurdles ) )
                        continue;

                    if ( form_double_superhurdle ( h1, h2, mem->conn_comp ) &&
                         nfortresses == 0 )
                        continue;   /* avoid eliminating a double superhurdle */

                    for ( k = 0; k < list_size ( &ocs[i] ); k++ )
                    {
                        int o1 = ( int ) list_get ( &ocs[i], k );
                        for ( k2 = 0; k2 < list_size ( &ocs[j] ); k2++ )
                        {
                            int o2 = ( int ) list_get ( &ocs[j], k2 );
                            add_all_merging_reversals ( l,
                                                        &mem->conn_comp[o1],
                                                        &mem->conn_comp[o2],
                                                        mem->cyclelist, c );
                        }
                    }
                }
            }

            /* in the cases of "neutral mode" and of a fortress, we have
               several additional classes of merges to consider */
            if ( neutral_mode || nfortresses == 1 )
            {

                /* we will need a list of benign components that have no
                   separating hurdle */
                List non_separated_ocs;
                init_list ( &non_separated_ocs, ncomp, sizeof ( int ) );
                for ( i = 0; i < ncomp; i++ )
                    if ( is_separated[i] == 0
                         && ( mem->conn_comp[i].type == ORIENTED
                              || mem->conn_comp[i].type == TRIVIAL ) )
                        push ( &non_separated_ocs, ( void * ) i );


                if ( nfortresses == 1 )
                {

                    /* we will construct two lists, to_merge1 and to_merge2,
                       then merge each component of one list with each component
                       of the other */
                    List to_merge1, to_merge2;
                    init_list ( &to_merge1, ncomp, sizeof ( int ) );
                    init_list ( &to_merge2, ncomp, sizeof ( int ) );
                    for ( i = 0; i < list_size ( mem->mhurdles ); i++ )
                    {
                        int h = ( int ) list_get ( mem->mhurdles, i );
                        push ( &to_merge1, ( void * ) h );
                        for ( j = 0;
                              lidx[h] != -1
                              && j < list_size ( &ocs[lidx[h]] ); j++ )
                        {
                            int oc = ( int ) list_get ( &ocs[lidx[h]], j );
                            push ( &to_merge1, ( void * ) oc );
                        }

                        /* anchor of chain */
                        if ( mem->conn_comp[h].anchor != -1 )
                        {
                            push ( &to_merge2,
                                   ( void * ) mem->conn_comp[h].anchor );
                        }

                        /* all protected nonhurdles not in chain */
                        for ( j = 0; j < ncomp; j++ )
                        {
                            if ( ( mem->conn_comp[j].type == PROTNHURDLE ||
                                   mem->conn_comp[j].type == PSEUDOHURDLE ) &&
                                 mem->conn_comp[j].chain !=
                                 mem->conn_comp[h].chain )
                            {
                                push ( &to_merge2, ( void * ) j );
                            }
                        }

                        /* merge with benign components that do not have
                           separating hurdles */
                        for ( j = 0; j < list_size ( &non_separated_ocs );
                              j++ )
                        {
                            int bc =
                                ( int ) list_get ( &non_separated_ocs, j );

                            /* don't add if the benign component falls in the middle
                               of hurdle h's chain */
                            if ( bc_hurdchain[bc] != mem->conn_comp[h].chain )
                                push ( &to_merge2, ( void * ) bc );
                        }

                        for ( j = 0; j < list_size ( &to_merge1 ); j++ )
                        {
                            int c1 = ( int ) list_get ( &to_merge1, j );
                            for ( k = 0; k < list_size ( &to_merge2 ); k++ )
                            {
                                int c2 = ( int ) list_get ( &to_merge2, k );
                                add_all_merging_reversals ( l,
                                                            &mem->
                                                            conn_comp[c1],
                                                            &mem->
                                                            conn_comp[c2],
                                                            mem->cyclelist,
                                                            c );
                            }
                        }
                        clear_list ( &to_merge1 );
                        clear_list ( &to_merge2 );
                    }
                    free_list ( &to_merge1 );
                    free_list ( &to_merge2 );
                }

                /* enumerate neutral reversals that merge components */
                if ( neutral_mode )
                {
                    List tmpl;
                    init_list ( &tmpl, ncomp, sizeof ( int ) );
                    for ( i = 0; i < list_size ( mem->mhurdles ); i++ )
                    {
                        int hrdl = ( int ) list_get ( mem->mhurdles, i );
                        /* anchor of chain */
                        if ( mem->conn_comp[hrdl].anchor != -1 )
                            push ( &tmpl,
                                   ( void * ) mem->conn_comp[hrdl].anchor );
                        /* hurdle itself, if it's a simple hurdle but not the
                           anchor of its own chain (rare but possible) */
                        if ( mem->conn_comp[hrdl].type == SIMPLEHURDLE &&
                             mem->conn_comp[hrdl].anchor != hrdl )
                            push ( &tmpl, ( void * ) hrdl );
                        /* protected nonhurdles not in chain */
                        for ( j = 0; j < ncomp; j++ )
                        {
                            if ( mem->conn_comp[j].type == PROTNHURDLE ||
                                 mem->conn_comp[j].type == PSEUDOHURDLE )
                            {

                                /* unanchored case */
                                if ( mem->conn_comp[hrdl].anchor == -1 )
                                {
                                    /* only merge if j is the penultimate UOC on the
                                       _other_ end of the hurdle chain; our convention
                                       is to set the "chain" attribute of penultimate
                                       UOCs to -1 * the label of their adjacent hurdles
                                       - 1 (to ensure negativity) */
                                    if ( mem->conn_comp[j].chain < 0 &&
                                         mem->conn_comp[j].chain !=
                                         -1 * hrdl - 1 )
                                        push ( &tmpl, ( void * ) j );
                                }
                                /* anchored case: merge with any PNH of another chain */
                                else
                                {
                                    if ( mem->conn_comp[j].chain !=
                                         mem->conn_comp[hrdl].chain
                                         && j != mem->conn_comp[hrdl].anchor )
                                        push ( &tmpl, ( void * ) j );
                                }
                            }
                        }
                        /* benign comps w/o separating hurdles */
                        for ( j = 0; j < list_size ( &non_separated_ocs );
                              j++ )
                        {
                            int bc =
                                ( int ) list_get ( &non_separated_ocs, j );
                            /* have to distinguish between anchored and unanchored cases */
                            if ( ( mem->conn_comp[hrdl].anchor != -1 &&
                                   bc_hurdchain[bc] !=
                                   mem->conn_comp[hrdl].chain )
                                 || ( mem->conn_comp[hrdl].anchor == -1
                                      && bc_hurdchain[bc] < 0
                                      && bc_hurdchain[bc] != -1 * hrdl - 1 ) )
                                push ( &tmpl, ( void * ) bc );
                        }
                        /* double superhurdle partners */
                        for ( j = 0;
                              j <
                              list_size ( &mem->conn_comp[hrdl].
                                          double_superhurdle_partners ); j++ )
                            if ( ( int )
                                 list_get ( &mem->conn_comp[hrdl].
                                            double_superhurdle_partners,
                                            j ) > hrdl )
                            {
                                /* '>' to be sure we only add once */
                                int partner =
                                    ( int ) list_get ( &mem->conn_comp[hrdl].
                                                       double_superhurdle_partners,
                                                       j );
                                push ( &tmpl, ( void * ) partner );

                                /* also add separated benign components */
                                for ( k = 0; lidx[partner] != -1 &&
                                      k < list_size ( &ocs[lidx[partner]] );
                                      k++ )
                                    push ( &tmpl,
                                           list_get ( &ocs[lidx[partner]],
                                                      k ) );
                            }


                        /* now merge everything in tmpl with hrdl and with each of
                           the BCs that hrdl separates */
                        for ( j = 0; j < list_size ( &tmpl ); j++ )
                        {
                            int comp = ( int ) list_get ( &tmpl, j );
                            if ( hrdl != comp )
                                add_all_merging_reversals ( m,
                                                            &mem->
                                                            conn_comp[hrdl],
                                                            &mem->
                                                            conn_comp[comp],
                                                            mem->cyclelist,
                                                            c );

                            if ( lidx[hrdl] != -1 )
                            {
                                for ( k = 0;
                                      k < list_size ( &ocs[lidx[hrdl]] );
                                      k++ )
                                {
                                    int c1 =
                                        ( int ) list_get ( &ocs[lidx[hrdl]],
                                                           k );
                                    add_all_merging_reversals ( m,
                                                                &mem->
                                                                conn_comp[c1],
                                                                &mem->
                                                                conn_comp
                                                                [comp],
                                                                mem->
                                                                cyclelist,
                                                                c );
                                }
                            }
                        }

                        /* benign comps of different sep classes (only if simple hurdle) */
                        if ( lidx[hrdl] != -1
                             && mem->conn_comp[hrdl].type == SIMPLEHURDLE )
                        {
                            for ( k = 0; k < list_size ( &ocs[lidx[hrdl]] );
                                  k++ )
                            {
                                int k2, c1 =
                                    ( int ) list_get ( &ocs[lidx[hrdl]], k );
                                for ( k2 = k + 1;
                                      k2 < list_size ( &ocs[lidx[hrdl]] );
                                      k2++ )
                                {
                                    int c2 =
                                        ( int ) list_get ( &ocs[lidx[hrdl]],
                                                           k2 );
                                    if ( sepclass[c1] != sepclass[c2] )
                                        add_all_merging_reversals ( m,
                                                                    &mem->
                                                                    conn_comp
                                                                    [c1],
                                                                    &mem->
                                                                    conn_comp
                                                                    [c2],
                                                                    mem->
                                                                    cyclelist,
                                                                    c );
                                }
                            }
                        }

                        clear_list ( &tmpl );
                    }
                    free_list ( &tmpl );
                }

                free_list ( &non_separated_ocs );
            }

            for ( i = 0; i < nseparators; i++ )
                free_list ( &ocs[i] );
            free ( ocs );
        }
    }

    if ( rsm == NULL )
        free_reversal_sorting_memory ( mem );
}

/* To run in normal mode, set source_comp_label to NULL and
   restrict_to to -1.  To restrict to consideration of a certain
   component (as when searching for a new unoriented component), set
   source_comp_label to an array indicating present component labels
   by position and set restrict_to the the label of the desired
   component */
void
find_connected_components ( int begidx, int len, int *inv2, int *upi,
                            int *dest_comp_label,
                            int *source_comp_label, int restrict_to,
                            ReversalSortingMemory * mem )
{
    int extent_b, extent_e;
    int i, top, comp;

    if ( source_comp_label == NULL )    /* be sure that they're
                                           consistent ... */
        restrict_to = -1;

    for ( i = begidx; i < len + begidx; i++ )
    {
        if ( restrict_to != -1 && source_comp_label[i] != restrict_to )
            continue;

        if ( upi[i] % 2 == 0 )
        {
            mem->cc_e[i] = upi[i];
            mem->cc_beg[mem->cc_e[i]] = ( i < inv2[upi[i] + 1] ? i
                                          : inv2[upi[i] + 1] );
            mem->cc_end[mem->cc_e[i]] = ( i < inv2[upi[i] + 1] ?
                                          inv2[upi[i] + 1] : i );
        }
        else
        {
            mem->cc_e[i] = upi[i] - 1;
            mem->cc_beg[upi[i]] = mem->cc_end[upi[i]] = -1;
            /* leave undef for odds */
        }
    }

    for ( i = begidx; i < len + begidx; i++ )
    {
        if ( restrict_to != -1 && source_comp_label[i] != restrict_to )
            continue;
        dest_comp_label[i] = mem->cc_beg[mem->cc_e[i]];
        mem->cc_parent[i] = -1;
    }

    clear_list ( mem->stack );
    for ( i = begidx; i < len + begidx; i++ )
    {
        if ( restrict_to != -1 && source_comp_label[i] != restrict_to )
            continue;

        if ( i == mem->cc_beg[mem->cc_e[i]] )
            push ( mem->stack, ( void * ) mem->cc_e[i] );
        top = ( int ) peek_stack ( mem->stack );

        extent_b = mem->cc_beg[mem->cc_e[i]];
        extent_e = mem->cc_end[mem->cc_e[i]];
        while ( mem->cc_beg[top] > mem->cc_beg[mem->cc_e[i]] )
        {
            extent_b =
                ( extent_b < mem->cc_beg[top] ? extent_b : mem->cc_beg[top] );
            extent_e =
                ( extent_e > mem->cc_end[top] ? extent_e : mem->cc_end[top] );
            mem->cc_parent[mem->cc_beg[top]] = mem->cc_beg[mem->cc_e[i]];
            pop_stack ( mem->stack );
            top = ( int ) peek_stack ( mem->stack );
        }
        mem->cc_beg[top] = ( extent_b < mem->cc_beg[top] ? extent_b :
                             mem->cc_beg[top] );
        mem->cc_end[top] = ( extent_e > mem->cc_end[top] ? extent_e :
                             mem->cc_end[top] );

        if ( i == mem->cc_end[top] )
            pop_stack ( mem->stack );
    }

    comp = 0;
    for ( i = begidx; i < len + begidx; i++ )
    {
        if ( restrict_to != -1 && source_comp_label[i] != restrict_to )
            continue;

        if ( mem->cc_parent[dest_comp_label[i]] == -1 )
        {
            if ( i == dest_comp_label[i] )
            {
                /* beginning of new component;
                   increment label */
                dest_comp_label[i] = comp;
                if ( restrict_to == -1 )    /* don't do this in the "detect" case */
                    mem->conn_comp[comp].begidx =
                        mem->conn_comp[comp].endidx = i;
                comp++;
            }
            else
            {                   /* refer to label at beginning of this
                                   component */
                dest_comp_label[i] = dest_comp_label[dest_comp_label[i]];
                if ( restrict_to == -1 )    /* don't do this in the "detect" case */
                    mem->conn_comp[dest_comp_label[i]].endidx = i + 1;
            }
        }
        else
        {                       /* use label of parent */
            dest_comp_label[i] =
                dest_comp_label[mem->cc_parent[dest_comp_label[i]]];
            if ( restrict_to == -1 )    /* don't do this in the "detect" case */
                mem->conn_comp[dest_comp_label[i]].endidx = i + 1;
        }
    }
}

/* Given an array of connected components, find sequence in which
   unoriented components appear; reduce repeated appearances to single
   instances */
void
build_uoc_list ( List * uoc_list, ConnectedComponent * conn_comp,
                 int *comp_label_by_pos, int n )
{
    int i, prev, current;
    prev = comp_label_by_pos[n - 1];
    for ( i = 0; i < n; i++ )
    {
        current = comp_label_by_pos[i];
        if ( conn_comp[current].type != ORIENTED &&
             conn_comp[current].type != TRIVIAL && current != prev )
        {
            push ( uoc_list, ( void * ) current );
            prev = current;
        }
    }
}

void
find_superhurdles ( List * uoc_list, ConnectedComponent * conn_comp,
                    int nuoc, int ncomp, int *nhurdles, int *nsuperhurdles )
{

    List S, adj_list[nuoc], cycles[nuoc], three_cycles_memb;
    int mark[nuoc], pred[nuoc], label[nuoc], idx[ncomp];
    int i, j, prevlbl, ncycles;

    /* build hurdle graph */
    for ( i = 0; i < nuoc; i++ )
    {
        init_list ( &adj_list[i], nuoc, sizeof ( int ) );   /* could be smaller */
        label[i] = -1;
    }
    for ( i = 0; i < ncomp; i++ )
        idx[i] = -1;
    prevlbl = ( int ) list_get ( uoc_list, list_size ( uoc_list ) - 1 );
    j = 0;
    label[j] = prevlbl;
    idx[prevlbl] = j;
    j++;
    for ( i = 0; i < list_size ( uoc_list ); i++ )
    {
        int currentlbl;
        currentlbl = ( int ) list_get ( uoc_list, i );
        if ( idx[currentlbl] == -1 )
        {
            label[j] = currentlbl;
            idx[currentlbl] = j;
            j++;
        }
        if ( prevlbl != currentlbl &&
             !list_contains ( &adj_list[idx[prevlbl]],
                              ( void * ) ( idx[currentlbl] ) ) )
        {
            /* we'll use a linear search, because
               it will be very unusual that the
               degree of any node is greater than
               3 */
            push ( &adj_list[idx[prevlbl]], ( void * ) idx[currentlbl] );
            push ( &adj_list[idx[currentlbl]], ( void * ) idx[prevlbl] );
        }
        prevlbl = currentlbl;
    }

    /* find cycles; label member nodes */
    init_list ( &S, nuoc, sizeof ( int ) );
    init_list ( &three_cycles_memb, nuoc / 2, sizeof ( int ) );
    for ( i = 0; i < nuoc; i++ )
    {
        mark[i] = 0;
        pred[i] = -1;
        init_list ( &cycles[i], nuoc * 2, sizeof ( int ) ); /* I changed from nuco/2 */
    }
    ncycles = 0;
    push ( &S, 0 );
    while ( !empty ( &S ) )
    {
        int v, u, n, p, cs;
        v = ( int ) pop_stack ( &S );
        p = pred[v];
        if ( mark[v] == 0 )
        {
            mark[v] = 1;
            for ( i = 0; i < list_size ( &adj_list[v] ); i++ )
            {
                n = ( int ) list_get ( &adj_list[v], i );
                if ( n != p )
                {
                    if ( mark[n] == 1 )
                    {
                        push ( &cycles[n], ( void * ) ncycles );
                        cs = 1; /* size of cycle */
                        u = v;
                        while ( u != n )
                        {
                            push ( &cycles[u], ( void * ) ncycles );
                            cs++;
                            u = pred[u];
                        }
                        if ( cs == 3 )
                            push ( &three_cycles_memb, ( void * ) v );
                        ncycles++;
                    }
                    else
                    {
                        push ( &S, ( void * ) n );
                        pred[n] = v;
                    }
                }
            }
        }
    }

    /* label hurdles according to rules of graph */
    *nhurdles = 0;
    *nsuperhurdles = 0;
    for ( i = 0; i < nuoc; i++ )
    {
        int deg, on_cycles;
        deg = list_size ( &adj_list[i] );
        on_cycles = list_size ( &cycles[i] );

        if ( on_cycles > 0 )
        {
            if ( deg >= 3 )
            {
                if ( conn_comp[label[i]].type != PSEUDOHURDLE )
                    conn_comp[label[i]].type = PROTNHURDLE;
                if ( deg > 3 )
                    conn_comp[label[i]].mult_protected = 1;
                conn_comp[label[i]].chain = -1;
            }
            else
            {                   /* degree of exactly 2 */
                conn_comp[label[i]].type = SIMPLEHURDLE;
                ( *nhurdles )++;
            }
        }
        else
        {                       /* not on a cycle */
            if ( deg >= 2 )
            {
                conn_comp[label[i]].type = PROTNHURDLE;
                if ( deg > 2 )
                    conn_comp[label[i]].mult_protected = 1;
                conn_comp[label[i]].chain = -1;
            }
            else
            {                   /* degree of exactly one */
                int n, degn, cyclesn;
                n = ( int ) list_get ( &adj_list[i], 0 );
                degn = list_size ( &adj_list[n] );
                cyclesn = list_size ( &cycles[n] );
                if ( ( cyclesn > 0 && degn == 3 ) ||
                     ( cyclesn == 0 && degn == 2 ) )
                {
                    conn_comp[label[i]].type = SUPHURDLE;
                    ( *nsuperhurdles )++;
                    if ( degn == 3 )
                    {
                        conn_comp[label[n]].type = PSEUDOHURDLE;
                        conn_comp[label[n]].protecting_superhurdle = label[i];
                        conn_comp[label[i]].protected_pseudohurdle = label[n];
                    }
                }
                else
                    conn_comp[label[i]].type = SIMPLEHURDLE;

                ( *nhurdles )++;    /* either way it counts as a hurdle */
            }
        }
    }

    /* find double superhurdles by examining 3-cycles */
    for ( i = 0; i < list_size ( &three_cycles_memb ); i++ )
    {
        int u[3], deg[3], k, l, v, w;
        /* find three vertices of cycle */
        u[0] = ( int ) list_get ( &three_cycles_memb, i );
        u[1] = pred[u[0]];
        u[2] = pred[u[1]];
        for ( j = 0; j < 3; j++ )
        {
            k = ( j + 1 ) % 3;
            l = ( j + 2 ) % 3;
            /* u[k], and u[l] now represent the other vertices of the
               3-cycle (wrt u[i]) */
            deg[j] = list_size ( &adj_list[u[j]] );
            if ( deg[j] == 3
                 || ( deg[j] == 4 && list_size ( &cycles[u[j]] ) == 2 ) )
            {
                /* if this is true, then we know that
                   if the rest of the 3-cycle is
                   "wiped out" by a merge, u[j] will
                   become a hurdle */
                deg[k] = list_size ( &adj_list[u[k]] );
                deg[l] = list_size ( &adj_list[u[l]] );
                if ( deg[k] == 2 )
                    v = u[k];
                else
                    v = find_single_hurdle ( u[k], adj_list, cycles );
                if ( deg[l] == 2 )
                    w = u[l];
                else
                    w = find_single_hurdle ( u[l], adj_list, cycles );
                if ( v != -1 && w != -1 )
                {
                    /* if this is true, then vertices u[k]
                       and u[l] have exactly one hurdle
                       each (now designated by v and w)
                       and those hurdles form a double
                       superhurdle */
                    /* label v and w as a double superhurdle */
                    push ( &conn_comp[label[v]].double_superhurdle_partners,
                           ( void * ) label[w] );
                    push ( &conn_comp[label[w]].double_superhurdle_partners,
                           ( void * ) label[v] );
                }
            }
        }
    }

    /* label each member of a hurdle chain with its chain; also label
       hurdles with anchors */
    for ( i = 0; i < nuoc; i++ )
    {
        if ( conn_comp[label[i]].type == SIMPLEHURDLE ||
             conn_comp[label[i]].type == SUPHURDLE )
        {

            /* climb chain and label each member */
            if ( conn_comp[label[i]].chain == label[i] )
            {
                /* if it has already been seen, chain
                   will be different from i */
                if ( list_size ( &adj_list[i] ) == 1 )
                {
                    int stop = 0;
                    int prevj = i;
                    j = ( int ) list_get ( &adj_list[i], 0 );
                    while ( !stop )
                    {
                        conn_comp[label[j]].chain = label[i];
                        if ( list_size ( &adj_list[j] ) > 2 )
                        {
                            conn_comp[label[i]].anchor = label[j];
                            stop = 1;
                        }
                        else if ( list_size ( &adj_list[j] ) == 1 )
                        {
                            int x1, x2;
                            conn_comp[label[i]].anchor =
                                conn_comp[label[j]].anchor = -1;
                            /* unanchored */
                            stop = 1;

                            /* in the case of an unanchored chain, we need to mark
                               the penultimate members of the chain (in both
                               directions), to facilitate enumeration of neutral
                               reversals */

                            /* We'll use the convention of setting the "chain"
                               attribute of a penultimate PNH to -1 * the label of
                               the hurdle to which it is adjacent - 1 (to ensure
                               negativity; in this case, we won't need the "chain"
                               attribute for normal purposes); in the case of a
                               3-member chain, we'll set it to some large negative
                               value */
                            x1 = label[( int ) list_get ( &adj_list[i], 0 )];
                            x2 = label[( int ) list_get ( &adj_list[j], 0 )];

                            if ( x1 == x2 )
                                conn_comp[x1].chain = NEGINFINITY;
                            else
                            {
                                conn_comp[x1].chain = -1 * label[i] - 1;
                                conn_comp[x2].chain = -1 * label[j] - 1;
                            }
                        }
                        else
                        {       /* degree exactly two (middle of chain) */
                            int newj = ( int ) list_get ( &adj_list[j], 0 );
                            if ( newj == prevj )
                            {
                                prevj = j;
                                j = ( int ) list_get ( &adj_list[j], 1 );
                            }
                            else
                            {
                                prevj = j;
                                j = newj;
                            }
                        }
                    }
                }
            }
        }
    }
}

int
find_single_hurdle ( int u, List * adj_list, List * cycles )
{
    int i, deg, prev, v = -1;

    deg = list_size ( &adj_list[u] );
    if ( deg > 3 )
        return -1;

    /* here we know that deg == 3 */
    for ( i = 0; i < list_size ( &adj_list[u] ); i++ )
    {
        v = ( int ) list_get ( &adj_list[u], i );
        if ( list_size ( &cycles[v] ) == 0 )
            break;
    }

    /* here we know that v is the neighbor of u NOT on the 3-cycle */
    prev = u;
    while ( 1 )
    {
        int next;
        deg = list_size ( &adj_list[v] );
        if ( deg > 2 )
            return -1;
        if ( deg == 1 )
            return v;           /* we've reached the end of the line */

        /* we know v == 2; advance v in the direction AWAY from the
           original 3-cycle */
        next = ( int ) list_get ( &adj_list[v], 0 ) == prev ?
            ( int ) list_get ( &adj_list[v], 1 ) :
            ( int ) list_get ( &adj_list[v], 0 );
        prev = v;
        v = next;
    }

    assert ( 0 );
    return -1;                  /* will never reach this point */
}


#ifdef BITWISE_DETECT
void
build_overlap_matrix ( ConnectedComponent * cc, int *upi, int *inv2,
                       int n, int c, List * cyclelist )
{
    int i, j, k, t, cycno, idx;
    int count[n];

    clear_list ( &cc->beg );
    clear_list ( &cc->end );
    clear_list ( &cc->grey_edges );

    for ( i = 0; i < list_size ( &cc->cyclelist ); i++ )
    {
        cycno = ( int ) list_get ( &cc->cyclelist, i );
        for ( t = 0; t <= 1; t++ )
        {                       /* consider edges of each orientation */
            List *belist;
            belist = &cyclelist[t * ( c + 1 ) + cycno];
            for ( j = 0; j < list_size ( belist ); j++ )
            {
                for ( k = 0; k <= 1; k++ )
                {               /* consider both sides of black edge */
                    idx = ( int ) list_get ( belist, j ) * 2 + k;
                    if ( upi[idx] % 2 == 0 )
                    {           /* consider each gray edge only when we
                                   find its even-numbered end (to
                                   avoid counting twice) */
                        push ( &cc->beg,
                               ( void * ) ( idx <
                                            inv2[upi[idx] +
                                                 1] ? idx : inv2[upi[idx] +
                                                                 1] ) );
                        push ( &cc->end,
                               ( void * ) ( idx <
                                            inv2[upi[idx] +
                                                 1] ? inv2[upi[idx] +
                                                           1] : idx ) );
                        push ( &cc->grey_edges, ( void * ) upi[idx] );
                    }
                }
            }
        }
    }

    set_bitvector_length ( &cc->p, list_size ( &cc->grey_edges ) );
    bitvector_clear ( &cc->p );
    for ( i = 0; i < list_size ( &cc->grey_edges ); i++ )
    {
        set_bitvector_length ( &cc->v[i], list_size ( &cc->grey_edges ) );
        bitvector_clear ( &cc->v[i] );
        count[i] = 0;
    }

    for ( i = 0; i < list_size ( &cc->grey_edges ); i++ )
    {
        bitvector_set ( &cc->v[i], i, 0 );
        for ( j = i + 1; j < list_size ( &cc->grey_edges ); j++ )
        {
            int beg1, beg2, end1, end2;
            beg1 = ( int ) list_get ( &cc->beg, i );
            beg2 = ( int ) list_get ( &cc->beg, j );
            end1 = ( int ) list_get ( &cc->end, i );
            end2 = ( int ) list_get ( &cc->end, j );
            if ( ( beg1 < beg2 && beg2 < end1 && end1 < end2 ) ||
                 ( beg2 < beg1 && beg1 < end2 && end2 < end1 ) )
            {
                bitvector_set ( &cc->v[i], j, 1 );
                bitvector_set ( &cc->v[j], i, 1 );
                count[i]++;
                count[j]++;
            }
        }

        if ( count[i] % 2 == 1 )
            bitvector_set ( &cc->p, i, 1 );
    }
}

int
new_nhurdles_plus_nfortresses ( int ccidx, Reversal * rev,
                                int n, int ncomp, int nuoc,
                                int nhurdles, int nfortresses,
                                ReversalSortingMemory * mem )
{

    int new_nhurdles = 0, new_nfortresses = 0;
    ConnectedComponent *cc;
    List *l;

    cc = &mem->conn_comp[ccidx];
    clear_list ( mem->V );
    printf ( "%d ", mem->V->array[0] );
    detect_new_unoriented_components ( rev, cc, mem );
    if ( empty ( mem->V ) )     /* no new uoc */
        return ( nhurdles + nfortresses );


    /* here, we know there exists at least one new uoc; now we must see
       whether new uocs lead to new hurdles and/or fortresses */

    if ( nuoc + list_size ( mem->V ) >= 3 )
        count_hurdles_after_reversal ( mem->V, mem->comp_label_by_pos,
                                       mem->conn_comp,
                                       rev, mem->inv2, n, ncomp, nuoc,
                                       &new_nhurdles, &new_nfortresses );
    else
        new_nhurdles = nhurdles + list_size ( mem->V );
    /* in this case, no protection is
       possible, so the new uocs must be 
       hurdles; furthermore, a fortress is
       not possible */

    /* be sure to clear mem->V */
    while ( ( l = ( List * ) pop_stack ( mem->V ) ) != NULL )
    {
        free_list ( l );
        free ( l );
    }

    return ( new_nhurdles + new_nfortresses );
}

void
detect_new_unoriented_components ( Reversal * rho,
                                   ConnectedComponent * cc,
                                   ReversalSortingMemory * mem )
{
    int i, k, lidx;
    float rbeg, rend;
    List *U = NULL;

    k = list_size ( &cc->grey_edges );

    /* copy matrix and parity vector */
    for ( i = 0; i < k; i++ )
    {
        set_bitvector_length ( &mem->bv_v[i], k );
        copy_bitvector ( &mem->bv_v[i], &cc->v[i] );
    }
    set_bitvector_length ( &mem->bv_p, k );
    copy_bitvector ( &mem->bv_p, &cc->p );

    /* identify affected vertices */
    rbeg = rho->start * 2 + 0.5;
    rend = rho->stop * 2 + 0.5;
    clear_list ( &mem->bv_pos );
    for ( i = 0; i < k; i++ )
    {
        int beg, end;
        beg = ( int ) list_get ( &cc->beg, i );
        end = ( int ) list_get ( &cc->end, i );
        if ( ( rbeg < beg && beg < rend && rend < end ) ||
             ( beg < rbeg && rbeg < end && end < rend ) )
            push ( &mem->bv_pos, ( void * ) i );
    }
    set_bitvector_length ( &mem->bv_a, k );
    bitvector_set_positions_of_ones ( &mem->bv_a, &mem->bv_pos );

#ifdef DEBUG
    printf ( "Before reversal (%d-%d):\n", rho->start, rho->stop );
    for ( i = 0; i < list_size ( &cc->grey_edges ); i++ )
    {
        printf ( "v(%d) [%d,%d]:\t", i,
                 ( int ) list_get ( &cc->grey_edges, i ),
                 ( int ) list_get ( &cc->grey_edges, i ) + 1 );
        print_bits ( stdout, &mem->bv_v[i] );
    }
    printf ( "p:\t\t" );
    print_bits ( stdout, &mem->bv_p );
    printf ( "a:\t\t" );
    print_bits ( stdout, &mem->bv_a );
#endif

    /* complement subgraph of affected vertices */
    bitvector_get_positions_of_ones ( &mem->bv_a, &mem->bv_pos );
    for ( i = 0; i < list_size ( &mem->bv_pos ); i++ )
    {
        int pos = ( int ) list_get ( &mem->bv_pos, i );
        bitvector_set ( &mem->bv_v[pos], pos, 1 );
        bitwise_xor ( &mem->bv_v[pos], &mem->bv_a, &mem->bv_v[pos] );
    }

#ifdef DEBUG
    for ( i = 0; i < k; i++ )
    {
        if ( ( int ) bitvector_get ( &mem->bv_a, i ) )
        {
            bitvector_set ( &mem->bv_v[i], i, 1 );
            bitwise_xor ( &mem->bv_v[i], &mem->bv_a, &mem->bv_v[i] );
        }
    }
#endif

    /* negate parity of affected vertices */
    bitwise_xor ( &mem->bv_p, &mem->bv_a, &mem->bv_p );

    /* identify affected vertices that are now unoriented */
    set_bitvector_length ( &mem->bv_l, k );
    bitwise_not ( &mem->bv_p, &mem->bv_l );
    bitwise_and ( &mem->bv_l, &mem->bv_a, &mem->bv_l );

#ifdef DEBUG
    printf ( "After reversal (%d-%d):\n", rho->start, rho->stop );
    for ( i = 0; i < list_size ( &cc->grey_edges ); i++ )
    {
        printf ( "v(%d) [%d,%d]:\t", i,
                 ( int ) list_get ( &cc->grey_edges, i ),
                 ( int ) list_get ( &cc->grey_edges, i ) + 1 );
        print_bits ( stdout, &mem->bv_v[i] );
    }
    printf ( "p:\t\t" );
    print_bits ( stdout, &mem->bv_p );
    printf ( "a:\t\t" );
    print_bits ( stdout, &mem->bv_a );
#endif

    /* search for an unoriented component in the new graph */
    for ( i = 0; i < k; i++ )
        mem->ge_mark[i] = -1;

    bitvector_get_positions_of_ones ( &mem->bv_l, &mem->bv_pos );

    if ( list_size ( &mem->bv_pos ) > 0 )
    {
        U = ( List * ) malloc ( sizeof ( List ) );
        init_list ( U, 2 * ( 2 * mem->ngenes + 2 ), sizeof ( int ) );
    }

    for ( lidx = 0; lidx < list_size ( &mem->bv_pos ); lidx++ )
    {
        int oriented, j, m, trivial;
        i = ( int ) list_get ( &mem->bv_pos, lidx );

        if ( mem->ge_mark[i] != -1 )
            continue;

        clear_list ( mem->stack );
        clear_list ( U );

        oriented = 0;
        trivial = 1;
        push ( mem->stack, ( void * ) i );

        while ( oriented == 0 && !empty ( mem->stack ) )
        {
            j = ( int ) pop_stack ( mem->stack );
            push ( U, list_get ( &cc->grey_edges, j ) );
            mem->ge_mark[j] = i;
            for ( m = 0; m < k; m++ )
            {
                if ( bitvector_get ( &mem->bv_v[j], m ) == 0 )
                    continue;
                trivial = 0;
                if ( bitvector_get ( &mem->bv_p, m ) == 1 ||
                     ( mem->ge_mark[m] > -1 && mem->ge_mark[m] < i ) )
                {
                    oriented = 1;
                    break;
                }
                if ( mem->ge_mark[m] == -1 )
                    push ( mem->stack, ( void * ) m );
            }
        }
        if ( oriented == 0 && trivial == 0 )
        {
            push ( mem->V, ( void * ) U );

            if ( lidx < list_size ( &mem->bv_pos ) - 1 )
            {
                /* allocate new list for next pass */
                U = ( List * ) malloc ( sizeof ( List ) );
                init_list ( U, 2 * ( 2 * mem->ngenes + 2 ), sizeof ( int ) );
            }
        }
        else if ( lidx == list_size ( &mem->bv_pos ) - 1 )
        {
            free_list ( U );
            free ( U );
        }
    }
}
#endif

void
count_hurdles_after_reversal ( List * V, int *comp_label_by_position,
                               ConnectedComponent * conn_comp,
                               Reversal * rho,
                               int *inv, int n, int ncomp, int nuoc,
                               int *new_nhurdles, int *new_nfortresses )
{

    int *comp_label_copy, *inv_copy;
    int new_nuoc, i, newcomp, label, startpos, stoppos, new_nsuperhurdles;
    List new_uoc_list;
    ConnectedComponent new_conn_comp[ncomp + list_size ( V )];

    new_nuoc = nuoc + list_size ( V );

    /* create copies of comp_label_by_position and inv, adjusting for
       reversal */
    comp_label_copy = ( int * ) malloc ( n * sizeof ( int ) );
    inv_copy = ( int * ) malloc ( n * sizeof ( int ) );
    startpos = rho->start * 2 + 1;
    stoppos = rho->stop * 2 + 1;
    for ( i = 0; i < n; i++ )
    {
        if ( i < startpos || i >= stoppos )
            comp_label_copy[i] = comp_label_by_position[i];
        else
            comp_label_copy[i] =
                comp_label_by_position[startpos + stoppos - 1 - i];

        if ( inv[i] < startpos || inv[i] >= stoppos )
            inv_copy[i] = inv[i];
        else
            inv_copy[i] = startpos + stoppos - 1 - inv[i];
    }

    /* find positions corresponding to grey_edges in lists within V and
       label them as belonging to new components */
    newcomp = ncomp;
    for ( i = 0; i < list_size ( V ); i++ )
    {
/*    List *U = (List*)list_get(V, i);
    for (j = 0; j < list_size(U); j++) {
      label = (int)list_get(U, j);
      comp_label_copy[inv_copy[label]] = comp_label_copy[inv_copy[label + 1]] = 
        newcomp; 
    }*/
        label = ( int ) list_get ( V, i );
        comp_label_copy[inv_copy[label]] =
            comp_label_copy[inv_copy[label + 1]] = newcomp;
        newcomp++;
    }

    /* copy list of components, setting all uocs to simple hurdles */
    *new_nhurdles = new_nsuperhurdles = 0;
    for ( i = 0; i < ncomp; i++ )
    {
        if ( conn_comp[i].type == SIMPLEHURDLE ||
             conn_comp[i].type == PROTNHURDLE
             || conn_comp[i].type == SUPHURDLE
             || conn_comp[i].type == PSEUDOHURDLE )
        {
            ( *new_nhurdles )++;
            new_conn_comp[i].type = SIMPLEHURDLE;
        }
        else
            new_conn_comp[i].type = conn_comp[i].type;

        init_list ( &new_conn_comp[i].double_superhurdle_partners, 2,
                    sizeof ( int ) );
        new_conn_comp[i].protecting_superhurdle =
            new_conn_comp[i].protected_pseudohurdle = -1;
        new_conn_comp[i].chain = new_conn_comp[i].anchor = i;
        new_conn_comp[i].mult_protected = 0;

        /* we won't use any other attributes of the ConnectedComponent
           structure */
    }


    /* don't forget about new ones! */
    for ( i = ncomp; i < newcomp; i++ )
        new_conn_comp[i].type = SIMPLEHURDLE;
    ( *new_nhurdles ) += newcomp - ncomp;

    /* count hurdles and superhurdles, using the same machinery as
       before */
    init_list ( &new_uoc_list, n, sizeof ( int ) );
    build_uoc_list ( &new_uoc_list, new_conn_comp, comp_label_copy, n );
    find_superhurdles ( &new_uoc_list, new_conn_comp, new_nuoc, newcomp,
                        new_nhurdles, &new_nsuperhurdles );


    if ( new_nsuperhurdles == *new_nhurdles && *new_nhurdles % 2 == 1 )
        *new_nfortresses = 1;
    else
        *new_nfortresses = 0;

    free_list ( &new_uoc_list );
    free ( comp_label_copy );
    free ( inv_copy );
}

#ifndef BITWISE_DETECT
/* Calculate the new number of hurdles and fortresses after a
   specified reversal.  This version simply re-runs
   "connected_components" on the component in question. */

/* Warning: I don't believe this function is generalized to handle the
   case in which multiple new UOCs are introduced (rare but possible).
   It doesn't seem worth fixing now, since the bitwise detect alg
   appears to be preferable under all conditions (in other words,
   always compile with -DBITWISE_DETECT) */
int
new_nhurdles_plus_nfortresses_cc ( int ccidx, Reversal * rev,
                                   int n, int ncomp, int nuoc,
                                   int nhurdles, int nfortresses,
                                   ReversalSortingMemory * mem )
{

    int i, comp, badcomp;
    int new_nhurdles = 0, new_nfortresses = 0;
    int MAXCOMPS;
    ConnectedComponent *cc;

    cc = &mem->conn_comp[ccidx];
    MAXCOMPS = ( cc->endidx - cc->begidx ) / 4 + 1;

    copy_for_reversal ( rev, cc->begidx, cc->endidx - cc->begidx,
                        mem->inv2_cpy, mem->upi_cpy, mem->comp_label_cpy,
                        mem->be_cpy, mem );
    find_connected_components ( cc->begidx, cc->endidx - cc->begidx,
                                mem->inv2_cpy, mem->upi_cpy,
                                mem->new_comp_labels, mem->comp_label_cpy,
                                ccidx, mem );
    for ( i = 0; i < MAXCOMPS; i++ )
    {
        mem->comp_count[i] = mem->comp_mark[i] = 0;
        clear_list ( &mem->comp_grey_edges[i] );
    }

    for ( i = cc->begidx; i < cc->endidx; i++ )
    {
        if ( mem->comp_label_cpy[i] != ccidx )
            continue;

        comp = mem->new_comp_labels[i];

        /* consider grey edge only when finding its even-numbered end */
        if ( mem->upi_cpy[i] % 2 == 0 )
        {
            push ( &mem->comp_grey_edges[comp], ( void * ) mem->upi_cpy[i] );
        }

        mem->comp_count[comp]++;

        if ( mem->comp_mark[comp] == 0 && i % 2 == 0
             && mem->be_cpy[i / 2] == 1 )
            mem->comp_mark[comp] = 1;   /* a single divergent black edge is
                                           enough to ensure that the entire 
                                           component is oriented */
    }

    badcomp = -1;
    for ( i = 0; i < MAXCOMPS; i++ )
        if ( mem->comp_mark[i] == 0 && mem->comp_count[i] > 2 )
            /* note that trivial components are
               handled specially */
            badcomp = i;

    if ( badcomp == -1 )
    {
        new_nhurdles = nhurdles;
        new_nfortresses = nfortresses;
    }
    else
    {

        /* here we know that the reversal has created at least one
           unoriented component */

        if ( nuoc + 1 >= 3 )
            count_hurdles_after_reversal ( &mem->comp_grey_edges[badcomp],
                                           mem->comp_label_by_pos,
                                           mem->conn_comp,
                                           rev, mem->inv2, n, ncomp, nuoc,
                                           &new_nhurdles, &new_nfortresses );
        else
            new_nhurdles = nhurdles + 1;
        /* in this case, no protection is
           possible, so the new uoc must be a
           hurdle; furthermore, a fortress is
           not possible */

    }

    return ( new_nhurdles + new_nfortresses );
}
#endif

/* add to the list all reversals that merge specified two components */
void
add_all_merging_reversals ( List * l, ConnectedComponent * c1,
                            ConnectedComponent * c2, List * cyclelist, int c )
{
    int i, j, k, k2, a, b, o1, o2;
    Reversal *rev;
    List *edges1, *edges2;

    /* add every pair of black edges from the two cycles */
    for ( i = 0; i < list_size ( &c1->cyclelist ); i++ )
    {
        for ( j = 0; j < list_size ( &c2->cyclelist ); j++ )
        {
            for ( o1 = 0; o1 < 2; o1++ )
            {
                for ( o2 = 0; o2 < 2; o2++ )
                {
                    edges1 =
                        &cyclelist[o1 * ( c + 1 ) +
                                   ( int ) list_get ( &c1->cyclelist, i )];
                    edges2 =
                        &cyclelist[o2 * ( c + 1 ) +
                                   ( int ) list_get ( &c2->cyclelist, j )];
                    for ( k = 0; k < list_size ( edges1 ); k++ )
                    {
                        for ( k2 = 0; k2 < list_size ( edges2 ); k2++ )
                        {
                            a = ( int ) list_get ( edges1, k );
                            b = ( int ) list_get ( edges2, k2 );
                            rev =
                                ( Reversal * ) malloc ( sizeof ( Reversal ) );
                            rev->start = ( a <= b ? a : b );
                            rev->stop = ( a <= b ? b : a );
                            push ( l, rev );
                        }
                    }
                }
            }
        }
    }
}

void
add_all_cutting_reversals ( List * l, ConnectedComponent * cc,
                            List * cyclelist )
{
    int j, k, k2, a, b;
    List *edges;
    Reversal *rev;
    for ( j = 0; j < list_size ( &cc->cyclelist ); j++ )
    {
        edges = &cyclelist[( int ) list_get ( &cc->cyclelist, j )];
        for ( k = 0; k < list_size ( edges ); k++ )
        {
            for ( k2 = k + 1; k2 < list_size ( edges ); k2++ )
            {
                a = ( int ) list_get ( edges, k );
                b = ( int ) list_get ( edges, k2 );
                rev = ( Reversal * ) malloc ( sizeof ( Reversal ) );
                rev->start = ( a <= b ? a : b );
                rev->stop = ( a <= b ? b : a );
                push ( l, rev );
            }
        }
    }
}

/* Returns a value indicating whether merging components h1 and h2
   will create a fortress */
int
eliminating_components_creates_fortress ( int h1, int h2,
                                          ConnectedComponent * conn_comp,
                                          int nhurdles, int nsuperhurdles )
{
    if ( nsuperhurdles >= 3 &&
         nsuperhurdles % 2 == 1 &&
         nhurdles - nsuperhurdles == 2 &&
         conn_comp[h1].type == SIMPLEHURDLE &&
         conn_comp[h2].type == SIMPLEHURDLE )
        return 1;

    if ( nsuperhurdles >= 4 &&
         nsuperhurdles % 2 == 0 &&
         nhurdles - nsuperhurdles == 1 &&
         ( ( conn_comp[h1].type == SIMPLEHURDLE &&
             conn_comp[h2].type == SUPHURDLE ) ||
           ( conn_comp[h1].type == SUPHURDLE &&
             conn_comp[h2].type == SIMPLEHURDLE ) ) )
        return 1;

    return 0;
}

/* Returns 1 iff h1 and h2 form a double superhurdle */
int
form_double_superhurdle ( int h1, int h2, ConnectedComponent * conn_comp )
{

    if ( ( list_size ( &conn_comp[h1].double_superhurdle_partners ) >= 1 &&
           ( int ) list_get ( &conn_comp[h1].double_superhurdle_partners,
                              0 ) == h2 )
         || ( list_size ( &conn_comp[h1].double_superhurdle_partners ) == 2
              && ( int ) list_get ( &conn_comp[h1].
                                    double_superhurdle_partners, 1 ) == h2 ) )
        return 1;

    return 0;
}

ReversalSortingMemory *
new_reversal_sorting_memory ( int ngenes )
{
    ReversalSortingMemory *mem;
    int n = 2 * ngenes + 2;
    int i;

    mem =
        ( ReversalSortingMemory * )
        malloc ( sizeof ( ReversalSortingMemory ) );
    mem->ngenes = ngenes;
    mem->inv = ( int * ) malloc ( ( ngenes + 1 ) * sizeof ( int ) );
    mem->upi = ( int * ) malloc ( n * sizeof ( int ) );
    mem->inv2 = ( int * ) malloc ( n * sizeof ( int ) );
    mem->be = ( int * ) malloc ( ( ngenes + 1 ) * sizeof ( int ) );
    mem->cycle = ( int * ) malloc ( ( ngenes + 1 ) * sizeof ( int ) );
    mem->comp_label_by_pos = ( int * ) malloc ( n * sizeof ( int ) );
    mem->comp_label_by_cycle =
        ( int * ) malloc ( ( ngenes + 1 ) * sizeof ( int ) );
    mem->cyclelist =
        ( List * ) malloc ( 2 * ( ngenes + 2 ) * sizeof ( List ) );
    mem->conn_comp =
        ( ConnectedComponent * ) malloc ( ( ngenes + 1 ) *
                                          sizeof ( ConnectedComponent ) );

    mem->cc_beg = ( int * ) malloc ( n * sizeof ( int ) );
    mem->cc_end = ( int * ) malloc ( n * sizeof ( int ) );
    mem->cc_e = ( int * ) malloc ( n * sizeof ( int ) );
    mem->cc_parent = ( int * ) malloc ( n * sizeof ( int ) );
    mem->stack = ( List * ) malloc ( sizeof ( List ) );
    init_list ( mem->stack, n * 4, sizeof ( int ) );

    mem->V = ( List * ) malloc ( sizeof ( List ) );
    init_list ( mem->V, n / 4 + 1, sizeof ( int ) );

    mem->mhurdles = ( List * ) malloc ( sizeof ( List ) );
    init_list ( mem->mhurdles, 2 * ( ngenes + 1 ), sizeof ( int ) );
    for ( i = 0; i < 2 * ( ngenes + 2 ); i++ )
        init_list ( &mem->cyclelist[i], ngenes + 1, sizeof ( int ) );
    for ( i = 0; i < ngenes + 1; i++ )
    {
        init_list ( &mem->conn_comp[i].cyclelist, ngenes + 1,
                    sizeof ( int ) );
        init_list ( &mem->conn_comp[i].double_superhurdle_partners, 2,
                    sizeof ( int ) );
    }

#ifdef BITWISE_DETECT
    init_bitvector ( &mem->bv_a, ngenes + 1 );
    init_bitvector ( &mem->bv_p, ngenes + 1 );
    init_bitvector ( &mem->bv_l, ngenes + 1 );
    mem->bv_v =
        ( BitVector * ) malloc ( ( ngenes + 1 ) * sizeof ( BitVector ) );
    for ( i = 0; i < ngenes + 1; i++ )
        init_bitvector ( &mem->bv_v[i], ngenes + 1 );
    mem->ge_mark = ( int * ) malloc ( ( ngenes + 1 ) * sizeof ( int ) );
    for ( i = 0; i < ngenes + 1; i++ )
    {
        init_list ( &mem->conn_comp[i].beg, n, sizeof ( int ) );
        init_list ( &mem->conn_comp[i].end, n, sizeof ( int ) );
        init_list ( &mem->conn_comp[i].grey_edges, n, sizeof ( int ) );
        init_bitvector ( &mem->conn_comp[i].p, ngenes + 1 );
        mem->conn_comp[i].v =
            ( BitVector * ) malloc ( ( ngenes + 1 ) * sizeof ( BitVector ) );
        for ( j = 0; j < ngenes + 1; j++ )
            init_bitvector ( &mem->conn_comp[i].v[j], ngenes + 1 );
    }
    init_list ( &mem->bv_pos, ngenes + 1, sizeof ( int ) );
#else
    mem->upi_cpy = ( int * ) malloc ( n * sizeof ( int ) );
    mem->comp_label_cpy = ( int * ) malloc ( n * sizeof ( int ) );
    mem->inv2_cpy = ( int * ) malloc ( n * sizeof ( int ) );
    mem->new_comp_labels = ( int * ) malloc ( n * sizeof ( int ) );
    mem->be_cpy = ( int * ) malloc ( n * sizeof ( int ) );
    mem->comp_mark = ( int * ) malloc ( ( n / 4 + 1 ) * sizeof ( int ) );
    mem->comp_count = ( int * ) malloc ( ( n / 4 + 1 ) * sizeof ( int ) );
    mem->comp_grey_edges =
        ( List * ) malloc ( ( n / 4 + 1 ) * sizeof ( List ) );
    for ( i = 0; i < n / 4 + 1; i++ )
        init_list ( &mem->comp_grey_edges[i], n / 2, sizeof ( int ) );
#endif

    return mem;
}

void
free_reversal_sorting_memory ( ReversalSortingMemory * mem )
{
    int i, n;

    n = 2 * mem->ngenes + 2;
    free ( mem->inv );
    free ( mem->upi );
    free ( mem->inv2 );
    free ( mem->be );
    free ( mem->cycle );
    free ( mem->comp_label_by_pos );
    free ( mem->comp_label_by_cycle );
    free_list ( mem->V );
    free ( mem->V );
    free_list ( mem->mhurdles );
    free ( mem->mhurdles );
    for ( i = 0; i < 2 * ( mem->ngenes + 2 ); i++ )
        free_list ( &mem->cyclelist[i] );
    free ( mem->cyclelist );
    for ( i = 0; i < mem->ngenes + 1; i++ )
    {
        free_list ( &mem->conn_comp[i].cyclelist );
        free_list ( &mem->conn_comp[i].double_superhurdle_partners );
    }
    free ( mem->conn_comp );
    free ( mem->cc_beg );
    free ( mem->cc_end );
    free ( mem->cc_e );
    free ( mem->cc_parent );
    free_list ( mem->stack );
    free ( mem->stack );

#ifdef BITWISE_DETECT
    for ( i = 0; i < mem->ngenes + 1; i++ )
    {
        free_list ( &mem->conn_comp[i].beg );
        free_list ( &mem->conn_comp[i].end );
        free_list ( &mem->conn_comp[i].grey_edges );
        free_bitvector ( &mem->conn_comp[i].p );
        for ( j = 0; j < mem->ngenes + 1; j++ )
            free_bitvector ( &mem->conn_comp[i].v[j] );
        free ( mem->conn_comp[i].v );
    }
    for ( i = 0; i < mem->ngenes + 1; i++ )
        free_bitvector ( &mem->bv_v[i] );
    free_bitvector ( &mem->bv_p );
    free_bitvector ( &mem->bv_a );
    free_bitvector ( &mem->bv_l );
    free ( mem->bv_v );
    free ( mem->ge_mark );
    free_list ( &mem->bv_pos );
#else
    free ( mem->upi_cpy );
    free ( mem->comp_label_cpy );
    free ( mem->inv2_cpy );
    free ( mem->new_comp_labels );
    free ( mem->be_cpy );
    for ( i = 0; i < ( n / 4 + 1 ); i++ )
        free_list ( &mem->comp_grey_edges[i] );
    free ( mem->comp_mark );
    free ( mem->comp_count );
    free ( mem->comp_grey_edges );
    free ( mem );
#endif
}

/* create copies of key arrays, adjusting for reversal */
void
copy_for_reversal ( Reversal * rev, int begidx, int len, int *inv2_cpy,
                    int *upi_cpy, int *comp_label_cpy, int *be_cpy,
                    ReversalSortingMemory * mem )
{

    int startpos, stoppos, i, j;

    startpos = rev->start * 2 + 1;
    stoppos = rev->stop * 2 + 1;

    for ( i = begidx; i < begidx + len; i++ )
    {
        if ( i < startpos || i >= stoppos )
        {
            comp_label_cpy[i] = mem->comp_label_by_pos[i];
            upi_cpy[i] = mem->upi[i];
        }
        else
        {
            comp_label_cpy[i] =
                mem->comp_label_by_pos[startpos + stoppos - 1 - i];
            upi_cpy[i] = mem->upi[startpos + stoppos - 1 - i];
        }
        inv2_cpy[upi_cpy[i]] = i;
    }

    /* reset black edges for region */
    for ( i = begidx; i < begidx + len; i += 2 )
        be_cpy[i / 2] = -1;
    for ( i = begidx; i < begidx + len; i += 2 )
    {
        if ( be_cpy[i / 2] != -1 )
            continue;
        j = i;
        while ( be_cpy[j / 2] == -1 )
        {
            if ( j % 2 != 0 )
            {                   /* (odd index) */
                j--;            /* follow black edge */
                be_cpy[j / 2] = 1;  /* record orientation */
            }
            else
            {                   /* (even index) */
                be_cpy[j / 2] = 0;  /* record orientation */
                j++;            /* follow black edge */
            }
            if ( upi_cpy[j] % 2 == 0 )  /* follow gray edge */
                j = inv2_cpy[upi_cpy[j] + 1];
            else
                j = inv2_cpy[upi_cpy[j] - 1];
        }
    }
}


/* Find all separating hurdles and the benign components that they
   separate */
void
find_separating_hurdles ( int *is_separator, int *sepclass,
                          int *bc_hurdchain, int *nseparators, int n,
                          int ncomp, ReversalSortingMemory * mem, List ** ocs,
                          int *lidx, int *llabel, int *is_separated,
                          int neutral_mode, int nfortresses )
{

    List tmpl, separated_ocs, separating_hurdles;
    int mark[n];
    int currenth, prevuoc, currentuoc, start, comp, class = 0, i, j;

    /* init arrays */
    for ( i = 0; i < n; i++ )
        mark[i] = 0;
    for ( i = 0; i < ncomp; i++ )
    {
        is_separator[i] = 0;
        sepclass[i] = -1;
    }

    /* init lists */
    init_list ( &tmpl, ncomp, sizeof ( int ) );
    init_list ( &separated_ocs, ncomp, sizeof ( int ) );
    init_list ( &separating_hurdles, ncomp, sizeof ( int ) );

    /* necessary to keep track of benign comps that fall in the middle
       of hurdle chains; see below */
    for ( i = 0; i < ncomp; i++ )
        bc_hurdchain[i] = ncomp;

    /* find a starting point on a hurdle */
    for ( i = 0; i < n; i++ )
    {
        if ( mem->conn_comp[mem->comp_label_by_pos[i]].type == SIMPLEHURDLE ||
             mem->conn_comp[mem->comp_label_by_pos[i]].type == SUPHURDLE )
            break;
    }

    if ( i == n )
        i = 0;
    currenth = currentuoc = mem->comp_label_by_pos[i];
    start = i;
    for ( i = start + 1; i <= start + n; i++ )
    {

        comp = mem->comp_label_by_pos[i % n];
        if ( comp == currenth )
        {
            /* we have come to a subsequent
               position associated with the last
               hurdle seen */

            while ( !empty ( &tmpl ) )
            {
                /* save any oriented components in
                   tmp1 along with their separating
                   hurdle (currenth) */
                comp = ( int ) pop_stack ( &tmpl );
                mark[comp] = 0; /* unmark as we empty list */
                sepclass[comp] = class;
                push ( &separated_ocs, ( void * ) comp );
                push ( &separating_hurdles, ( void * ) currenth );
                if ( is_separator[currenth] == 0 )
                {
                    is_separator[currenth] = 1;
                    ( *nseparators )++;
                }
            }
            class++;
        }

        else if ( mem->conn_comp[comp].type == SIMPLEHURDLE ||
                  mem->conn_comp[comp].type == SUPHURDLE ||
                  mem->conn_comp[comp].type == PSEUDOHURDLE ||
                  mem->conn_comp[comp].type == PROTNHURDLE )
        {

            prevuoc = currentuoc;
            currentuoc = comp;

            if ( mem->conn_comp[comp].type == SIMPLEHURDLE ||
                 mem->conn_comp[comp].type == SUPHURDLE )
                currenth = comp;

            while ( !empty ( &tmpl ) )
            {
                int c = ( int ) pop_stack ( &tmpl );
                mark[c] = 0;

                /* while we're at it, we'll keep track of benign components
                   that fall in the "middle" of hurdle chains (i.e., that
                   have members of the same chain on either side); this
                   information will be important later */

                /* if the BC falls between a hurdle and a protected
                   nonhurdle, both belonging to an unanchored chain, we need
                   to treat it as a special case, in order to enumerate
                   neutral reversals properly.  Our convention will be to
                   mark its chain as being equal to -1 * the label of the
                   hurdle - 1 (to ensure a negative value) */
                if ( ( mem->conn_comp[prevuoc].type == SIMPLEHURDLE ||
                       mem->conn_comp[prevuoc].type == SUPHURDLE ) &&
                     mem->conn_comp[prevuoc].anchor == -1 &&
                     mem->conn_comp[currentuoc].type != SIMPLEHURDLE &&
                     mem->conn_comp[currentuoc].type != SUPHURDLE )
                    bc_hurdchain[c] = -1 * prevuoc - 1;
                else if ( ( mem->conn_comp[currentuoc].type == SIMPLEHURDLE ||
                            mem->conn_comp[currentuoc].type == SUPHURDLE ) &&
                          mem->conn_comp[currentuoc].anchor == -1 &&
                          mem->conn_comp[prevuoc].type != SIMPLEHURDLE &&
                          mem->conn_comp[prevuoc].type != SUPHURDLE )
                    bc_hurdchain[c] = -1 * currentuoc - 1;

                /* alternatively, if the BC falls between edges belonging to
                   the same UOC, and that UOC is the penultimate one in an
                   unanchored hurdle chain, set bc_hurdchain[c] to chain of UOC */
                else if ( prevuoc == currentuoc
                          && mem->conn_comp[prevuoc].chain < 0 )
                    bc_hurdchain[c] = mem->conn_comp[prevuoc].chain;

                /* otherwise if the UOCs on either side of the BC belong to
                   the same chain, we will simply mark the BC with that
                   chain's number (note, however, that we make exceptions
                   for BCs that fall in a "fold" of the anchor of a chain,
                   and for anchors of chains that are "multiply protected") */
                else if ( mem->conn_comp[prevuoc].chain >= 0 &&
                          ( prevuoc != currentuoc ||
                            prevuoc !=
                            mem->conn_comp[mem->conn_comp[prevuoc].chain].
                            anchor ) )
                {
                    /* if a PNH is the anchor of multiple chains, we cannot
                       rely on its "chain" attribute */
                    if ( ( mem->conn_comp[prevuoc].chain ==
                           mem->conn_comp[currentuoc].chain ||
                           mem->conn_comp[mem->conn_comp[currentuoc].chain].
                           anchor == prevuoc )
                         && ( mem->conn_comp[prevuoc].type != PROTNHURDLE
                              || mem->conn_comp[prevuoc].mult_protected != 1 )
                         && ( mem->conn_comp[currentuoc].type != PROTNHURDLE
                              || mem->conn_comp[currentuoc].mult_protected !=
                              1 ) )
                        bc_hurdchain[c] = mem->conn_comp[currentuoc].chain;
                    else if ( mem->conn_comp[mem->conn_comp[prevuoc].chain].
                              anchor == currentuoc
                              && ( mem->conn_comp[currentuoc].type !=
                                   PROTNHURDLE
                                   || mem->conn_comp[currentuoc].
                                   mult_protected != 1 ) )
                        bc_hurdchain[c] = mem->conn_comp[prevuoc].chain;
                }
            }
        }

        else if ( ( mem->conn_comp[comp].type == ORIENTED ||
                    mem->conn_comp[comp].type == TRIVIAL )
                  && mark[comp] == 0 )
        {
            /* we've encountered an oriented
               component that could be mergeable;
               store in tmpl */
            mark[comp] = 1;
            push ( &tmpl, ( void * ) comp );
        }
    }

    if ( neutral_mode || *nseparators > 0 || nfortresses > 0 )
    {
        int h, oc, counter = 0;

        /* sort separated ocs by their separating hurdles */
        *ocs = ( List * ) malloc ( *nseparators * sizeof ( List ) );
        for ( i = 0; i < *nseparators; i++ )
        {
            init_list ( &( *ocs )[i], list_size ( &separated_ocs ),
                        sizeof ( int ) );
            llabel[i] = -1;
        }
        for ( i = 0; i < ncomp; i++ )
        {
            lidx[i] = -1;
            is_separated[i] = 0;
        }
        for ( i = 0; i < list_size ( &separated_ocs ); i++ )
        {
            oc = ( int ) list_get ( &separated_ocs, i );
            is_separated[oc] = 1;
            h = ( int ) list_get ( &separating_hurdles, i );
            j = lidx[h];
            if ( j == -1 )
            {
                j = counter++;
                lidx[h] = j;
                llabel[j] = h;
            }
            push ( &( *ocs )[j], ( void * ) oc );
        }
    }

    free_list ( &tmpl );
    free_list ( &separated_ocs );
    free_list ( &separating_hurdles );
}
