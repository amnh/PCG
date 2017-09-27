#include "structs.h"
#include "greedy_median.h"

/* the following 2 inline routines all have the same function:
   they pursue all forced consequences of picking a particular edge
   as the edge leaving i; they return the number of edges forcibly
   chosen, including the original */

/* i->succ1[i] is the chosen adjacency */
INLINE int
finish1 ( int i, int *count, int *degree, int *succ1, int *succ2,
          int *pred1, int *pred2, int *tour, int *otherEnd, int num_genes )
{
    int i2, ind, incr, aff1, aff2, otherEndi, otherEndind;

    /* record and count initial edge from i to succ1[i] */
    tour[i] = ind = succ1[i];
    incr = 1;
    *count += 1;
#ifdef DEBUG
    fprintf ( outfile, "finish1, selecting edge (%3d,%3d)\n", i, succ1[i] );
    fflush ( outfile );
#endif
    degree[i] += 1;             /* add outgoing edge */
    degree[-i] = -1;            /* flag complement unusable */
    degree[ind] += 2;           /* add incoming edge */
    degree[-ind] = -1;          /* flag complement unusable */
#ifdef DEBUG
    fprintf ( outfile, "finish1, otherEnd[%3d]=%3d, otherEnd[%3d]=%3d\n",
              i, otherEnd[i], ind, otherEnd[ind] );
    fflush ( outfile );
#endif
    otherEndi = otherEnd[i];
    otherEndind = otherEnd[ind];
    otherEnd[otherEndi] = otherEndind;
    otherEnd[otherEndind] = otherEndi;

    /* need to check pred1[succ2[i]] and pred2[succ1[i]], the two endpoints
       affected by the choice of arc (i,succ1[i]) */
    if ( succ2[i] != 0 )
    {
        aff1 = pred1[succ2[i]];
        if ( aff1 != 0 )
        {
            i2 = succ1[aff1];
            if ( ( ( degree[aff1] == 0 ) || ( degree[aff1] == 2 ) )
                 && ( i2 != 0 ) )
            {
                if ( ( ( degree[i2] == 0 ) || ( degree[i2] == 1 ) ) &&  /* i2 can receive */
                     ( ( otherEnd[aff1] != i2 ) || ( *count == num_genes - 1 ) )    /* no short loop */
                     )
                {
                    /* must pick edge (aff1,succ1[aff1]) */
                    incr +=
                        finish1 ( aff1, count, degree, succ1, succ2, pred1,
                                  pred2, tour, otherEnd, num_genes );
                }
            }
        }
    }
    aff2 = pred2[ind];
    if ( aff2 != 0 )
    {
        i2 = succ1[aff2];
        if ( ( ( degree[aff2] == 0 ) || ( degree[aff2] == 2 ) )
             && ( i2 != 0 ) )
        {
            if ( ( ( degree[i2] == 0 ) || ( degree[i2] == 1 ) ) &&  /* i2 can receive */
                 ( ( otherEnd[aff2] != i2 ) || ( *count == num_genes - 1 ) )    /* no short loop */
                 )
            {
                /* must pick edge (aff2,succ1[aff2]) */
                incr +=
                    finish1 ( aff2, count, degree, succ1, succ2, pred1, pred2,
                              tour, otherEnd, num_genes );
            }
        }
    }

    return ( incr );
}


/* i->succ2[i] is the chosen adjacency */
INLINE int
finish2 ( int i, int *count, int *degree, int *succ1, int *succ2,
          int *pred1, int *pred2, int *tour, int *otherEnd, int num_genes )
{
    int i2, ind, incr, aff1, aff2, otherEndi, otherEndind;

    /* record and count initial edge from i to succ2[i] */
    tour[i] = ind = succ2[i];
    incr = 1;
    *count += 1;
#ifdef DEBUG
    fprintf ( outfile, "finish2, selecting edge (%3d,%3d)\n", i, succ2[i] );
    fflush ( outfile );
#endif
    degree[i] += 1;             /* add outgoing edge */
    degree[-i] = -1;            /* flag complement unusable */
    degree[ind] += 2;           /* add incoming edge */
    degree[-ind] = -1;          /* flag complement unusable */
#ifdef DEBUG
    fprintf ( outfile, "finish2, otherEnd[%3d]=%3d, otherEnd[%3d]=%3d\n",
              i, otherEnd[i], ind, otherEnd[ind] );
    fflush ( outfile );
#endif
    otherEndi = otherEnd[i];
    otherEndind = otherEnd[ind];
    otherEnd[otherEndi] = otherEndind;
    otherEnd[otherEndind] = otherEndi;

    /* need to check pred2[succ1[i]] and pred1[succ2[i]], the two endpoints
       affected by the choice of arc (i,succ2[i]) */
    if ( succ1[i] != 0 )
    {
        aff1 = pred2[succ1[i]];
        if ( aff1 != 0 )
        {
            i2 = succ2[aff1];
            if ( ( ( degree[aff1] == 0 ) || ( degree[aff1] == 2 ) )
                 && ( i2 != 0 ) )
            {
                if ( ( ( degree[i2] == 0 ) || ( degree[i2] == 1 ) ) &&  /* i2 can receive */
                     ( ( otherEnd[aff1] != i2 ) || ( *count == num_genes - 1 ) )    /* no short loop */
                     )
                {
                    /* must pick edge (aff1,succ1[aff1]) */
                    incr +=
                        finish2 ( aff1, count, degree, succ1, succ2, pred1,
                                  pred2, tour, otherEnd, num_genes );
                }
            }
        }
    }
    aff2 = pred1[ind];
    if ( aff2 != 0 )
    {
        i2 = succ2[aff2];
        if ( ( ( degree[aff2] == 0 ) || ( degree[aff2] == 2 ) )
             && ( i2 != 0 ) )
        {
            if ( ( ( degree[i2] == 0 ) || ( degree[i2] == 1 ) ) &&  /* i2 can receive */
                 ( ( otherEnd[aff2] != i2 ) || ( *count == num_genes - 1 ) )    /* no short loop */
                 )
            {
                /* must pick edge (aff2,succ1[aff2]) */
                incr +=
                    finish2 ( aff2, count, degree, succ1, succ2, pred1, pred2,
                              tour, otherEnd, num_genes );
            }
        }
    }

    return ( incr );
}


void
greedy_median ( int *gene, struct genome_struct *g1, struct genome_struct *g2,
                int num_genes, int *degree, int *succ1, int *succ2f,
                int *succ2b, int *pred1, int *pred2, int *tour,
                int *otherEnd )
{
    /* goes through both genomes, constructs two arrays, indexed by vertex,
       of adjacencies that exist in one genome but not the other, as well
       as a degree array and a tour array; then repeatedly starts at
       a vertex of degree 1, picks an edge from the genome that so far
       has fewer degree edges, and pursues all forced actions (degree 2
       vertices eliminate the other incident edge; degree 1 vertices with
       only 1 remaining edge are forced */
    /* leaves junk in arrays degree, succ1 (stack), succ2f (picked),
       succ2b (incycle), pred1, pred2, tour (outcycle), and otherEnd */
    /* the greedy median genome itself is returned in array tour */

    /* indices */
    int i, ind, i1, i2, otherEndi, otherEndind;

    /* gene values */
    int gen1, gen2, gen1first;

    /* counters */
    int count, count1, count2, shared_forward, shared_backward;

    /* pointer within array */
    int first, current;
    int *succ2, *stacknone, *stackin;

    /* stack pointers */
    int topnone, topin;

#ifdef DEBUG
    fprintf ( outfile,
              "Entering greedy_median(): num_genes=%3d, g1=%p, g2=%p\n",
              num_genes, g1, g2 );
    fflush ( outfile );
#endif
    /* note: this is using a different encoding than the
       conversion to tsp -- no negation of next gene -- so
       array pointers have to point to the middle */
    degree += num_genes;
    succ1 += num_genes;
    succ2f += num_genes;
    succ2b += num_genes;
    pred1 += num_genes;
    pred2 += num_genes;
    otherEnd += num_genes;
    tour += num_genes;
    /* reset entries */
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        tour[i] = 0;
        succ1[i] = succ2f[i] = succ2b[i] = pred1[i] = pred2[i] = 0;
        otherEnd[i] = i;
        degree[i] = -1;         /* flag not usable */
    }

    /* first set up adjacencies -- succ and pred for each genome */
    /* loop is unrolled for efficiency */
    /* first adjacency */
    gen1 = g1->genes[0];
    gen2 = g1->genes[1];
#ifdef DEBUG
    if ( ( gen1 < -num_genes ) || ( gen1 > num_genes ) ||
         ( gen2 < -num_genes ) || ( gen2 > num_genes ) )
    {
        fprintf ( outfile, "error1 in greedy_median:gen1=%3d, gen2=%3d\n",
                  gen1, gen2 );
        exit ( -1 );
    }
#endif
    degree[gen1] = 0;           /* flag usable */
    succ1[gen1] = gen2;
    pred1[gen2] = gen1;
    gen1first = gen1;
    /* all middle adjacencies */
    for ( i = 2; i < num_genes; i++ )
    {
        gen1 = gen2;
        gen2 = g1->genes[i];
#ifdef DEBUG
        if ( ( gen1 < -num_genes ) || ( gen1 > num_genes ) ||
             ( gen2 < -num_genes ) || ( gen2 > num_genes ) )
        {
            fprintf ( outfile, "error2 in greedy_median:gen1=%3d, gen2=%3d\n",
                      gen1, gen2 );
            exit ( -1 );
        }
#endif
        degree[gen1] = 0;       /* flag usable */
        succ1[gen1] = gen2;
        pred1[gen2] = gen1;
    }
    /* last adjacency */
    gen1 = gen2;
    gen2 = gen1first;
    degree[gen1] = 0;           /* flag usable */
    succ1[gen1] = gen2;
    pred1[gen2] = gen1;
#ifdef DEBUG
    fprintf ( outfile, "first genome processed\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "pred1[%3d]=%3d, succ1[%3d]=%3d\n", i, pred1[i], i,
                  succ1[i] );
#endif

    /* we do not know in which relative order we should scan genome 2,
       so try both and choose the one with the larger number of shared
       adjacencies */
    shared_backward = shared_forward = 0;   /* count of shared adjacencies */
    /* loop is unrolled for efficiency */
    /* first adjacency */
    gen1 = g2->genes[0];
    gen2 = g2->genes[1];
#ifdef DEBUG
    if ( ( gen1 < -num_genes ) || ( gen1 > num_genes ) ||
         ( gen2 < -num_genes ) || ( gen2 > num_genes ) )
    {
        fprintf ( outfile, "error3 in greedy_median:gen1=%3d, gen2=%3d\n",
                  gen1, gen2 );
        exit ( -1 );
    }
#endif
    succ2f[gen1] = gen2;
    succ2b[-gen2] = -gen1;      /* set up both arrays -- we'll pick one later */
    if ( succ1[gen1] == gen2 )
        shared_forward++;
    if ( succ1[-gen2] == -gen1 )
        shared_backward++;
    gen1first = gen1;
    /* all middle adjacencies */
    for ( i = 2; i < num_genes; i++ )
    {
        gen1 = gen2;
        gen2 = g2->genes[i];
#ifdef DEBUG
        if ( ( gen1 < -num_genes ) || ( gen1 > num_genes ) ||
             ( gen2 < -num_genes ) || ( gen2 > num_genes ) )
        {
            fprintf ( outfile, "error4 in greedy_median:gen1=%3d, gen2=%3d\n",
                      gen1, gen2 );
            exit ( -1 );
        }
#endif
        succ2f[gen1] = gen2;
        succ2b[-gen2] = -gen1;  /* set up both arrays -- we'll pick one later */
        if ( succ1[gen1] == gen2 )
            shared_forward++;
        if ( succ1[-gen2] == -gen1 )
            shared_backward++;
    }
    /* last adjacency */
    gen1 = gen2;
    gen2 = gen1first;
    succ2f[gen1] = gen2;
    succ2b[-gen2] = -gen1;      /* set up both arrays -- we'll pick one later */
    if ( succ1[gen1] == gen2 )
        shared_forward++;
    if ( succ1[-gen2] == -gen1 )
        shared_backward++;
#ifdef DEBUG
    fprintf ( outfile, "second genome processed\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "succ2f[%3d]=%3d, succ2b[%3d]=%3d\n", i, succ2f[i],
                  i, succ2b[i] );
#endif
#ifdef VERYVERBOSE
    fprintf ( outfile, "genome 1 forward is:\n" );
    current = -num_genes;
    while ( succ1[current] == 0 )
        current++;
    for ( i = 0; i <= num_genes; i++ )
    {
        fprintf ( outfile, "%3d ,", current );
        current = succ1[current];
    }
    fprintf ( outfile, "\n" );
    fflush ( outfile );
    fprintf ( outfile, "genome 2 forward is:\n" );
    current = -num_genes;
    while ( succ2f[current] == 0 )
        current++;
    for ( i = 0; i <= num_genes; i++ )
    {
        fprintf ( outfile, "%3d ,", current );
        current = succ2f[current];
    }
    fprintf ( outfile, "\n" );
    fflush ( outfile );
    fprintf ( outfile, "genome 2 backward is:\n" );
    current = -num_genes;
    while ( succ2b[current] == 0 )
        current++;
    for ( i = 0; i <= num_genes; i++ )
    {
        fprintf ( outfile, "%3d ,", current );
        current = succ2b[current];
    }
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif
#ifdef DEBUG
    fprintf ( outfile, "shared_forward=%3d, shared_backward=%3d\n",
              shared_forward, shared_backward );
    fflush ( outfile );
#endif
    if ( shared_forward >= shared_backward )
    {
        succ2 = succ2f;
    }
    else
    {
        succ2 = succ2b;
    }
    /* we know which array to use, but it's still missing usability flags
       as well as pred pointers */
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        if ( succ2[i] != 0 )
            degree[i] = 0;      /* flag usable */
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "in greedy_median, done processing genomes\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile,
                  "i=%3d, succ1=%3d, pred1=%3d, succ2=%3d, pred2=%3d, degree=%3d\n",
                  i, succ1[i], pred1[i], succ2[i], pred2[i], degree[i] );
    }
    fflush ( outfile );
#endif

    /* start recording the new genome */
    /* it includes all adjacencies shared by genomes 1 & 2 */

    /* count # adjacencies unique to each genome and overall */
    count = count1 = count2 = 0;

    /* first get shared edges and place them all in; also set up pred2 */
    for ( i = -num_genes; i <= num_genes; i++ )
    {

        if ( succ2[i] != 0 )
            pred2[succ2[i]] = i;    /* finish setting up second array */

        if ( ( tour[i] == 0 ) && ( degree[i] >= 0 ) )
        {                       /* not yet set up and avail */
            if ( succ1[i] == succ2[i] )
            {                   /* shared adjacency, always safe */
                /* select edge i -> succ1[i] */
                tour[i] = ind = succ1[i];
                count++;
#ifdef DEBUG
                fprintf ( outfile, "selecting common edge (%3d,%3d):\n", i,
                          tour[i] );
                fflush ( outfile );
#endif
                /* handle origin */
                degree[i] += 1; /* add outgoing edge */
                degree[-i] = -1;    /* flag complement unusable */
                /* handle destination */
                degree[ind] += 2;   /* add incoming edge */
                degree[-ind] = -1;  /* flag complement unusable */
                /* update segments */
                otherEndi = otherEnd[i];
                otherEndind = otherEnd[ind];
                otherEnd[otherEndi] = otherEndind;
                otherEnd[otherEndind] = otherEndi;
            }
        }
    }                           /* end for */
#ifdef VERYVERBOSE
    fprintf ( outfile,
              "in greedy_median, done processing common adjacencies\n" );
    fprintf ( outfile, "count=%3d\n", count );
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile,
                  "i=%3d, succ1=%3d, pred1=%3d, succ2=%3d, pred2=%3d, degree=%3d, otherEnd=%3d\n",
                  i, succ1[i], pred1[i], succ2[i], pred2[i], degree[i],
                  otherEnd[i] );
    }
    fflush ( outfile );
#endif
    /* All common adjacencies have been forced in;
       now attempt to allocate unshared adjacencies evenly */
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        if ( ( degree[i] == 0 ) || ( degree[i] == 2 ) )
        {                       /* needs an outgoing edge */
            i1 = succ1[i];
            i2 = succ2[i];
#ifdef DEBUG
            fprintf ( outfile,
                      "in greedy_median, i=%3d needs outgoing edge, succ1[i]=%3d, succ2[i]=%3d, degree[i]=%3d, degree[succ1]=%3d, degree[succ2]=%3d\n",
                      i, i1, i2, degree[i], degree[i1], degree[i2] );
            fflush ( outfile );
#endif
            if ( ( ( degree[i1] == 0 ) || ( degree[i1] == 1 ) ) &&  /* i1 can receive */
                 ( ( otherEnd[i] != i1 ) || ( count == num_genes - 1 ) ) && /* no short loop */
                 ( ( count1 <= count2 ) || ( degree[i2] == -1 )
                   || ( degree[i2] >= 2 ) )
                 /* preferred (to balance counts) or forced (i2 not avail) */
                 )
            {                   /* use edge i->i1 */
                count1 += finish1 ( i, &count, degree, succ1, succ2,
                                    pred1, pred2, tour, otherEnd, num_genes );
            }
            else
            {
                if ( ( ( degree[i2] == 0 ) || ( degree[i2] == 1 ) ) &&  /* i2 can receive */
                     ( ( otherEnd[i] != i2 ) || ( count == num_genes - 1 ) )    /* no short loop */
                     )
                {               /* use edge i->i2 */
                    count2 += finish2 ( i, &count, degree, succ1, succ2,
                                        pred1, pred2, tour, otherEnd,
                                        num_genes );
                }
                /* implicit else: cannot find a valid adjacency here --
                   will fill in at random later */
            }
        }
    }                           /* end for */
#ifdef DEBUG
    fprintf ( outfile,
              "done processing unique adjacencies, count=%3d, count1=%3d, count2=%3d\n",
              count, count1, count2 );
    fflush ( outfile );
#endif

    /* We may only have a partial tour of a median genome at this point;
       if so, we fill in at random */
    if ( count < num_genes )
    {
#ifdef DEBUG
        fprintf ( outfile, "finish median with random choices\n" );
        fflush ( outfile );
#endif
        /* for efficiency, create two stacks: one of genes without
           any adjacency in either direction and one of genes with an incoming
           edge */
        /* we can reuse arrays succ and pred now -- we're done with them --
           so we'll use succ1 for the genes without adjacency, pred1
           for the incoming edges */
        topnone = topin = -num_genes;   /* initialize 2 stacks */
        stacknone = succ1;
        stackin = pred1;
        /* collect vertices of degree 0 and vertices of indegree 1 */
        for ( i = -num_genes; i <= num_genes; i++ )
        {
            switch ( degree[i] )
            {
                case -1:
                case 0:
                    /* compute index of reverse gene, but only do it once */
                    if ( i < 0 )
                    {
                        if ( degree[-i] <= 0 )
                        {
#ifdef VERYVERBOSE
                            fprintf ( outfile,
                                      "added isolated vertex %3d to stack\n",
                                      i );
#endif
                            stacknone[topnone] = i;
                            /* could randomize here a choice of i or i+num_genes */
                            topnone++;
                        }
                    }
                    break;
                case 1:        /* let case 2 collect the pieces -- no need for both ends */
                    break;
                case 2:
                    stackin[topin] = i;
                    topin++;
#ifdef VERYVERBOSE
                    fprintf ( outfile, "added degree-1 vertex %3d to stack\n",
                              i );
#endif
                    break;
                case 3:
                    break;      /* nothing to do */
            }
        }
        /* handle vertices of degree 0, if any */
        if ( topnone > -num_genes )
        {                       /* there are isolated vertices */
            /* Form a single chain of all nodes on none stack */
            topnone--;
            first = current = stacknone[topnone];
            while ( topnone > -num_genes )
            {
                topnone--;
                ind = stacknone[topnone];
                tour[current] = ind;
#ifdef DEBUG
                if ( ( current < -num_genes ) || ( current > num_genes )
                     || ( ind < -num_genes ) || ( ind > -num_genes )
                     || ( current == 0 ) )
                {
                    fprintf ( outfile,
                              "error6 in greedy_median; current=%3d, ind=%3d\n",
                              current, ind );
                    exit ( -1 );
                }
#endif
#ifdef DEBUG
                count++;
#endif
#ifdef VERYVERBOSE
                fprintf ( outfile, "added isolated vertex %3d to chain\n",
                          ind );
#endif
                current = tour[current];
            }                   /* end while -- going through the stack */
#ifdef VERYVERBOSE
            fprintf ( outfile, "chain starts at %3d and ends at %3d\n", first,
                      current );
#endif
            otherEnd[first] = current;
            otherEnd[current] = first;
            /* Prepend list from first to current to first segment on stack */
            /* Such a segment must exist -- not possible to have only isolated
               vertices */
            ind = stackin[topin - 1];
            otherEndind = otherEnd[ind];    /* front of segment */
            tour[current] = otherEndind;
#ifdef DEBUG
            count++;
#endif
            otherEnd[first] = ind;
            otherEnd[ind] = first;
        }                       /* end if -- handling isolated vertices */
#ifdef VERYVERBOSE
        fprintf ( outfile,
                  "in greedy_median, done processing single nodes\n" );
        fprintf ( outfile, "count=%3d\n", count );
        for ( i = -num_genes; i <= num_genes; i++ )
        {
            fprintf ( outfile, "i=%3d, tour=%3d, degree=%3d, otherEnd=%3d\n",
                      i, tour[i], degree[i], otherEnd[i] );
        }
        fflush ( outfile );
#endif

        /* Now pair up a node with incoming edge to a node with outgoing edge */
        while ( topin > 1 - num_genes )
        {                       /* at least two segments on the stack */
            topin--;
            i1 = stackin[topin];
            i2 = stackin[topin - 1];
            tour[i1] = otherEnd[i2];
#ifdef DEBUG
            count++;
#endif
            otherEndi = otherEnd[i1];
            otherEnd[otherEndi] = i2;
            otherEnd[i2] = otherEndi;
        }
        /* close the tour  */
        /* note: ok to assume stack not empty, since count was too low */
#ifdef DEBUG
        fprintf ( outfile, "topin=%3d\n", topin );
        fflush ( outfile );
#endif
        topin--;
        tour[stackin[topin]] = otherEnd[stackin[topin]];
#ifdef DEBUG
        count++;
#endif
    }                           /* end if count too low */
#ifdef VERBOSE
    fprintf ( outfile, "in greedy_median, should have a tour...\n" );
    fprintf ( outfile, "count=%3d\n", count );
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile, "i=%3d, tour=%3d, degree=%3d, otherEnd=%3d\n",
                  i, tour[i], degree[i], otherEnd[i] );
    }
    fflush ( outfile );
#endif

    /* We now have a full tour of a median genome, so write gene[] array  */
#ifdef DEBUG
    fprintf ( outfile, "median returned is\n" );
    fflush ( outfile );
#endif
    current = -num_genes;
    while ( degree[current] != 3 )
        current++;
    for ( i = 0; i < num_genes; i++ )
    {
        gene[i] = current;
        current = tour[current];
#ifdef DEBUG
        fprintf ( outfile, ", %3d", gene[i] );
        fflush ( outfile );
#endif
    }
#ifdef DEBUG
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif

    return;
}
