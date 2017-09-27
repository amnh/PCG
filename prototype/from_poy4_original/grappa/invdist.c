#include <stdio.h>
#include <stdlib.h>
#include "invdist.h"
#include "structs.h"

#ifdef TESTING
double time_linear, time_BH;
#endif

void
calc_invmatrix ( struct genome_struct *genomes, int num_genes,
                 int num_genomes, distmem_t * distmem, int CIRCULAR )
{
    int i, j;
    int dist;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                dist =
                    invdist_circular ( genomes + i, genomes + j, num_genes,
                                       distmem );
            else
                dist =
                    invdist_noncircular ( genomes + i, genomes + j, 0,
                                          num_genes, distmem );
        }
    }

    return;
}

void
setinvmatrix ( int **distmatrix, struct genome_struct *genomes,
               int num_genes, int num_genomes, distmem_t * distmem,
               int CIRCULAR )
{
    int i, j;

    for ( i = 0; i < num_genomes; i++ )
    {
        distmatrix[i][i] = 0;
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                distmatrix[i][j] = distmatrix[j][i] =
                    invdist_circular ( genomes + i, genomes + j, num_genes,
                                       distmem );
            else
                distmatrix[i][j] = distmatrix[j][i] =
                    invdist_noncircular ( genomes + i, genomes + j, 0,
                                          num_genes, distmem );
        }
    }
    return;
}

void
calc_invmatrix_BH ( struct genome_struct *genomes, int num_genes,
                    int num_genomes, distmem_t * distmem, int CIRCULAR )
{
    int i, j;
    int dist;

    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( CIRCULAR )
                dist =
                    invdist_circular_BH ( genomes + i, genomes + j, num_genes,
                                          distmem );
            else
                dist =
                    invdist_noncircular_BH ( genomes + i, genomes + j, 0,
                                             num_genes, distmem );
        }
    }

    return;
}

int
calculate_offset ( struct genome_struct *g1, struct genome_struct *g2,
                   int num_genes )
{
    int i;
    int gA;
    int *genes2;

#ifdef DEBUG
    for ( i = 0; i < num_genes; i++ )
    {
        fprintf ( outfile, "calc_offset: g1[%3d]: %3d  g2[%3d]: %3d\n",
                  i, g1->genes[i], i, g2->genes[i] );
    }
#endif

    genes2 = g2->genes;

    gA = g1->genes[0];

    for ( i = 0; i < num_genes; i++ )
    {
        if ( genes2[i] == gA )
            return ( i );
        if ( genes2[i] == -gA )
            return ( i + num_genes );
    }

    fprintf ( stderr, "ERROR: calc_offset() could not locate gene %d\n", gA );

    return ( -1 );
}

int
num_breakpoints ( int *perm, int size )
{
    int i, b;
    int pA, pA1;
    int pB, pB1;

    b = 0;
    if ( perm[1] != 1 )
        b++;
    for ( i = 2; i < size - 1; i += 2 )
    {
        pA = perm[i];
        pA1 = ( pA == size ? 1 : pA + 1 );
        pB = perm[i + 1];
        pB1 = ( pB == size ? 1 : pB + 1 );
        if ( ( pB != pA1 ) && ( pA != pB1 ) )
            b++;
    }

    return ( b );
}

int
num_cycles ( int *perm, int size, distmem_t * distmem )
{
    int *done;
    int i, ind, j1, j2;
    int next;
    int c;
    int *greyEdges;
    int *invperm;
    int *cycle;

    done = distmem->done;
    greyEdges = distmem->greyEdges;
    invperm = distmem->done;
    cycle = distmem->labeled;

    c = 0;

#if 0
    /* This is now implicit */
    /* set black -edges */
    for ( i = 0; i < size; i += 2 )
    {
        blackEdges[i] = i + 1;
        blackEdges[i + 1] = i;
    }
#endif

    /* set grey -edges */
    for ( i = 0; i < size; i++ )
    {
        invperm[perm[i]] = i;
        greyEdges[i] = -1;
    }

    j1 = invperm[1];
    if ( j1 != 1 )
        greyEdges[0] = j1;

    for ( i = 1; i < size - 1; i += 2 )
    {
        ind = perm[i];
        if ( ind < perm[i + 1] )
        {
            j1 = invperm[ind - 1];
            j2 = invperm[ind + 2];
        }
        else
        {
            j1 = invperm[ind + 1];
            j2 = invperm[ind - 2];
        }
        if ( j1 != i - 1 )
        {
            greyEdges[i] = j1;
        }
        if ( j2 != i + 2 )
        {
            greyEdges[i + 1] = j2;
        }
    }

    j1 = invperm[size - 2];
    if ( j1 != size - 2 )
        greyEdges[size - 1] = j1;

#ifdef DEBUG
    for ( i = 0; i < size; i++ )
    {
        fprintf ( stdout, "i: %3d Grey edge: (%3d, %3d)\n", perm[i],
                  perm[i], greyEdges[i] == -1 ? -1 : perm[greyEdges[i]] );
    }
#endif

    for ( i = 0; i < size; i++ )
        done[i] = 0;

    for ( i = 0; i < size; i++ )
    {
        if ( done[i] == 0 && greyEdges[i] != -1 )
        {
            cycle[i] = i;
            done[i] = 1;
            next = i;
            do
            {
                if ( next % 2 == 0 )
                    next++;
                else
                    next--;
                done[next] = 1;
                cycle[next] = i;
                next = greyEdges[next];
                done[next] = 1;
                cycle[next] = i;
            }
            while ( next != i );
            c++;
        }
    }

    return ( c );
}

int
num_cycles_BH ( int *perm, int size, distmem_t * distmem )
{
    int *done;
    int i, ind, j1, j2;
    int next;
    int c;
    int *greyEdges;
    int *invperm;

    done = distmem->done;
    greyEdges = distmem->greyEdges;
    invperm = distmem->done;

    c = 0;

#if 0
    /* This is now implicit */
    /* set black -edges */
    for ( i = 0; i < size; i += 2 )
    {
        blackEdges[i] = i + 1;
        blackEdges[i + 1] = i;
    }
#endif

    /* set grey -edges */
    for ( i = 0; i < size; i++ )
    {
        invperm[perm[i]] = i;
        greyEdges[i] = -1;
    }

    j1 = invperm[1];
    if ( j1 != 1 )
        greyEdges[0] = j1;

    for ( i = 1; i < size - 1; i += 2 )
    {
        ind = perm[i];
        if ( ind < perm[i + 1] )
        {
            j1 = invperm[ind - 1];
            j2 = invperm[ind + 2];
        }
        else
        {
            j1 = invperm[ind + 1];
            j2 = invperm[ind - 2];
        }
        if ( j1 != i - 1 )
        {
            greyEdges[i] = j1;
        }
        if ( j2 != i + 2 )
        {
            greyEdges[i + 1] = j2;
        }
    }

    j1 = invperm[size - 2];
    if ( j1 != size - 2 )
        greyEdges[size - 1] = j1;

#ifdef DEBUG
    for ( i = 0; i < size; i++ )
    {
        fprintf ( stdout, "i: %3d Grey edge: (%3d, %3d)\n", perm[i],
                  perm[i], greyEdges[i] == -1 ? -1 : perm[greyEdges[i]] );
    }
#endif

    for ( i = 0; i < size; i++ )
        done[i] = 0;

    for ( i = 0; i < size; i++ )
    {
        if ( done[i] == 0 && greyEdges[i] != -1 )
        {
            done[i] = 1;
            next = i;
            do
            {
                if ( next % 2 == 0 )
                    next++;
                else
                    next--;
                done[next] = 1;
                next = greyEdges[next];
                done[next] = 1;
            }
            while ( next != i );
            c++;
        }
    }

    return ( c );
}

void
connected_component ( int size, distmem_t * distmem, int *num_components )
{
    int i;
    int stack_ptr;
    int *greyEdges;
    int *cycle, *range, *cc;
    component_t *components;
    int *stack_root, *stack_range;
    int *parent, *next;
    int right, p;
#ifdef TESTING
    struct timeval tm;
    double time0, time1;
#endif

#ifdef TESTING
    gettimeofday ( &tm, NULL );
    time0 = ( double ) tm.tv_sec + ( double ) tm.tv_usec / 1000000.0;
#endif

    greyEdges = distmem->greyEdges;

    next = stack_root = distmem->stack;
    range = distmem->oriented;
    stack_range = cc = distmem->cc;
    parent = cycle = distmem->labeled;
    components = distmem->components;
    stack_ptr = -1;
    *num_components = 0;

    /* Use Linear algorithm to compute connected component */
    for ( i = 0; i < size; i++ )
    {
        if ( greyEdges[i] == -1 )
            continue;
        range[cycle[i]] = i;
    }

    for ( i = 0; i < size; i++ )
    {
        if ( greyEdges[i] == -1 )
            continue;           /*it is self loop,discard it */
        if ( parent[i] == i )
        {
            stack_ptr++;
            stack_root[stack_ptr] = i;
            stack_range[stack_ptr] = range[i];
        }
        else
        {                       /*check the top of stack for intersection */
            right = i;
#ifdef DEBUG
            fprintf ( stdout, "top:%d,rang:%d,i:%d,parent[i]:%d\n",
                      stack_root[stack_ptr], stack_range[stack_ptr], i,
                      parent[i] );
            fflush ( stdout );
#endif
            while ( stack_root[stack_ptr] > parent[i] )
            {
                /*union top to the i's connected component */
                parent[stack_root[stack_ptr]] = parent[i];
                if ( right < stack_range[stack_ptr] )
                    right = stack_range[stack_ptr]; /*extend the active range */
                stack_ptr--;
            }
            if ( stack_range[stack_ptr] < right )
                stack_range[stack_ptr] = right;
            if ( stack_range[stack_ptr] <= i )
            {
                /*the top connected-component is INACTIVE */
                components[*num_components].index = stack_root[stack_ptr];
                ( *num_components )++;
                stack_ptr--;
            }
        }
    }
    /*turn the forest to set of linked lists whose list head is index of
       component */
    for ( i = 0; i < size; i++ )
        next[i] = -1;
    for ( i = 0; i < size; i++ )
    {
        if ( greyEdges[i] == -1 )
            cc[i] = -1;
        else if ( i != parent[i] )
        {
            /* insert i between parent(i) and next of parent(i) */
            next[i] = next[parent[i]];
            next[parent[i]] = i;
        }
    }

    /*label each node with its root */
    for ( i = 0; i < *num_components; i++ )
    {
        p = components[i].index;
        while ( p != -1 )
        {
            cc[p] = i;
            p = next[p];
        }
    }

#ifdef TESTING
    gettimeofday ( &tm, NULL );
    time1 = ( double ) tm.tv_sec + ( double ) tm.tv_usec / 1000000.0;
    time_linear += time1 - time0;
#endif
    return;
}

void
num_hurdles_and_fortress ( int *perm, int size,
                           int *num_hurdles, int *num_fortress,
                           distmem_t * distmem )
{
    int cIdx;
    int i, j;
    int *oriented, *cc, *labeled;
    component_t *components;
    int num_components;
    int num_oriented;
    int first_comp, last_comp, num_block;
    int num_superhurdles;
    int *greyEdges;

    /* By default, set number of hurdles and fortresses to 0 */
    *num_hurdles = 0;
    *num_fortress = 0;

    greyEdges = distmem->greyEdges;

    oriented = distmem->oriented;
    cc = distmem->cc;
    labeled = distmem->labeled;
    components = distmem->components;

    connected_component ( size, distmem, &num_components );


    if ( num_components == 0 )
    {
#ifdef DEBUG
        fprintf ( stdout, "No components -- quick exit\n" );
#endif
        return;
    }


    /* Calculate if each gray edge is oriented or unoriented */
    /* At the same time, label the connected component of a vertex as the
       index of its root */


    for ( i = 0; i < size; i++ )
    {
        j = greyEdges[i];
        if ( j == -1 )
        {
            oriented[i] = FALSE;
        }
        else
        {
            if ( i < j )
            {
                if ( ( j - i ) % 2 != 0 )
                {
                    oriented[i] = FALSE;
                    oriented[j] = FALSE;
                }
                else
                {
                    oriented[i] = TRUE;
                    oriented[j] = TRUE;
                }
            }
        }
    }

    /* Look for any vertices that are "labeled".
       These are the roots of the cnnected components.
       Record them in the "components" array,
       and set num_components.
     */


    /* If a component contains an oriented vertex then it is oriented.
       Otherwise, the component is unoriented. */

    for ( i = 0; i < num_components; i++ )
    {
        components[i].oriented = FALSE;
    }

    for ( i = 0; i < size; i++ )
    {
        if ( oriented[i] == 1 )
            components[cc[i]].oriented = TRUE;
    }


    /* Count nonoriented components */

    num_oriented = 0;
    for ( i = 0; i < num_components; i++ )
    {
        if ( components[i].oriented == FALSE )
        {
            num_oriented++;
        }
    }

    if ( num_oriented == 0 )
    {
#ifdef DEBUG
        fprintf ( stdout, "Quick exit -- no hurdles\n" );
#endif
        return;
    }

    for ( i = 0; i < num_components; i++ )
    {
        components[i].blocks = 0;
        components[i].hurdle = 0;
        components[i].left = -1;
        components[i].right = -1;
    }

    /* HURDLES 
       Hurdles are a subset of the nonoriented components. 
       There are two types of hurdles (in the KST-sense):
       "simple" and "superhurdle".
       First, we implicitly eliminate oriented components.
       Second, if a nonoriented component is one contiguous block of
       vertices, it is a hurdle.
       Third, if a hurdle "protects" the same non-hurdle on its left and
       right side, then it is a "superhurdle".
     */

    first_comp = -1;
    last_comp = -1;
    num_block = -1;
    for ( i = 0; i < size; i++ )
    {
        cIdx = cc[i];
        if ( cIdx != -1 )
        {
            if ( components[cIdx].oriented == FALSE )
            {
                if ( cIdx != last_comp )
                {
                    if ( last_comp == -1 )
                    {
                        first_comp = cIdx;
                    }
                    else
                    {
                        components[last_comp].right = cIdx;
                        components[cIdx].left = last_comp;
                    }
                    last_comp = cIdx;
                    num_block++;
                    components[cIdx].blocks++;
                }
            }
        }
    }

#ifdef DEBUG
    for ( i = 0; i < num_components; i++ )
    {
        if ( components[i].oriented == FALSE )
        {
            fprintf ( stdout, "nonoriented component %3d: blocks: %3d\n",
                      i, components[i].blocks );
        }
    }
#endif

    for ( i = 0; i < num_components; i++ )
    {
        if ( ( components[i].oriented == FALSE )
             && ( components[i].blocks == 1 ) )
        {
#ifdef DEBUG
            fprintf ( stdout, "nonoriented component %3d: blocks: %3d\n",
                      i, components[i].blocks );
#endif
            components[i].hurdle = HURDLE;
            ( *num_hurdles )++;
        }
    }
    if ( ( first_comp == last_comp )
         && ( components[first_comp].blocks == 2 ) )
    {
        components[first_comp].hurdle = ( HURDLE | GREATHURDLE );
        ( *num_hurdles )++;
    }

    if ( *num_hurdles < 3 )
        return;

    num_superhurdles = 0;
    for ( i = 0; i < num_components; i++ )
    {
        if ( components[i].hurdle )
        {
            if ( ( components[i].left == components[i].right ) &&
                 ( components[i].left != -1 ) )
            {
                if ( ( components[components[i].left].blocks == 2 ) &&
                     ( ( components[components[i].left].
                         hurdle & GREATHURDLE ) == 0 ) )
                {
#ifdef DEBUG
                    fprintf ( stdout, "Component %3d is a superhurdle \n",
                              i );
#endif
                    components[i].hurdle |= SUPERHURDLE;
                    num_superhurdles++;
                }
                else
                {
                    return;
                }
            }
            else
            {
                return;
            }
        }
    }

#ifdef DEBUG
    fprintf ( stdout, "Number of superhurdles: %3d\n", num_superhurdles );
#endif

    /* Set num_fortress if there are an odd number of hurdles,
       all of which are superhurdles. */
    if ( ( *num_hurdles == num_superhurdles )
         && ( num_superhurdles % 2 == 1 ) )
        *num_fortress = 1;

    return;
}

void
connected_component_BH ( int size, distmem_t * distmem, int *num_components )
{
    UFelem *uf;
    int *stack;
    int stackPtr;
    int cIdx;
    int i, j, v;
    int s, eB, eE;
    int *oriented, *cc, *labeled;
    component_t *components;
    int *greyEdges;

#ifdef TESTING
    struct timeval tm;
    double time0, time1;
#endif

#ifdef TESTING
    gettimeofday ( &tm, NULL );
    time0 = ( double ) tm.tv_sec + ( double ) tm.tv_usec / 1000000.0;
#endif

    greyEdges = distmem->greyEdges;

    uf = distmem->uf;

    UFcreate ( uf, size );

    stack = distmem->stack;
    oriented = distmem->oriented;
    cc = distmem->cc;
    labeled = distmem->labeled;
    components = distmem->components;
    stackPtr = 0;

    /* Use Berman & Hannenhalli to label the connected components */

    for ( i = 0; i < size; i++ )
    {
        /* if i is the beginning of an edge (i, j) */
        j = greyEdges[i];
        if ( j != -1 )
        {
            if ( i < j )
            {
                /* Create a new component with set (i,j) and handle (i,j) */
                uf[i].B = i;
                uf[i].E = j;
                uf[i].handleB = i;
                uf[i].handleE = j;

                /* push i onto the stack */
                stack[stackPtr++] = i;
            }
            else
            {
                /* if i is the end of an edge */
                /* This always has to be the case in the else loop */
                /* C <- Find(i) */
                cIdx = UFfind ( uf, j );

                /* s <- handle(C).B */
                s = uf[cIdx].handleB;
                /* While top >= s */
                while ( ( stackPtr > 0 ) && ( stack[stackPtr - 1] >= s ) )
                {
                    /* C <- Union(Find(pop), C) */
                    cIdx =
                        UFunion ( uf, UFfind ( uf, stack[stackPtr - 1] ),
                                  cIdx );
                    stackPtr--;
                }
                /* e <- handle(C) */
                eB = uf[cIdx].handleB;
                eE = uf[cIdx].handleE;
                /* if e.E > i */
                if ( eE > i )
                {               /* if C is active */
                    /* push(e.B) */
                    stack[stackPtr++] = eB;
                }
            }
        }
    }

#ifdef DEBUG
    for ( i = 0; i < size; i++ )
    {
        j = greyEdges[i];
        if ( j != -1 )
        {
            if ( i < j )
            {
                cIdx = UFfind ( uf, i );
                fprintf ( stdout,
                          "Grey Edge (%3d, %3d): component: %3d handle (%3d,%3d)\n",
                          perm[i], perm[j],
                          cIdx,
                          perm[uf[cIdx].handleB], perm[uf[cIdx].handleE] );
            }
        }
    }
#endif

    /* Calculate if each gray edge is oriented or unoriented */
    /* At the same time, label the connected component of a vertex as the
       index of its root */

    for ( i = 0; i < size; i++ )
        labeled[i] = -1;

    for ( i = 0; i < size; i++ )
    {
        j = greyEdges[i];
        if ( j == -1 )
        {
            cc[i] = -1;
        }
        else
        {
            if ( i < j )
            {
                v = cc[i] = cc[j] = UFfind ( uf, i );
                labeled[v] = 1;
            }
        }
    }

    /* Look for any vertices that are "labeled".
       These are the roots of the cnnected components.
       Record them in the "components" array,
       and set num_components.   
     */

    *num_components = 0;
    for ( i = 0; i < size; i++ )
    {
        if ( labeled[i] == 1 )
        {
            labeled[i] = *num_components;
            ( *num_components )++;
        }
    }
    /* Set the component label of each vertex to the label of its
       component root. */

    for ( i = 0; i < size; i++ )
    {
        if ( cc[i] != -1 )
            cc[i] = labeled[cc[i]];
    }

#ifdef TESTING
    gettimeofday ( &tm, NULL );
    time1 = ( double ) tm.tv_sec + ( double ) tm.tv_usec / 1000000.0;
    time_BH += time1 - time0;
#endif

    return;
}

void
num_hurdles_and_fortress_BH ( int *perm, int size,
                              int *num_hurdles, int *num_fortress,
                              distmem_t * distmem )
{
    int cIdx;
    int i, j;
    int *oriented, *cc;
    component_t *components;
    int num_components;
    int num_oriented;
    int first_comp, last_comp, num_block;
    int num_superhurdles;
    int *greyEdges;

    /* By default, set number of hurdles and fortresses to 0 */
    *num_hurdles = 0;
    *num_fortress = 0;

    greyEdges = distmem->greyEdges;

    oriented = distmem->oriented;
    cc = distmem->cc;
    components = distmem->components;

    connected_component_BH ( size, distmem, &num_components );

    for ( i = 0; i < size; i++ )
    {
        j = greyEdges[i];
        if ( j == -1 )
        {
            oriented[i] = FALSE;
        }
        else
        {
            if ( i < j )
            {
                if ( ( j - i ) % 2 != 0 )
                {
                    oriented[i] = FALSE;
                    oriented[j] = FALSE;
                }
                else
                {
                    oriented[i] = TRUE;
                    oriented[j] = TRUE;
                }
            }
        }
    }

    /* Look for any vertices that are "labeled".
       These are the roots of the cnnected components.
       Record them in the "components" array,
       and set num_components.
     */

    if ( num_components == 0 )
    {
#ifdef DEBUG
        fprintf ( stdout, "No components -- quick exit\n" );
#endif
        return;
    }


    /* If a component contains an oriented vertex then it is oriented.
       Otherwise, the component is unoriented. */

    for ( i = 0; i < num_components; i++ )
    {
        components[i].oriented = FALSE;
    }

    for ( i = 0; i < size; i++ )
    {
        if ( oriented[i] == 1 )
            components[cc[i]].oriented = TRUE;
    }

#ifdef DEBUG
    for ( i = 0; i < num_components; i++ )
    {
        fprintf ( stdout,
                  "Component: %3d: index: %3d handle: (%3d, %3d) Oriented: %1d\n",
                  i, v = components[i].index,
                  perm[uf[v].handleB], perm[uf[v].handleE],
                  components[i].oriented );
    }
#endif

    /* Count nonoriented components */

    num_oriented = 0;
    for ( i = 0; i < num_components; i++ )
    {
        if ( components[i].oriented == FALSE )
        {
            num_oriented++;
        }
    }

    if ( num_oriented == 0 )
    {
#ifdef DEBUG
        fprintf ( stdout, "Quick exit -- no hurdles\n" );
#endif
        return;
    }

    for ( i = 0; i < num_components; i++ )
    {
        components[i].blocks = 0;
        components[i].hurdle = 0;
        components[i].left = -1;
        components[i].right = -1;
    }

    /* HURDLES 
       Hurdles are a subset of the nonoriented components. 
       There are two types of hurdles (in the KST-sense):
       "simple" and "superhurdle".
       First, we implicitly eliminate oriented components.
       Second, if a nonoriented component is one contiguous block of
       vertices, it is a hurdle.
       Third, if a hurdle "protects" the same non-hurdle on its left and
       right side, then it is a "superhurdle".
     */

    first_comp = -1;
    last_comp = -1;
    num_block = -1;
    for ( i = 0; i < size; i++ )
    {
        cIdx = cc[i];
        if ( cIdx != -1 )
        {
            if ( components[cIdx].oriented == FALSE )
            {
                if ( cIdx != last_comp )
                {
                    if ( last_comp == -1 )
                    {
                        first_comp = cIdx;
                    }
                    else
                    {
                        components[last_comp].right = cIdx;
                        components[cIdx].left = last_comp;
                    }
                    last_comp = cIdx;
                    num_block++;
                    components[cIdx].blocks++;
                }
            }
        }
    }

#ifdef DEBUG
    for ( i = 0; i < num_components; i++ )
    {
        if ( components[i].oriented == FALSE )
        {
            fprintf ( stdout, "nonoriented component %3d: blocks: %3d\n",
                      i, components[i].blocks );
        }
    }
#endif

    for ( i = 0; i < num_components; i++ )
    {
        if ( ( components[i].oriented == FALSE )
             && ( components[i].blocks == 1 ) )
        {
#ifdef DEBUG
            fprintf ( stdout, "nonoriented component %3d: blocks: %3d\n",
                      i, components[i].blocks );
#endif
            components[i].hurdle = HURDLE;
            ( *num_hurdles )++;
        }
    }
    if ( ( first_comp == last_comp )
         && ( components[first_comp].blocks == 2 ) )
    {
        components[first_comp].hurdle = ( HURDLE | GREATHURDLE );
        ( *num_hurdles )++;
    }

    num_superhurdles = 0;
    for ( i = 0; i < num_components; i++ )
    {
        if ( components[i].hurdle )
        {
            if ( ( components[i].left == components[i].right ) &&
                 ( components[components[i].left].blocks == 2 ) &&
                 ( ( components[components[i].left].hurdle & GREATHURDLE ) ==
                   0 ) )
            {
#ifdef DEBUG
                fprintf ( stdout, "Component %3d is a superhurdle \n", i );
#endif
                components[i].hurdle |= SUPERHURDLE;
                num_superhurdles++;
            }
            else
            {
                return;
            }
        }
    }

#ifdef DEBUG
    fprintf ( stdout, "Number of superhurdles: %3d\n", num_superhurdles );
#endif

    /* Set num_fortress if there are an odd number of hurdles,
       all of which are superhurdles. */
    if ( ( *num_hurdles == num_superhurdles )
         && ( num_superhurdles % 2 == 1 ) )
        *num_fortress = 1;

    return;
}

int
invdist_noncircular ( struct genome_struct *g1, struct genome_struct *g2,
                      int offset, int num_genes, distmem_t * distmem )
{
    int i, twoi, n;
    int b, c;
    int g;
    int *perm1, *perm2, *perm;
    int num_hurdles;
    int num_fortress;
    int reversal_dist;

#ifdef DEBUG
    fprintf ( outfile, "invdist_noncircular: offset: %3d\n", offset );
#endif

    n = 2 * num_genes + 2;

    perm1 = distmem->perm1;
    perm2 = distmem->perm2;
    perm = distmem->perm;

    for ( i = 0; i < num_genes; i++ )
    {
        g = 2 * g1->genes[i];
        twoi = 2 * i + 1;
        if ( g > 0 )
        {
            perm1[g - 1] = twoi;
            perm1[g] = twoi + 1;
        }
        else
        {
            perm1[-g] = twoi;
            perm1[-g - 1] = twoi + 1;
        }
    }
#ifdef DEBUG
    fprintf ( outfile, "G1: " );
    for ( i = 0; i < num_genes; i++ )
    {
        fprintf ( outfile, "%4d ", g1->genes[i] );
    }
    fprintf ( outfile, "\n" );

    fprintf ( outfile, "P1: " );
    for ( i = 1; i <= 2 * num_genes; i++ )
    {
        fprintf ( outfile, "%4d ", perm1[i] );
    }
    fprintf ( outfile, "\n" );
#endif

    for ( i = 0; i < num_genes; i++ )
    {
        if ( offset < num_genes )
            g = g2->genes[( offset + i ) % num_genes];
        else
        {
            g = -g2->genes[( offset - i ) % num_genes];
        }
        twoi = 2 * i + 1;
        if ( g > 0 )
        {
            perm2[twoi] = 2 * g - 1;
            perm2[twoi + 1] = 2 * g;
        }
        else
        {
            perm2[twoi] = -2 * g;
            perm2[twoi + 1] = -2 * g - 1;
        }
    }

#ifdef DEBUG
    fprintf ( outfile, "G2: " );
    for ( i = 0; i < num_genes; i++ )
    {
        fprintf ( outfile, "%4d ",
                  offset < num_genes ?
                  g2->genes[( offset + i ) % num_genes] :
                  -g2->genes[( offset - i ) % num_genes] );
    }
    fprintf ( outfile, "\n" );

    fprintf ( outfile, "P2: " );
    for ( i = 1; i <= 2 * num_genes; i++ )
    {
        fprintf ( outfile, "%4d ", perm2[i] );
    }
    fprintf ( outfile, "\n" );
#endif

    perm[0] = 0;
    for ( i = 1; i < n - 1; i++ )
    {
        perm[i] = perm1[perm2[i]];
    }
    perm[n - 1] = n - 1;

#ifdef DEBUG
    fprintf ( outfile, " P: " );
    for ( i = 0; i < n; i++ )
    {
        fprintf ( outfile, "%4d ", perm[i] );
    }
    fprintf ( outfile, "\n" );
#endif

    b = num_breakpoints ( perm, n );
    c = num_cycles ( perm, n, distmem );
    num_hurdles_and_fortress ( perm, n, &num_hurdles, &num_fortress,
                               distmem );
    reversal_dist = b - c + num_hurdles + num_fortress;

#ifdef DEBUG
    fprintf ( outfile, "Number of breakpoints: %d\n", b );
    fprintf ( outfile, "Number of cycles:      %d\n", c );
    fprintf ( outfile, "Number of hurdles:     %d\n", num_hurdles );
    fprintf ( outfile, "Fortress:              %d\n", num_fortress );
    fprintf ( outfile, "REVERSAL DISTANCE: %3d\n", reversal_dist );
#endif

    return ( reversal_dist );
}

int
invdist_noncircular_BH ( struct genome_struct *g1, struct genome_struct *g2,
                         int offset, int num_genes, distmem_t * distmem )
{
    int i, twoi, n;
    int b, c;
    int g;
    int *perm1, *perm2, *perm;
    int num_hurdles;
    int num_fortress;
    int reversal_dist;

#ifdef DEBUG
    fprintf ( outfile, "invdist_noncircular_BH: offset: %3d\n", offset );
#endif

    n = 2 * num_genes + 2;

    perm1 = distmem->perm1;
    perm2 = distmem->perm2;
    perm = distmem->perm;

    for ( i = 0; i < num_genes; i++ )
    {
        g = 2 * g1->genes[i];
        twoi = 2 * i + 1;
        if ( g > 0 )
        {
            perm1[g - 1] = twoi;
            perm1[g] = twoi + 1;
        }
        else
        {
            perm1[-g] = twoi;
            perm1[-g - 1] = twoi + 1;
        }
    }
#ifdef DEBUG
    fprintf ( outfile, "G1: " );
    for ( i = 0; i < num_genes; i++ )
    {
        fprintf ( outfile, "%4d ", g1->genes[i] );
    }
    fprintf ( outfile, "\n" );

    fprintf ( outfile, "P1: " );
    for ( i = 0; i < n; i++ )
    {
        fprintf ( outfile, "%4d ", perm1[i] );
    }
    fprintf ( outfile, "\n" );
#endif

    for ( i = 0; i < num_genes; i++ )
    {
        if ( offset < num_genes )
            g = g2->genes[( offset + i ) % num_genes];
        else
        {
            g = -g2->genes[( offset - i ) % num_genes];
        }
        twoi = 2 * i + 1;
        if ( g > 0 )
        {
            perm2[twoi] = 2 * g - 1;
            perm2[twoi + 1] = 2 * g;
        }
        else
        {
            perm2[twoi] = -2 * g;
            perm2[twoi + 1] = -2 * g - 1;
        }
    }

#ifdef DEBUG
    fprintf ( outfile, "G2: " );
    for ( i = 0; i < num_genes; i++ )
    {
        fprintf ( outfile, "%4d ",
                  offset < num_genes ?
                  g2->genes[( offset + i ) % num_genes] :
                  -g2->genes[( offset - i ) % num_genes] );
    }
    fprintf ( outfile, "\n" );

    fprintf ( outfile, "P2: " );
    for ( i = 0; i < n; i++ )
    {
        fprintf ( outfile, "%4d ", perm2[i] );
    }
    fprintf ( outfile, "\n" );
#endif

    perm[0] = 0;
    for ( i = 1; i < n - 1; i++ )
    {
        perm[i] = perm1[perm2[i]];
    }
    perm[n - 1] = n - 1;

#ifdef DEBUG
    fprintf ( outfile, " P: " );
    for ( i = 0; i < n; i++ )
    {
        fprintf ( outfile, "%4d ", perm[i] );
    }
    fprintf ( outfile, "\n" );
#endif

    b = num_breakpoints ( perm, n );
    c = num_cycles_BH ( perm, n, distmem );
    num_hurdles_and_fortress_BH ( perm, n, &num_hurdles, &num_fortress,
                                  distmem );
    reversal_dist = b - c + num_hurdles + num_fortress;

#ifdef DEBUG
    fprintf ( outfile, "Number of breakpoints: %d\n", b );
    fprintf ( outfile, "Number of cycles:      %d\n", c );
    fprintf ( outfile, "Number of hurdles:     %d\n", num_hurdles );
    fprintf ( outfile, "Fortress:              %d\n", num_fortress );
    fprintf ( outfile, "REVERSAL DISTANCE: %3d\n", reversal_dist );
#endif

    return ( reversal_dist );
}

int
invdist_circular ( struct genome_struct *g1, struct genome_struct *g2,
                   int num_genes, distmem_t * distmem )
{

    int offset;

    offset = calculate_offset ( g1, g2, num_genes );

    return ( invdist_noncircular ( g1, g2, offset, num_genes, distmem ) );
}

int
invdist_circular_BH ( struct genome_struct *g1, struct genome_struct *g2,
                      int num_genes, distmem_t * distmem )
{

    int offset;

    offset = calculate_offset ( g1, g2, num_genes );

    return ( invdist_noncircular_BH ( g1, g2, offset, num_genes, distmem ) );
}

int
invdist_noncircular_nomem ( struct genome_struct *g1,
                            struct genome_struct *g2,
                            int offset, int num_genes )
{
    distmem_t distmem;
    int dist;

    distmem.perm1 =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.perm1 == ( int * ) NULL )
        fprintf ( stderr, "ERROR: perm1 NULL\n" );
    distmem.perm2 =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.perm2 == ( int * ) NULL )
        fprintf ( stderr, "ERROR: perm2 NULL\n" );
    distmem.perm =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.perm == ( int * ) NULL )
        fprintf ( stderr, "ERROR: perm NULL\n" );
    distmem.done =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.done == ( int * ) NULL )
        fprintf ( stderr, "ERROR: done NULL\n" );
    distmem.greyEdges =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.greyEdges == ( int * ) NULL )
        fprintf ( stderr, "ERROR: greyEdges NULL\n" );

    distmem.stack =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.stack == ( int * ) NULL )
        fprintf ( stderr, "ERROR: stack NULL\n" );
    distmem.oriented =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.oriented == ( int * ) NULL )
        fprintf ( stderr, "ERROR: oriented NULL\n" );
    distmem.cc = ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.cc == ( int * ) NULL )
        fprintf ( stderr, "ERROR: cc NULL\n" );
    distmem.labeled =
        ( int * ) malloc ( ( 2 * num_genes + 2 ) * sizeof ( int ) );
    if ( distmem.labeled == ( int * ) NULL )
        fprintf ( stderr, "ERROR: labeled NULL\n" );
    distmem.components = ( component_t * )
        malloc ( ( 2 * num_genes + 2 ) * sizeof ( component_t ) );
    if ( distmem.components == ( component_t * ) NULL )
        fprintf ( stderr, "ERROR: components NULL\n" );


    dist = invdist_noncircular ( g1, g2, offset, num_genes, &distmem );


    free ( distmem.components );
    free ( distmem.labeled );
    free ( distmem.cc );
    free ( distmem.oriented );
    free ( distmem.stack );

    free ( distmem.greyEdges );
    free ( distmem.done );
    free ( distmem.perm1 );
    free ( distmem.perm2 );
    free ( distmem.perm );

    return ( dist );
}

int
invdist_circular_nomem ( struct genome_struct *g1, struct genome_struct *g2,
                         int num_genes )
{
    int offset;

    offset = calculate_offset ( g1, g2, num_genes );

    return ( invdist_noncircular_nomem ( g1, g2, offset, num_genes ) );
}
