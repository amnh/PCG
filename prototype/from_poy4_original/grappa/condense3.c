#include "structs.h"
#include "condense3.h"

void
condense3 ( int *ingene1, int *ingene2, int *ingene3,
            int *outgene1, int *outgene2, int *outgene3,
            int num_genes, int *num_cond, int *succ, int *pred,
            int *code, int *decode )
{
    /* uses arrays succ and pred to test where the three input genomes
       have common adjacencies; then produces condensed versions of the
       three genomes and passes them back along with condensed size */
    /* note: these arrays must be passed with a pointer to the middle
       of the array, because we'll use both negative and positive
       offsets! location 0 will not be used, so that these arrays
       need to be of size 2*num_genes + 1 */
    int i, ind, index, lastgen, gen1, gen2, gen1first, newcode;
#ifdef VERYVERBOSE
    fprintf ( outfile, "Entering condense3\n" );
    fflush ( outfile );
#endif

    /* reset entries */
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        succ[i] = pred[i] = BREAK;
    }
    succ[0] = pred[0] = 0;
    /* process each of the three genomes in turn */
    /* first genome simply records its adjacencies, but following
       ones erase any they don't agree with -- but we have to
       make allowances for reverse adjacencies, i.e., we have
       to consider that (a,b) agrees with (-b,-a) */
    /* all loops are unrolled for efficiency */

    /* first genome */
#ifdef VERYVERBOSE
    fprintf ( outfile, "in condense 3, first genome is:\n" );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, "%3d, ", ingene1[i] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif
    /* first adjacency */
    gen1 = ingene1[0];
    gen2 = ingene1[1];
    succ[gen1] = gen2;
    pred[gen2] = gen1;
    pred[-gen1] = -gen2;
    succ[-gen2] = -gen1;
    gen1first = gen1;
    /* all middle adjacencies */
    for ( i = 2; i < num_genes; i++ )
    {
        gen1 = gen2;
        gen2 = ingene1[i];
        succ[gen1] = gen2;
        pred[gen2] = gen1;
        pred[-gen1] = -gen2;
        succ[-gen2] = -gen1;
    }
    /* last adjacency */
    succ[gen2] = gen1first;
    pred[gen1first] = gen2;
    pred[-gen2] = -gen1first;
    succ[-gen1first] = -gen2;
#ifdef VERYVERBOSE
    fprintf ( outfile, "In condense3, done scanning genome 1\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "i=%3d, pred=%3d, succ=%3d\n", i, pred[i],
                  succ[i] );
    fflush ( outfile );
#endif

    *num_cond = 0;              /* if the entire chain is common, we'll have a single block */
    /* note: only for circular! */
    /* for linear, should be initialized to 1 */

    /* second genome */
    /* given the situation
       lastgen -> gen1 -> gen2
       we need to check and perhaps set four adjacencies:
       pred[gen1] =? lastgen, succ[gen1] =? gen2,
       succ[-gen1] =? -lastgen, pred[-gen1] =? -gen2 */
    /* first adjacency */
#ifdef VERYVERBOSE
    fprintf ( outfile, "in condense 3, second genome is:\n" );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, "%3d, ", ingene2[i] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif
    lastgen = ingene2[num_genes - 1];
    gen1 = ingene2[0];
    gen1first = gen1;
    gen2 = ingene2[1];
    /* check succ[gen1] */
    if ( ( succ[gen1] != BREAK ) && ( succ[gen1] != gen2 ) )
    {
        /* break adjacency gen1->succ[gen1] and mirror */
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( pred[-gen1] != BREAK ) && ( pred[-gen1] != -gen2 ) )
    {
        /* break adjacency and mirror */
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        *num_cond += 1;
    }
    /* check pred[gen1] */
    if ( ( pred[gen1] != BREAK ) && ( pred[gen1] != lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( succ[-gen1] != BREAK ) && ( succ[-gen1] != -lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        *num_cond += 1;
    }
    /* all middle adjacencies */
    for ( i = 2; i < num_genes; i++ )
    {
        lastgen = gen1;
        gen1 = gen2;
        gen2 = ingene2[i];
        /* check succ[gen1] */
        if ( ( succ[gen1] != BREAK ) && ( succ[gen1] != gen2 ) )
        {
            /* break adjacency gen1->succ[gen1] and mirror */
            pred[succ[gen1]] = BREAK;
            succ[gen1] = BREAK;
            succ[pred[-gen1]] = BREAK;
            pred[-gen1] = BREAK;
            *num_cond += 1;
        }
        /* check mirror */
        if ( ( pred[-gen1] != BREAK ) && ( pred[-gen1] != -gen2 ) )
        {
            /* break adjacency and mirror */
            succ[pred[-gen1]] = BREAK;
            pred[-gen1] = BREAK;
            pred[succ[gen1]] = BREAK;
            succ[gen1] = BREAK;
            *num_cond += 1;
        }
        /* check pred[gen1] */
        if ( ( pred[gen1] != BREAK ) && ( pred[gen1] != lastgen ) )
        {
            /* break adjacency pred[gen1]->gen1 & mirror */
            succ[pred[gen1]] = BREAK;
            pred[gen1] = BREAK;
            pred[succ[-gen1]] = BREAK;
            succ[-gen1] = BREAK;
            *num_cond += 1;
        }
        /* check mirror */
        if ( ( succ[-gen1] != BREAK ) && ( succ[-gen1] != -lastgen ) )
        {
            /* break adjacency pred[gen1]->gen1 & mirror */
            pred[succ[-gen1]] = BREAK;
            succ[-gen1] = BREAK;
            succ[pred[gen1]] = BREAK;
            pred[gen1] = BREAK;
            *num_cond += 1;
        }
    }
    /* last adjacency */
    lastgen = gen1;
    gen1 = gen2;
    gen2 = gen1first;
    /* check succ[gen1] */
    if ( ( succ[gen1] != BREAK ) && ( succ[gen1] != gen2 ) )
    {
        /* break adjacency gen1->succ[gen1] and mirror */
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( pred[-gen1] != BREAK ) && ( pred[-gen1] != -gen2 ) )
    {
        /* break adjacency and mirror */
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        *num_cond += 1;
    }
    /* check pred[gen1] */
    if ( ( pred[gen1] != BREAK ) && ( pred[gen1] != lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( succ[-gen1] != BREAK ) && ( succ[-gen1] != -lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        *num_cond += 1;
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "In condense3, done scanning genome 2\n" );
    for ( i = -num_genes; i <= num_genes; i++ )
        fprintf ( outfile, "i=%3d, pred=%3d, succ=%3d\n", i, pred[i],
                  succ[i] );
    fprintf ( outfile, "num_cond=%3d\n", *num_cond );
    fflush ( outfile );
#endif

    /* third genome */
    /* given the situation
       lastgen -> gen1 -> gen2
       we need to check and perhaps set four adjacencies:
       pred[gen1] =? lastgen, succ[gen1] =? gen2,
       succ[-gen1] =? -lastgen, pred[-gen1] =? -gen2 */
    /* first adjacency */
#ifdef VERYVERBOSE
    fprintf ( outfile, "in condense 3, third genome is:\n" );
    for ( i = 0; i < num_genes; i++ )
        fprintf ( outfile, "%3d, ", ingene3[i] );
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif
    lastgen = ingene3[num_genes - 1];
    gen1 = ingene3[0];
    gen1first = gen1;
    gen2 = ingene3[1];
    /* check succ[gen1] */
    if ( ( succ[gen1] != BREAK ) && ( succ[gen1] != gen2 ) )
    {
        /* break adjacency gen1->succ[gen1] and mirror */
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( pred[-gen1] != BREAK ) && ( pred[-gen1] != -gen2 ) )
    {
        /* break adjacency and mirror */
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        *num_cond += 1;
    }
    /* check pred[gen1] */
    if ( ( pred[gen1] != BREAK ) && ( pred[gen1] != lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( succ[-gen1] != BREAK ) && ( succ[-gen1] != -lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        *num_cond += 1;
    }
    /* all middle adjacencies */
    for ( i = 2; i < num_genes; i++ )
    {
        lastgen = gen1;
        gen1 = gen2;
        gen2 = ingene3[i];
        /* check succ[gen1] */
        if ( ( succ[gen1] != BREAK ) && ( succ[gen1] != gen2 ) )
        {
            /* break adjacency gen1->succ[gen1] and mirror */
            pred[succ[gen1]] = BREAK;
            succ[gen1] = BREAK;
            succ[pred[-gen1]] = BREAK;
            pred[-gen1] = BREAK;
            *num_cond += 1;
        }
        /* check mirror */
        if ( ( pred[-gen1] != BREAK ) && ( pred[-gen1] != -gen2 ) )
        {
            /* break adjacency and mirror */
            succ[pred[-gen1]] = BREAK;
            pred[-gen1] = BREAK;
            pred[succ[gen1]] = BREAK;
            succ[gen1] = BREAK;
            *num_cond += 1;
        }
        /* check pred[gen1] */
        if ( ( pred[gen1] != BREAK ) && ( pred[gen1] != lastgen ) )
        {
            /* break adjacency pred[gen1]->gen1 & mirror */
            succ[pred[gen1]] = BREAK;
            pred[gen1] = BREAK;
            pred[succ[-gen1]] = BREAK;
            succ[-gen1] = BREAK;
            *num_cond += 1;
        }
        /* check mirror */
        if ( ( succ[-gen1] != BREAK ) && ( succ[-gen1] != -lastgen ) )
        {
            /* break adjacency pred[gen1]->gen1 & mirror */
            pred[succ[-gen1]] = BREAK;
            succ[-gen1] = BREAK;
            succ[pred[gen1]] = BREAK;
            pred[gen1] = BREAK;
            *num_cond += 1;
        }
    }
    /* last adjacency */
    lastgen = gen1;
    gen1 = gen2;
    gen2 = gen1first;
    /* check succ[gen1] */
    if ( ( succ[gen1] != BREAK ) && ( succ[gen1] != gen2 ) )
    {
        /* break adjacency gen1->succ[gen1] and mirror */
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( pred[-gen1] != BREAK ) && ( pred[-gen1] != -gen2 ) )
    {
        /* break adjacency and mirror */
        succ[pred[-gen1]] = BREAK;
        pred[-gen1] = BREAK;
        pred[succ[gen1]] = BREAK;
        succ[gen1] = BREAK;
        *num_cond += 1;
    }
    /* check pred[gen1] */
    if ( ( pred[gen1] != BREAK ) && ( pred[gen1] != lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        *num_cond += 1;
    }
    /* check mirror */
    if ( ( succ[-gen1] != BREAK ) && ( succ[-gen1] != -lastgen ) )
    {
        /* break adjacency pred[gen1]->gen1 & mirror */
        pred[succ[-gen1]] = BREAK;
        succ[-gen1] = BREAK;
        succ[pred[gen1]] = BREAK;
        pred[gen1] = BREAK;
        *num_cond += 1;
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "In condense3, done scanning genome 3\n" );
    fflush ( outfile );
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile, "i=%3d, pred=%3d, succ=%3d\n", i, pred[i],
                  succ[i] );
        fflush ( outfile );
    }
    fprintf ( outfile, "num_cond=%3d\n", *num_cond );
    fflush ( outfile );
#endif

    if ( *num_cond == 0 )
        return;
    /* no need for any work: all 3 genomes are the same */


    /* Now the arrays succ and pred record exactly common sequences and
       num_cond stores the count of distinct sequences */
    /* Note that the same common sequence may occur twice in the arrays,
       once in each direction -- we'll leave it that way */

    /* Now set the code array, which assigns integers between 1 and num_cond
       to the various chains */
    /* Traverse arrays, recoding */
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        code[i] = decode[i] = 0;
    }
    newcode = 1;
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        if ( ( code[i] == 0 ) && ( pred[i] == BREAK ) )
        {
            /* start of sequence, not yet labeled through mirror */
            code[i] = newcode;
            decode[newcode] = i;
            /* check if there is a mirror version */
            ind = i;
            while ( succ[ind] != BREAK )
                ind = succ[ind];    /* get to the end of chain (start of mirror) */
            if ( code[-ind] == 0 )
            {
                code[-ind] = -newcode;  /* mirror version has opposite sign */
                decode[-newcode] = -ind;
            }
            newcode++;
        }
    }
#ifdef DEBUG
    fprintf ( outfile, "In condense3, done coding\n" );
    fflush ( outfile );
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile, "code[%3d]=%3d\n", i, code[i] );
        fflush ( outfile );
    }
    for ( i = -num_genes; i <= num_genes; i++ )
    {
        fprintf ( outfile, "decode[%3d]=%3d\n", i, decode[i] );
        fflush ( outfile );
    }
#endif

    /* write the new genomes */
    /* genome 1 */
#ifdef VERYVERBOSE
    fprintf ( outfile, "writing outgene1\n" );
    fflush ( outfile );
#endif
    index = 0;
    for ( i = 0; i < num_genes; i++ )
    {
        ind = ingene1[i];
        if ( code[ind] != 0 )
        {
#ifdef VERYVERBOSE
            fprintf ( outfile, "%3d, ", code[ind] );
#endif
            outgene1[index] = code[ind];
            index++;
        }
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "\n" );
    fprintf ( outfile, "writing outgene2\n" );
    fflush ( outfile );
#endif
    /* genome 2 */
    index = 0;
    for ( i = 0; i < num_genes; i++ )
    {
        ind = ingene2[i];
        if ( code[ind] != 0 )
        {
#ifdef VERYVERBOSE
            fprintf ( outfile, "%3d, ", code[ind] );
#endif
            outgene2[index] = code[ind];
            index++;
        }
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "\n" );
    fprintf ( outfile, "writing outgene3\n" );
    fflush ( outfile );
#endif
    /* genome 3 */
    index = 0;
    for ( i = 0; i < num_genes; i++ )
    {
        ind = ingene3[i];
        if ( code[ind] != 0 )
        {
#ifdef VERYVERBOSE
            fprintf ( outfile, "%3d, ", code[ind] );
#endif
            outgene3[index] = code[ind];
            index++;
        }
    }
#ifdef VERYVERBOSE
    fprintf ( outfile, "\n" );
    fflush ( outfile );
#endif

    /* We'll keep the arrays code, succ, and pred for decoding
       after a successful relabeling */
    return;
}

void
decode3 ( int *outgenes, int *ingenes, int *succ, int *decode, int num_cond )
{
    /* decode the gene list of num_cond genes in ingenes, using
       pred, succ, decode, and writes the decoded gene list of
       num_genes genes in outgenes */
    int i, index, gene;

    index = 0;                  /* index into decoded genome */
#ifdef DEBUG
    fprintf ( outfile, "decoding results: \n" );
    for ( i = -num_cond; i <= num_cond; i++ )
    {
        fprintf ( outfile, "decode[%3d]=%3d\n", i, decode[i] );
    }
    fprintf ( outfile,
              "outgenes=%p, ingenes=%p, succ=%p, decode=%p, numcond=%3d\n",
              outgenes, ingenes, succ, decode, num_cond );
    fprintf ( outfile, "code: " );
    fflush ( outfile );
#endif
    for ( i = 0; i < num_cond; i++ )
    {                           /* scan condensed genome */
        gene = decode[ingenes[i]];
        /* follow the chain (if any) starting at gene in the succ
           array and write it out to outgene */
        outgenes[index] = gene;
#ifdef DEBUG
        fprintf ( outfile, "%3d, ", outgenes[index] );
#endif
        index++;
        while ( succ[gene] != BREAK )
        {
            gene = succ[gene];
            outgenes[index] = gene;
#ifdef DEBUG
            fprintf ( outfile, "%3d, ", outgenes[index] );
#endif
            index++;
        }
    }
#ifdef DEBUG
    fprintf ( outfile, "\n" );
#endif
    return;
}
