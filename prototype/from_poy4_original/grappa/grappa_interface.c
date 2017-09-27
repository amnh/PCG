#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "caml/bigarray.h" /* Added by Lauren */
#include "caml/memory.h" /* Added by Lauren */
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/mlvalues.h"
#include "invdist.h"
#include "correction.h"
#include "binencode.h"
#include "structs.h"
#include "all_sorting_reversals.h"
#include "lists.h"
#include "med_util.h"
#include "sorting_reversal_median.h"
#include "inversion_median.h"
#include "inversion_median_alberto.h"
#include "growTree.h"
#include "condense.h"
#include "inittree.h"
#include "labeltree.h"
#include "specialinit.h"

#define Genome_matrix_struct(a) ((struct genome_struct *) Data_custom_val(a))

VertexFactory *newvf = NULL;
int DOBRANCH;

struct genome_arr_t
{
    struct genome_struct *genome_ptr;
    int num_genome;
    int num_gene;
};

void grappa_CAML_genome_arr_free (value c_genome_arr) {
    //printf("Start of genone_arr_CAML_free\n"); fflush(stdout);
    struct genome_arr_t *genome_arr;
    struct genome_struct *genome;
    int i;
    genome_arr = (struct genome_arr_t *) Data_custom_val (c_genome_arr);

    for (i = 0 ; i < genome_arr->num_genome; i++){
       genome = genome_arr->genome_ptr + i;
        if (genome != (struct genome_struct *) NULL) {
            free (genome->genes);
            free (genome->gnamePtr);
        }
    }
    free (genome_arr->genome_ptr);
    //printf("End of genone_arr_CAML_free\n"); fflush(stdout);
    return;
}

static struct custom_operations genomeOps = {
    "http://www.amnh.org/poy/genome/grappa.0.1",
    custom_finalize_default,
  //  &genome_CAML_free,
    custom_compare_default,
    custom_hash_default,
    // custom_serialize_default,
    // custom_deserialize_default,
};

static struct custom_operations genomeArrOps = {
    "http://www.amnh.org/poy/genome/grappa.0.1",
    &grappa_CAML_genome_arr_free,
//    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    // custom_serialize_default,
    // custom_deserialize_default,
};

value grappa_CAML_print_genome(value c_genome, value c_num_gen) {
    CAMLparam2(c_genome, c_num_gen);
    struct genome_struct *genome;
    int i, num_gen;
    genome = (struct genome_struct *) Data_custom_val(c_genome);
    num_gen = Int_val (c_num_gen);
    printf ("Number gene for printing: %i\n", num_gen); fflush (stdout);
    printf("%s: ",genome->gnamePtr); fflush (stdout);
    for (i = 0 ; i < num_gen ; i++) {
        printf(" %2d ",genome->genes[i]);
    }
    printf("\nEnd of printing the genome\n"); fflush (stdout);
    CAMLreturn (Val_unit);
}

value grappa_CAML_print_genome_arr (value c_genome_arr, value c_num_genome, value c_num_gen) {
    CAMLparam3(c_genome_arr, c_num_genome, c_num_gen);
    struct genome_arr_t *genome_arr;
    int i,j;
    int num_genome, num_gen;
    genome_arr = (struct genome_arr_t *) Data_custom_val(c_genome_arr);
    num_genome = Int_val(c_num_genome);
    num_gen = Int_val(c_num_gen);

    printf ("Number genome: %i, number gene: %i\n", num_genome, num_gen);
    for (i = 0; i < num_genome; i++) {
        printf("%s: ",(genome_arr->genome_ptr + i)->gnamePtr);
        printf  ("%p\n", genome_arr->genome_ptr + i); fflush (stdout);
        for (j = 0 ; j < num_gen ; j++)
            printf(" %2d", (genome_arr->genome_ptr + i)->genes[j]);
        printf("\n"); fflush (stdout);
        printf ("\n");
    }
    printf ("\nEnd of all genomes\n"); fflush (stdout);
    CAMLreturn (Val_unit);
}

value grappa_CAML_get_num_genome (value c_genome_arr) {
    int num_genome;
    CAMLparam1 (c_genome_arr);
    struct genome_arr_t *genome_arr;
    genome_arr = (struct genome_arr_t *) Data_custom_val (c_genome_arr);
    num_genome = genome_arr->num_genome;
    CAMLreturn (Val_int(num_genome));
}


value grappa_CAML_get_num_gene (value c_genome_arr) {
    CAMLparam1 (c_genome_arr);
    struct genome_arr_t *genome_arr;
    genome_arr = (struct genome_arr_t *) Data_custom_val (c_genome_arr);
    CAMLreturn (Val_int(genome_arr->num_gene));
}

value grappa_CAML_get_one_genome(value c_genome_arr, value c_index) {
    CAMLparam2(c_genome_arr,c_index);
    struct genome_arr_t *genome_arr;
    struct genome_struct *genome;
    int index;

    CAMLlocal1 (c_genome);

    index = Int_val (c_index);
    genome_arr = (struct genome_arr_t  *) Data_custom_val (c_genome_arr);

    /* Allocate custom block.  This may trigger a garbage collection, causing
     * genome_arr to be moved, so we get the new value of genome_arr. */
    c_genome = alloc_custom(&genomeOps, sizeof (struct genome_struct ), 1, 1000000);
    genome = (struct genome_struct *) Data_custom_val(c_genome);

    genome_arr = (struct genome_arr_t  *) Data_custom_val (c_genome_arr);


/*    int *genes;
    int genome_num;
    char *encoding;
    char *gnamePtr;
    char parent[32];
*/

    genome->genes = (genome_arr->genome_ptr + index)->genes;
    genome->gnamePtr = (genome_arr->genome_ptr)->gnamePtr;
    genome->encoding = (genome_arr->genome_ptr + index)->encoding;
    //strcpy(genome->parent, (genome_arr->genome_ptr + index)->parent);



    CAMLreturn(c_genome);
}


/* Added by Lauren to take in Ocaml values.
 Returns the inversion distance between gene1 and gene2.
 gene1-- the first genome
 gene2-- the 2nd genome
 num-- the number of gene in each genome (i.e., the length of gene1;
 the length of both must be the same)
 circular-- an int that's positive if genome are circular */
value grappa_CAML_cmp_inv_dis(value c_gene1, value c_gene2,
				      value c_num_gen, value c_circular) {
    CAMLparam4(c_gene1, c_gene2, c_num_gen, c_circular);
    int num_gene, distance, circ;
    struct genome_struct *g1, *g2;


    g1 = (struct genome_struct *) Data_custom_val (c_gene1);
    g2 = (struct genome_struct *) Data_custom_val (c_gene2);
    num_gene = Int_val (c_num_gen);
    circ = Int_val (c_circular);

    if (circ == 1) {
         distance = invdist_circular_nomem(g1, g2, num_gene);
    } else {
         distance = invdist_noncircular_nomem(g1, g2, 0, num_gene);
    }

    CAMLreturn(Val_int(distance));
}


/*****************************************************************/
value grappa_CAML_create_empty_genome_arr(value numgenome, value numgene)
{
    int Numgenome, Numgene, i;
    struct genome_struct *genome_list;
    struct genome_arr_t *genome_arr;
    CAMLparam2(numgenome, numgene);

    Numgenome = Int_val(numgenome);
    Numgene = Int_val(numgene);
  /************/

    genome_list =
        ( struct genome_struct * ) malloc ( Numgenome *
                                            sizeof ( struct genome_struct ) );
    if ( genome_list == ( struct genome_struct * ) NULL )
        fprintf ( stderr, "ERROR: genome_list NULL\n" );

    for ( i = 0; i < Numgenome; i++ )
    {
         genome_list[i].gnamePtr =
            ( char * ) malloc ( MAX_NAME * sizeof ( char ) );
        sprintf (genome_list[i].gnamePtr, "%i", i);
        if ( genome_list[i].gnamePtr == ( char * ) NULL )
        {
            fprintf ( stderr, "ERROR: gname NULL\n" );
        };

        genome_list[i].genes =( int * ) malloc ( Numgene * sizeof ( int ) );

    }

    CAMLlocal1 (c_genome_arr);
    c_genome_arr = alloc_custom(&genomeArrOps, sizeof(struct genome_arr_t), 1, 1000000);
    genome_arr = (struct genome_arr_t *) Data_custom_val(c_genome_arr);
    genome_arr->genome_ptr = genome_list;
    genome_arr->num_genome = Numgenome;
    genome_arr->num_gene = Numgene;

    CAMLreturn(c_genome_arr);
}

value grappa_CAML_set (value c_genome_arr, value c_genome_no, value c_index, value c_gene_no) {
    struct genome_arr_t *genome_arr;
    int index, genome_no, gene_no;
    CAMLparam4 (c_genome_arr, c_genome_no, c_index, c_gene_no);

    genome_arr = (struct genome_arr_t  *) Data_custom_val (c_genome_arr);
    genome_no = Int_val (c_genome_no);
    gene_no = Int_val (c_gene_no);
    index = Int_val (c_index);
    (genome_arr->genome_ptr + genome_no)->genes[index] = gene_no;

    CAMLreturn(Val_unit);
}

/*
 * See the OCaml interface for more information about this function. Notice that
 * it produce an inverted list of transformations, that is corrected in the
 * OCaml side. *)
 */

value
grappa_CAML_inversions (value genes1, value genes2,
        value c_num_genes, value dist) {
    CAMLparam4(genes1, genes2, c_num_genes, dist);
    CAMLlocal3(resulttmp, result, r);
    List intermediate_reversals_list;
    int num_genes, i, /*j,*/ inv_dist;
    //struct genome_arr_t *genes1_arr, *genes2_arr;
    struct genome_struct *permutation, *origin;
    int *temp_genes;
    Reversal *rev;
    result = Val_int(0); /* We start with the empty list */

    inv_dist = Int_val(dist);

    permutation = (struct genome_struct *) Data_custom_val (genes1);
    origin = (struct genome_struct *) Data_custom_val (genes2);
    /* First one in should be ancestor-- the permutation that you want to
       transform into the descendant (even though "origin" is a confusing
       thing to call descendant) */
    num_genes = Int_val(c_num_genes);

    temp_genes = (int *)malloc(num_genes * sizeof(int));

    if (0 == num_genes) CAMLreturn(result);
    /* Initialize list that will be used to store the sorting reversals
       found between the permutations at each step. */
    init_list(&intermediate_reversals_list, (num_genes + 1) * num_genes,
            sizeof(Reversal *));

    i = 0;

    do {

        clear_list(&intermediate_reversals_list);

        find_all_sorting_reversals(&intermediate_reversals_list, NULL, permutation,
                origin, num_genes, NULL);

        if  (list_size(&intermediate_reversals_list) > 0) {
            rev = (Reversal *)list_get(&intermediate_reversals_list, 0);
            copy_with_reversal(temp_genes, permutation->genes, num_genes, rev);
            permcopy(permutation->genes, temp_genes, num_genes);
            r = caml_alloc_tuple(2);
            Store_field(r,0,Val_int(rev->start + 1));
            Store_field(r,1,Val_int(rev->stop));
            resulttmp = caml_alloc(2,0);
            Store_field(resulttmp,0,r);
            Store_field(resulttmp,1,result);
            result = resulttmp;
        }

        i++;

    } while (list_size(&intermediate_reversals_list) > 0);

    fflush(stdout); /* Change so can be stderr, too? */

    CAMLreturn(result);
}

