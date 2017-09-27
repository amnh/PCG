/*
   This is the file to define the process of a hill climbing method.
   The idea is simple: take the first three genomes, which will create
   one tree, then pick the fourth from the remaining list, try to insert
   it to one of the three edges, and choose the one with the minimum
   score, then go on to the fifth, till the end. However, the considering
   is that the ordering of the input is important, so we will try
   to find an eliminated graph and reorder the input in order to this
   graph, hence there are many functions evolved here.
   This file is implemented by Jijun Tang and can be reached at
   tangjijun@hotmail.com, please refer to his thesis for detail.
*/


#include "structs.h"
#include "labeltree.h"
#include "growTree.h"
#include "circ_order.h"
#include <time.h>
#include <float.h>
extern int init_score_tree ( int COND, struct tNode *tree,
                             int NUM_GENES, int NUM_GENOMES,
                             int tspsolver, int thresh,
                             struct adj_struct *adj_list,
                             struct adj_struct *adj_pool,
                             intpair_t * neighbors, int *stack, int *incycle,
                             int *outcycle, int **weights, int *degree,
                             int *otherEnd, edge_t * edges,
                             struct tNode *tpool,
                             struct genome_struct *labels, int *pred1,
                             int *pred2, int *picked, int *decode, int *genes,
                             int CIRCULAR, int distmethod, distmem_t distmem,
                             int CORRECTION, struct qNode *qpool,
                             int *edgepool, struct genome_struct *genome_list,
                             smalledge_t * smalledges, int **status,
                             triple_t * triple, int inittspsolver, int OPT,
                             int initmethod );

/* it seemed that I have to init tree, because everything will be changed
when add a new genome into the tree, not only the close three, potentially
every internode may be changed, if to store it is another case (huge 
restore , copy and do again*/

int
growTree ( int COND, struct tNode **tree, int NUM_GENES, int NUM_GENOMES,
           int tspsolver, int thresh,
           struct adj_struct *adj_list, struct adj_struct *adj_pool,
           intpair_t * neighbors,
           int *stack, int *incycle, int *outcycle, int **weights,
           int *degree, int *otherEnd, edge_t * edges,
           struct tNode *tpool, struct genome_struct *labels,
           int *pred1, int *pred2, int *picked, int *decode,
           int *genes, int CIRCULAR,
           int distmethod, distmem_t distmem, int CORRECTION,
           struct qNode *qpool, int *edgepool,
           struct genome_struct *genome_list, smalledge_t * smalledges,
           int **status, triple_t * triple, int inittspsolver, int OPT,
           int initmethod, bin_env_t * bin_env )
{
    int score, mtag, ntag, i;
    struct tNode *internal, *newNode, *cutPoint;
    /* initial with the first three genomes */
    if ( *tree == NULL )
    {
        init_gen_bin ( 3, bin_env );
  printf("In Grow Tree %2d\n",genome_list[2].genes[2]);
        *tree = first ( 3, tpool, 0, bin_env );
  printf("In Grow Tree %2d\n",genome_list[2].genes[2]);
        ( *tree )->leaf = TRUE;
        score =
            init_score_tree ( COND, *tree, NUM_GENES, 3, tspsolver, thresh,
                              adj_list, adj_pool, neighbors, stack, incycle,
                              outcycle, weights, degree, otherEnd, edges,
                              tpool, labels, pred1, pred2, picked, decode,
                              genes, CIRCULAR, distmethod, distmem,
                              CORRECTION, qpool, edgepool, genome_list,
                              smalledges, status, triple, inittspsolver, OPT,
                              initmethod );
    }
    printf("I have init scored \n");
    /* add one by one */
    for ( i = 4; i <= NUM_GENOMES; i++ )
    {
        /* find how many leaves and internal nodes in the current tree */
        mtag = 0;
        ntag = 0;
        findNumbers ( *tree, &mtag, &ntag );
        /* now create new leaf and internal node, with topology relation */
        newNode = &tpool[mtag + ntag];
        internal = &tpool[mtag + ntag + 1];
        newNode->leaf = TRUE;
        newNode->parent = internal;
        newNode->lChild = newNode->rChild = NULL;
        newNode->tag = mtag + 1;

        internal->rChild = newNode;
        internal->leaf = FALSE;
        internal->tag = ( 0 - ntag - 1 );

        /* find the best position to add the new edge, which will return a node */
        score = LARGENUM;
        cutPoint =
            addNewEdge ( COND, ( *tree )->lChild, *tree, internal, newNode,
                         &score, NUM_GENES, i, tspsolver, thresh, adj_list,
                         adj_pool, neighbors, stack, incycle, outcycle,
                         weights, degree, otherEnd, edges, tpool, labels,
                         pred1, pred2, picked, decode, genes, CIRCULAR,
                         distmethod, distmem, CORRECTION, qpool, edgepool,
                         genome_list, smalledges, status, triple,
                         inittspsolver, OPT, initmethod );
        /* the new edge should be added between cutPoint and its parent */
        if ( cutPoint->parent->lChild == cutPoint )
        {
            cutPoint->parent->lChild = internal;
        }
        else
        {
            cutPoint->parent->rChild = internal;
        }
        internal->parent = cutPoint->parent;
        cutPoint->parent = internal;
        internal->lChild = cutPoint;
        ( *tree )->leaf = TRUE;
        /* score the current best tree? */
        score =
            init_score_tree ( COND, *tree, NUM_GENES, i, tspsolver, thresh,
                              adj_list, adj_pool, neighbors, stack, incycle,
                              outcycle, weights, degree, otherEnd, edges,
                              tpool, labels, pred1, pred2, picked, decode,
                              genes, CIRCULAR, distmethod, distmem,
                              CORRECTION, qpool, edgepool, genome_list,
                              smalledges, status, triple, inittspsolver, OPT,
                              initmethod );
        /*print_tree_nexus(*tree); */

    }

    return score;

}





/* copy everything in node1 to node2*/
void
copyNode ( struct tNode *node1, struct tNode *node2, int num_genes )
{
    int i;
    /* make copy of the node2's genome ptr */
    struct genome_struct *tmp_genome = node2->genome;
    /*memcpy everythin */
    memcpy ( node2, node1, sizeof ( struct tNode ) );
    /* restore genome of node2 */
    node2->genome = tmp_genome;
    /*copy genes and everthing */
    for ( i = 0; i < num_genes; i++ )
    {
        node2->genome->genes[i] = node1->genome->genes[i];
    }
    node2->genome->encoding = node1->genome->encoding;
    node2->genome->genome_num = node1->genome->genome_num;
    node2->genome->gnamePtr = node1->genome->gnamePtr;

    return;
}

/* internal->tag internal->rChild=newNode newNode->paren=internal, newNode->l=r=NULL, newNode->leaf = true*/
struct tNode *
addNewEdge ( int COND, struct tNode *tree, struct tNode *root,
             struct tNode *internal, struct tNode *newNode, int *score,
             int num_genes, int num_genomes, int tspsolver, int thresh,
             struct adj_struct *adj_list, struct adj_struct *adj_pool,
             intpair_t * neighbors, int *stack, int *incycle, int *outcycle,
             int **weights, int *degree, int *otherEnd, edge_t * edges,
             struct tNode *tpool, struct genome_struct *labels, int *pred1,
             int *pred2, int *picked, int *decode, int *genes, int CIRCULAR,
             int distmethod, distmem_t distmem, int CORRECTION,
             struct qNode *qpool, int *edgepool,
             struct genome_struct *genome_list, smalledge_t * smalledges,
             int **status, triple_t * triple, int inittspsolver, int OPT,
             int initmethod )
{
    int scores[5];
    struct tNode *lReturn, *rReturn;
    scores[0] = scores[1] = scores[2] = scores[4] = LARGENUM * num_genes;


    /* make new topology between the new edge and the tree, that is, add the new edge
       into the tree */
    if ( tree->parent->lChild == tree )
    {
        tree->parent->lChild = internal;
    }
    else
    {
        tree->parent->rChild = internal;
    }
    internal->parent = tree->parent;
    tree->parent = internal;
    internal->lChild = tree;
    root->leaf = TRUE;
    /* score the new tree */
    scores[0] =
        init_score_tree ( COND, root, num_genes, num_genomes, tspsolver,
                          thresh, adj_list, adj_pool, neighbors, stack,
                          incycle, outcycle, weights, degree, otherEnd, edges,
                          tpool, labels, pred1, pred2, picked, decode, genes,
                          CIRCULAR, distmethod, distmem, CORRECTION, qpool,
                          edgepool, genome_list, smalledges, status, triple,
                          inittspsolver, OPT, initmethod );
    /* restore the tree back to the input status, delete the new edge */
    tree->parent = internal->parent;
    if ( tree->parent->lChild == internal )
    {
        tree->parent->lChild = tree;
    }
    else
    {
        tree->parent->rChild = tree;
    }
    /* if the input node is a leaf, return now */
    if ( tree->leaf )
    {
        *score = scores[0];
        return tree;
    }
    /* recursive to child */
    root->leaf = TRUE;
    lReturn =
        addNewEdge ( COND, tree->lChild, root, internal, newNode, &scores[1],
                     num_genes, num_genomes, tspsolver, thresh, adj_list,
                     adj_pool, neighbors, stack, incycle, outcycle, weights,
                     degree, otherEnd, edges, tpool, labels, pred1, pred2,
                     picked, decode, genes, CIRCULAR, distmethod, distmem,
                     CORRECTION, qpool, edgepool, genome_list, smalledges,
                     status, triple, inittspsolver, OPT, initmethod );
    rReturn =
        addNewEdge ( COND, tree->rChild, root, internal, newNode, &scores[2],
                     num_genes, num_genomes, tspsolver, thresh, adj_list,
                     adj_pool, neighbors, stack, incycle, outcycle, weights,
                     degree, otherEnd, edges, tpool, labels, pred1, pred2,
                     picked, decode, genes, CIRCULAR, distmethod, distmem,
                     CORRECTION, qpool, edgepool, genome_list, smalledges,
                     status, triple, inittspsolver, OPT, initmethod );
    /* compare the scores, find the best one, return it */
    scores[3] = scores[0];
    if ( scores[3] > scores[1] )
        scores[3] = scores[1];
    if ( scores[3] > scores[2] )
        scores[3] = scores[2];
    if ( scores[3] == scores[0] )
    {
        *score = scores[0];
        return tree;
    }
    if ( scores[3] == scores[1] )
    {
        *score = scores[1];
        return tree->lChild;
    }
    else
    {
        *score = scores[2];
        return tree->rChild;
    }
}

/* to find how many internal and leaves*/
void
findNumbers ( struct tNode *tree, int *mtag, int *ntag )
{
    if ( tree == NULL )
        return;
    if ( tree->leaf )
        ( *mtag )++;
    else
        ( *ntag )++;
    findNumbers ( tree->lChild, mtag, ntag );
    findNumbers ( tree->rChild, mtag, ntag );
    return;
}


void
orderGenome ( struct genome_struct *genome_list,
              struct genome_struct *cpgenome, int **cpdist, int **distmatrix,
              int num_genes, int num_genomes, int threshHold, int start )
{
    int i, j;
    int *result;
    result = ( int * ) malloc ( sizeof ( int ) * num_genomes );
    for ( i = 0; i < num_genomes; i++ )
    {
        cpdist[i] = ( int * ) malloc ( sizeof ( int ) * num_genomes );
        for ( j = 0; j < num_genomes; j++ )
        {
            cpdist[i][j] = distmatrix[i][j];
            if ( cpdist[i][j] > threshHold )
                cpdist[i][j] = -1;
        }
    }


    eliminateGraph ( cpdist, result, num_genomes, start );

    for ( i = 0; i < num_genomes; i++ )
    {
        printf ( "%d ", result[i] );
        cpgenome[i].encoding = genome_list[result[i]].encoding;
        for ( j = 0; j < num_genes; j++ )
            cpgenome[i].genes[j] = genome_list[result[i]].genes[j];
        cpgenome[i].genome_num = i + 1;
        cpgenome[i].gnamePtr = genome_list[result[i]].gnamePtr;
        /*memcpy(&cpgenome[i], &genome_list[result[i]], sizeof(struct genome_struct)); */
    }

    free ( result );

}

void
eliminateGraph ( int **dist, int *order, int num, int start )
{
    int i, j;
    struct graph_vertex **vertexList;
    struct vertex_stack *aStack;
    int *L;
    int x, v, y, maxL;
    int test, x1;
    L = ( int * ) malloc ( sizeof ( int ) * num );
    vertexList =
        ( struct graph_vertex ** ) malloc ( sizeof ( struct graph_vertex * ) *
                                            num );
    for ( i = 0; i < num; i++ )
    {
        vertexList[i] = createVertex ( num, i );
        L[i] = 0;
    }
    for ( i = 0; i < num; i++ )
    {
        for ( j = 0; j < num; j++ )
        {
            if ( dist[i][j] > 0 && j != vertexList[i]->name )
                addNeighbor ( vertexList[j], vertexList[i] );
        }
    }
    aStack = createStack (  );
    x = start;
    L[x] = -1;
    for ( i = num - 1; i >= 0; i-- )
    {
        v = x;
        order[i] = v;

        stack_push ( aStack, vertexList[v] );
        for ( j = 0; j < num; j++ )
        {
            if ( neighbor ( vertexList[j], vertexList[v] ) )
            {
                if ( L[j] >= 0 )
                {
                    L[j] = L[j] + 1;
                }
            }
        }
        test = 1;
        while ( !stack_empty ( aStack ) && test )
        {
            y = findOne ( aStack )->name;
            x1 = -1;
            maxL = -2;
            for ( j = 0; j < num; j++ )
            {
                if ( neighbor ( vertexList[j], vertexList[y] ) )
                {
                    if ( L[j] > maxL )
                    {
                        maxL = L[j];
                        x1 = j;
                    }
                }
            }
            if ( L[x1] > 0 )
            {
                x = x1;
                L[x1] = -1;
                test = 0;
            }
            else
                stack_pop ( aStack );
        }
    }
    free ( L );
    for ( i = 0; i < num; i++ )
    {
        free ( vertexList[i]->neighbors );
        free ( vertexList[i] );
    }
    free ( vertexList );
    while ( aStack->head != NULL )
    {
        stack_pop ( aStack );
    }
    return;
}

struct graph_vertex *
createVertex ( int num_neighbor, int name )
{
    struct graph_vertex *newVertex;
    int i;
    newVertex =
        ( struct graph_vertex * ) malloc ( sizeof ( struct graph_vertex ) );
    newVertex->neighbors =
        ( struct graph_vertex ** ) malloc ( sizeof ( struct graph_vertex * ) *
                                            ( 1 + num_neighbor ) );
    for ( i = 0; i <= num_neighbor; i++ )
    {
        newVertex->neighbors[i] = NULL;
    }
    newVertex->name = name;
    newVertex->num_neighbors = 0;
    return newVertex;
}

/* add v1 to v2's neighbor*/
void
addNeighbor ( struct graph_vertex *v1, struct graph_vertex *v2 )
{
    v2->neighbors[v2->num_neighbors] = v1;
    v2->num_neighbors++;
}

struct vertex_stack *
createStack (  )
{
    struct vertex_stack *newStack;
    newStack =
        ( struct vertex_stack * ) malloc ( sizeof ( struct vertex_stack ) );
    newStack->head = NULL;
    return newStack;
}
struct graph_vertex *
stack_pop ( struct vertex_stack *aStack )
{
    struct stack_node *tmp;
    struct graph_vertex *reVertex;
    if ( aStack == NULL || aStack->head == NULL )
        return NULL;
    tmp = aStack->head;
    aStack->head = aStack->head->next;
    reVertex = tmp->theVertex;
    free ( tmp );
    return reVertex;
}

void
stack_push ( struct vertex_stack *aStack, struct graph_vertex *aVertex )
{
    struct stack_node *tmp1, *tmp2;
    if ( aStack == NULL || aVertex == NULL )
        return;
    tmp1 = ( struct stack_node * ) malloc ( sizeof ( struct stack_node ) );
    tmp1->next = NULL;
    tmp1->theVertex = aVertex;
    tmp2 = aStack->head;
    aStack->head = tmp1;
    tmp1->next = tmp2;
    return;
}

int
stack_member ( struct vertex_stack *aStack, struct graph_vertex *v )
{
    struct stack_node *tmp;
    tmp = aStack->head;
    while ( tmp != NULL )
    {
        if ( tmp->theVertex == v )
            return 1;
        tmp = tmp->next;
    }
    return 0;
}

int
stack_empty ( struct vertex_stack *aStack )
{
    if ( aStack == NULL || aStack->head == NULL )
        return 1;
    else
        return 0;
}

void
stack_del ( struct vertex_stack *aStack, struct graph_vertex *v )
{
    struct stack_node *tmp1, *tmp2;
    if ( aStack == NULL || aStack->head == NULL )
        return;
    if ( aStack->head->theVertex == v )
    {
        stack_pop ( aStack );
        return;
    }
    tmp1 = aStack->head->next;
    tmp2 = aStack->head;
    while ( tmp1 != NULL )
    {
        if ( tmp1->theVertex == v )
        {
            tmp2->next = tmp1->next;
            free ( tmp1 );
            return;
        }
        tmp1 = tmp1->next;
    }
    return;
}

/* is v1 a neighbor of v2*/
int
neighbor ( struct graph_vertex *v1, struct graph_vertex *v2 )
{
    int i;
    for ( i = 0; i < v2->num_neighbors; i++ )
    {
        if ( v1 == v2->neighbors[i] )
            return 1;
    }
    return 0;
}

/* remove v1 from v2*/
void
removeNeighbor ( struct graph_vertex *v1, struct graph_vertex *v2 )
{
    int i;
    for ( i = 0; i < v2->num_neighbors; i++ )
    {
        if ( v1 == v2->neighbors[i] )
        {
            v2->neighbors[i] = v2->neighbors[v2->num_neighbors - 1];
            v2->neighbors[v2->num_neighbors - 1] = NULL;
            v2->num_neighbors--;
        }
    }
}
struct graph_vertex *
findOne ( struct vertex_stack *aStack )
{
    struct stack_node *tmp;
    tmp = aStack->head;
    return aStack->head->theVertex;
}

void
copyTree ( struct tNode *tree, struct genome_struct *labels, int *cpSC_lChild,
           int *cpSC_rChild, int *cpSC_parent, int num_genes )
{
    int i;
    if ( tree == NULL )
        return;
    i = tree->tag;
    copyGenome ( tree->genome, &labels[i], num_genes );
    cpSC_lChild[i] = *tree->sc_lChild;
    cpSC_rChild[i] = *tree->sc_rChild;
    cpSC_parent[i] = *tree->sc_parent;
    if ( tree->lChild != NULL )
        copyTree ( tree->lChild, labels, cpSC_lChild, cpSC_rChild,
                   cpSC_parent, num_genes );
    if ( tree->rChild != NULL )
        copyTree ( tree->rChild, labels, cpSC_lChild, cpSC_rChild,
                   cpSC_parent, num_genes );
    return;
}

/* copy 1->2*/
void
copyGenome ( struct genome_struct *g1, struct genome_struct *g2,
             int num_genes )
{
    int i;
    for ( i = 0; i < num_genes; i++ )
    {
        g2->genes[i] = g1->genes[i];
    }
    return;
}

void
restoreTree ( struct tNode *tree, struct genome_struct *labels,
              int *cpSC_lChild, int *cpSC_rChild, int *cpSC_parent,
              int num_genes )
{
    int i;
    if ( tree == NULL )
        return;
    i = tree->tag;
    copyGenome ( &labels[i], tree->genome, num_genes );
    *tree->sc_lChild = cpSC_lChild[i];
    *tree->sc_rChild = cpSC_rChild[i];
    *tree->sc_parent = cpSC_parent[i];
    if ( tree->lChild != NULL )
        restoreTree ( tree->lChild, labels, cpSC_lChild, cpSC_rChild,
                      cpSC_parent, num_genes );
    if ( tree->rChild != NULL )
        restoreTree ( tree->rChild, labels, cpSC_lChild, cpSC_rChild,
                      cpSC_parent, num_genes );
    return;
}

void
createUF ( int n, struct union_set *us )
{
    int i;
    for ( i = 0; i < n; i++ )
    {
        us[i].rank = 0;
        us[i].parent = i;
    }
    return;
}

void
unionUF ( int i, int j, struct union_set *us )
{
    if ( us[i].rank >= us[j].rank )
    {
        us[j].parent = i;
        if ( us[i].rank == us[j].rank )
        {
            us[i].rank = us[i].rank + 1;
        }
    }
    else
    {
        us[i].parent = j;
    }
    return;
}

int
findUF ( int i, struct union_set *us )
{
    int above;
    above = us[i].parent;
    while ( above != us[above].parent )
    {
        us[i].parent = us[above].parent;
        i = us[above].parent;
        above = us[i].parent;
    }
    return above;
}

int
MST ( int num_genomes, int **distmatrix )
{
    int i, j, k, i1, j1;
    int *count;
    edge_t **sortedges, *newedge, *edges;
    struct union_set *us;
    int picked, maxweight;
    int numedges;
    struct adj_struct *node = NULL, *tmp;
    struct adj_struct *adj_list;
    us = ( struct union_set * ) malloc ( sizeof ( struct union_set ) *
                                         num_genomes );
    createUF ( num_genomes, us );

    maxweight = -1;
    j = 0;
    for ( i = 0; i < num_genomes; i++ )
    {
        for ( j = i + 1; j < num_genomes; j++ )
        {
            if ( distmatrix[i][j] > maxweight )
                maxweight = distmatrix[i][j];
        }
    }
    maxweight += 1;
    count = ( int * ) malloc ( sizeof ( int ) * maxweight );
    sortedges = ( edge_t ** ) malloc ( sizeof ( edge_t * ) * maxweight );

    adj_list =
        ( struct adj_struct * ) malloc ( ( num_genomes ) *
                                         sizeof ( struct adj_struct ) );

    /* create adj-list */
    for ( i = 0; i < num_genomes; i++ )
    {
        if ( i == num_genomes - 1 )
        {
            adj_list[i].next = NULL;
        }
        else
        {
            adj_list[i].next =
                ( struct adj_struct * )
                malloc ( sizeof ( struct adj_struct ) );
            node = adj_list[i].next;
        }
        for ( j = i + 1; j < num_genomes; j++ )
        {
            node->weight = distmatrix[i][j];
            node->vertex = j;
            if ( j == num_genomes - 1 )
                node->next = NULL;
            else
            {
                node->next =
                    ( struct adj_struct * )
                    malloc ( sizeof ( struct adj_struct ) );
                node = node->next;
            }
        }

    }
    /* uses positive/negative indexing and so position 0 in arrays
       is always wasted */

    /* prepare a sorted list of edges (in increasing order of cost) */

    /* use distribution sort -- here ad hoc, since we only have 4 values:
       0, 1, 2, and L = -largevalue */
    /* could just use an array of pointers since adj. lists are sorted... */

    /* tally values */
    for ( i = 0; i < maxweight; i++ )
    {
        count[i] = 0;
    }
    for ( i = 0; i < num_genomes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            j = node->vertex;
            if ( i != j )
            {
                count[node->weight]++;
            }
            node = node->next;
        }
    }
    numedges = 0;
    for ( i = 0; i < maxweight; i++ )
    {
        numedges += count[i];
    }
    edges = ( edge_t * ) malloc ( sizeof ( edge_t ) * ( numedges + 10 ) );
    /* this is a place to merge with circular_ordering.c, to
       use the same amount of initialized edge**** together */

    sortedges[0] = edges;
    for ( i = 1; i < maxweight; i++ )
    {
        sortedges[i] = sortedges[i - 1] + count[i - 1];
    }

    for ( i = 0; i < num_genomes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            j = node->vertex;
            if ( i != j )
            {
                k = node->weight;
                ( sortedges[k] )->edge1 = node;
                ( sortedges[k] )->I = i;
                ( sortedges[k] )->J = j;
                ( sortedges[k] )++;
            }
            node = node->next;
        }
    }
    for ( i = 0; i < num_genomes; i++ )
    {
        node = adj_list[i].next;
        while ( node != NULL )
        {
            tmp = node->next;
            free ( node );
            node = tmp;
        }
    }
    picked = 0;
    newedge = edges;
    while ( ( picked < num_genomes - 1 )
            && ( newedge < ( edges + numedges ) ) )
    {
        i = newedge->I;
        j = newedge->J;
        i1 = findUF ( i, us );
        j1 = findUF ( j, us );
        if ( i1 != j1 )
        {
            picked++;
            unionUF ( i1, j1, us );
            printf ( "%d->%d\n", i, j );
        }
        newedge++;
    }
    free ( edges );
    free ( adj_list );
    free ( count );
    free ( sortedges );
    return distmatrix[i][j];
}

/* Triangulate the threshold graph:  */
void
triangulate ( int **dist, int **origDist, double w, int num )
{
    struct graph_vertex **vertexList;
    struct graph_vertex **neighborV;
    struct vertex_stack *aStack;
    struct stack_node *tmp1;
    struct graph_vertex *tmpV1;
    int count = 0, i, j, k, n1, n2;
    double gTriWidth = 100.0;
    double max_width = 0;

    double min_cost = FLT_MAX;
    int min_cost_edges = 0;
    int best_v = -1;
    int *tax2pos, *pos2tax;
    int *intersected, *tmpArray;
    double width = 0;
    double cost = 0;
    int num_edges = 0;
    double d_ab;
    double over;

    pos2tax = ( int * ) malloc ( sizeof ( int ) * num );
    tax2pos = ( int * ) malloc ( sizeof ( int ) * num );
    intersected = ( int * ) malloc ( sizeof ( int ) * ( num + 2 ) );
    tmpArray = ( int * ) malloc ( sizeof ( int ) * ( num + 2 ) );

    vertexList =
        ( struct graph_vertex ** ) malloc ( sizeof ( struct graph_vertex * ) *
                                            num );
    for ( i = 0; i < num; i++ )
    {
        vertexList[i] = createVertex ( num, i );
    }
    for ( i = 0; i < num; i++ )
    {
        for ( j = 0; j < num; j++ )
        {
            if ( dist[i][j] > 0 && j != vertexList[i]->name )
                addNeighbor ( vertexList[j], vertexList[i] );
        }
    }
    aStack = createStack (  );
    for ( i = 0; i < num; i++ )
    {
        stack_push ( aStack, vertexList[i] );
    }

    for ( i = 0; i < num; i++ )
        pos2tax[i] = 0;

    while ( !stack_empty ( aStack ) )
    {
        min_cost = FLT_MAX;
        min_cost_edges = 0;
        best_v = -1;

        tmp1 = aStack->head;
        while ( tmp1 != NULL )
        {
            tmpV1 = tmp1->theVertex;
            width = 0;
            cost = 0;
            num_edges = 0;

            intersectNodes ( tmpV1, aStack, num, intersected, tmpArray );
            i = 0;
            while ( i < num && intersected[i] > 0 )
            {
                j = i + 1;
                while ( j < num && intersected[j] > 0 )
                {
                    if ( neighbor ( vertexList[intersected[i]],
                                    vertexList[intersected[j]] ) )
                    {
                        d_ab = dist[intersected[i]][intersected[j]];
                        num_edges++;
                        over = d_ab - w;
                        cost = over;
                        if ( d_ab > width )
                            width = d_ab;
                        if ( cost > min_cost )
                            goto CONTINUE;
                    }
                    j++;
                }
                i++;
            }

            if ( cost < min_cost )
            {
                max_width = width;
                min_cost = cost;
                min_cost_edges = num_edges;
                best_v = tmp1->theVertex->name;
                if ( cost == 0 )
                    break;
            }
          CONTINUE:;
            tmp1 = tmp1->next;
        }
        pos2tax[count] = best_v;
        tax2pos[best_v] = count;
        count++;
        if ( min_cost )
        {
            if ( max_width > gTriWidth )
                gTriWidth = max_width;
            neighborV = vertexList[best_v]->neighbors;
            i = vertexList[best_v]->num_neighbors;
            j = 0;
            while ( j < i )
            {
                if ( stack_member ( aStack, neighborV[j] ) )
                {
                    k = j + 1;
                    while ( k < i )
                    {
                        if ( stack_member ( aStack, neighborV[k] )
                             && !neighbor ( neighborV[j], neighborV[k] ) )
                        {
                            n1 = neighborV[j]->name;
                            n2 = neighborV[k]->name;
                            dist[n1][n2] = origDist[n1][n2];
                            addNeighbor ( vertexList[n1], vertexList[n2] );
                            addNeighbor ( vertexList[n2], vertexList[n1] );
                            min_cost_edges--;
                            if ( min_cost_edges == 0 )
                                goto OUT;
                        }
                        k++;
                    }
                }
                j++;
            }
        }
      OUT:;
        stack_del ( aStack, vertexList[best_v] );
    }

    for ( i = 0; i < num; i++ )
    {
        free ( vertexList[i]->neighbors );
        free ( vertexList[i] );
    }
    free ( vertexList );
    while ( aStack->head != NULL )
    {
        stack_pop ( aStack );
    }

    free ( intersected );
    free ( tmpArray );
    free ( tax2pos );
    free ( pos2tax );
}

void
intersectNodes ( struct graph_vertex *v, struct vertex_stack *aStack,
                 int num, int *intersected, int *tmpArray )
{
    int i, numN;
    struct stack_node *tmp;
    numN = v->num_neighbors;
    for ( i = 0; i < num; i++ )
    {
        tmpArray[i] = 0;
    }
    for ( i = 0; i < numN; i++ )
    {
        tmpArray[v->neighbors[i]->name] = 1;
    }
    tmp = aStack->head;
    while ( tmp != NULL )
    {
        tmpArray[tmp->theVertex->name]++;
        tmp = tmp->next;
    }
    numN = 0;
    for ( i = 0; i < num; i++ )
    {
        if ( tmpArray[i] == 2 )
        {
            intersected[numN] = i;
            numN++;
        }
    }
    intersected[numN] = -1;
    return;

}
