/* this is the file to define the function interface for the circular ordering
   lower bound. There are two types of circular ordering: the initial and
   the improved. For the improved bound, the "fast" just switch all the
   internode one by one, so it will compute at most n-2 bounds and find the
   largest one. The other will try to switch the internal nodes for
   every combination, so it will compute at most 2^(n-3) bounds.
   The improved bound is implemented by Jijun Tang, please refer to his
   thesis for detail. Jijun can be reached at tangjijun@hotmail.com
*/

#include "structs.h"
#include "invdist.h"
#include "circ_order.h"
extern int DOBRANCH;
extern int nextID;
extern int *ordering;
extern int *bestOrdering;
int *tmp;
int
compute_co_score ( int **distmatrix, struct tNode *treeNode,    /*int first, */
                   int *prev
                   /*,struct genome_struct *genome_list,int num_genes */  )
{
    int id, score = 0;
    if ( treeNode == NULL )
        return 0;
    if ( treeNode->leaf == TRUE )
    {
        id = treeNode->tag;
#ifdef DEBUG
        fprintf ( outfile, "ADDING %d and %d\n", *prev, id );
#endif
        score = distmatrix[( *prev ) - 1][id - 1];
        if ( DOBRANCH )
        {
            ordering[nextID] = id - 1;
            nextID++;
        }
        *prev = id;
    }
    score += compute_co_score ( distmatrix, treeNode->lChild, prev );
    score += compute_co_score ( distmatrix, treeNode->rChild, prev );

#ifdef DEBUG
    fprintf ( outfile, "SCORE is %d\n", score );
    fprintf ( outfile, "FINIS!\n" );
#endif

    return score;
}

int
test_all_co_score_fast ( int best_so_far_2, int colb, int **distmatrix,
                         struct tNode *root, struct tNode *treeNode,
                         int firstg, int *prev )
{
    struct tNode *left;
    struct tNode *right;
    int tmpColb;
    /* if it is a leaf, return with the input colb */
    if ( treeNode->leaf )
        return colb;
    /* calculate the current colb */
    *prev = root->tag;
    tmpColb = compute_co_score ( distmatrix, root, prev );
    tmpColb += distmatrix[firstg - 1][*prev - 1];
    /* if find a good one, return it arbitually */
    if ( best_so_far_2 < tmpColb )
    {
        return tmpColb;
    }
    /* if the bound is better than the input, make it as the new lower bound */
    if ( colb < tmpColb )
        colb = tmpColb;

    /* for internal nodes only */
    /* make copy of the pointer to left and right children,
       will switch them back after finish, to make sure the tree didn't change */
    left = treeNode->lChild;
    right = treeNode->rChild;

    /* swhich left and right */
    treeNode->lChild = right;
    treeNode->rChild = left;
    /* calculate bound, if better, change */
    *prev = root->tag;
    tmpColb = compute_co_score ( distmatrix, root, prev );
    tmpColb += distmatrix[firstg - 1][*prev - 1];
    if ( best_so_far_2 < tmpColb )
    {
        return tmpColb;
    }
    if ( tmpColb > colb )
        colb = tmpColb;

    /* recursive to left child */
    if ( !treeNode->lChild->leaf )
    {
        tmpColb =
            test_all_co_score_fast ( best_so_far_2, colb, distmatrix, root,
                                     treeNode->lChild, firstg, prev );
        if ( best_so_far_2 < tmpColb )
        {
            /* switch back */
            treeNode->lChild = left;
            treeNode->rChild = right;
            return tmpColb;
        }
        if ( tmpColb > colb )
            colb = tmpColb;
    }
    if ( !treeNode->rChild->leaf )
    {
        /* recursive to right child */
        tmpColb =
            test_all_co_score_fast ( best_so_far_2, colb, distmatrix, root,
                                     treeNode->rChild, firstg, prev );
        if ( best_so_far_2 < tmpColb )
        {
            /* switch back */
            treeNode->lChild = left;
            treeNode->rChild = right;
            return tmpColb;
        }
        if ( tmpColb > colb )
            colb = tmpColb;

    }
    /* switch back */
    treeNode->lChild = left;
    treeNode->rChild = right;
    if ( colb < tmpColb )
        return tmpColb;
    return colb;
}

int
test_all_co_score ( int best_so_far_2, int colb, int **distmatrix,
                    struct tNode *root, struct tNode *treeNode, int firstg,
                    int *prev, int flag )
{
    struct tNode *left;
    struct tNode *right;
    int tmpColb;
    /* if it is a leaf, return with the input colb */
    if ( treeNode->leaf )
        return colb;
    /* calculate the current colb */
    if ( flag != 0 )
    {
        *prev = root->tag;
        if ( DOBRANCH )
        {
            nextID = 0;
        }
        tmpColb = compute_co_score ( distmatrix, root, prev );
        tmpColb += distmatrix[firstg - 1][*prev - 1];
        if ( best_so_far_2 < tmpColb )
        {
            if ( DOBRANCH )
            {
                tmp = bestOrdering;
                bestOrdering = ordering;
                ordering = tmp;
            }
            return tmpColb;
        }
        /* if the bound is better than the input, make it as the new lower bound */
        if ( colb < tmpColb )
        {
            colb = tmpColb;
            if ( DOBRANCH )
            {
                tmp = bestOrdering;
                bestOrdering = ordering;
                ordering = tmp;
            }
        }

    }

    /* for internal nodes only */
    /* make copy of the pointer to left and right children,
       will swith them back after finish, to make sure the tree didn't change */
    left = treeNode->lChild;
    right = treeNode->rChild;

    if ( !treeNode->lChild->leaf )
    {
        tmpColb = test_all_co_score ( best_so_far_2, colb, distmatrix, root,
                                      treeNode->lChild, firstg, prev, 0 );
        if ( best_so_far_2 < tmpColb )
        {
            return tmpColb;
        }
        if ( tmpColb > colb )
        {
            colb = tmpColb;
        }
    }

    if ( !treeNode->rChild->leaf )
    {
        tmpColb = test_all_co_score ( best_so_far_2, colb, distmatrix, root,
                                      treeNode->rChild, firstg, prev, 0 );
        if ( best_so_far_2 < tmpColb )
        {
            return tmpColb;
        }
        if ( tmpColb > colb )
        {
            colb = tmpColb;
        }
    }
    /* swhich left and right */
    treeNode->lChild = right;
    treeNode->rChild = left;
    /* calculate bound, if better, change */
    *prev = root->tag;
    if ( DOBRANCH )
    {
        nextID = 0;
    }

    tmpColb = compute_co_score ( distmatrix, root, prev );
    tmpColb += distmatrix[firstg - 1][*prev - 1];
    if ( best_so_far_2 < tmpColb )
    {
        if ( DOBRANCH )
        {
            tmp = bestOrdering;
            bestOrdering = ordering;
            ordering = tmp;
        }
        return tmpColb;
    }
    if ( tmpColb > colb )
    {
        if ( DOBRANCH )
        {
            tmp = bestOrdering;
            bestOrdering = ordering;
            ordering = tmp;
        }
        colb = tmpColb;
    }

    /* recursive to left child */
    if ( !treeNode->lChild->leaf )
    {
        tmpColb = test_all_co_score ( best_so_far_2, colb, distmatrix, root,
                                      treeNode->lChild, firstg, prev, 1 );
        if ( best_so_far_2 < tmpColb )
        {
            return tmpColb;
        }
        if ( tmpColb > colb )
            colb = tmpColb;
    }
    if ( !treeNode->rChild->leaf )
    {
        /* recursive to right child */
        tmpColb = test_all_co_score ( best_so_far_2, colb, distmatrix, root,
                                      treeNode->rChild, firstg, prev, 1 );
        if ( best_so_far_2 < tmpColb )
        {
            return tmpColb;
        }
        if ( tmpColb > colb )
            colb = tmpColb;

    }
    /* switch back */
    treeNode->lChild = left;
    treeNode->rChild = right;
    if ( colb < tmpColb )
        return tmpColb;
    return colb;
}
