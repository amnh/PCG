------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG.Class
-- Copyright   :  () 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------


{-# LANGUAGE MultiParamTypeClasses #-}


module Bio.Graph.PhylogeneticDAG.Class
  ( PhylogeneticDAGish(..)
  ) where


-- |
-- A typeclass for abstracting opperations on a phylogenetic DAG.
class PhylogeneticDAGish dag where

    postorderDAG         :: (e -> [e'] -> e')
                         -> (n -> [n'] -> n')
                         -> (u -> [u'] -> u')
                         -> (v -> [v'] -> v')
                         -> (w -> [w'] -> w')
                         -> (x -> [x'] -> x')
                         -> (y -> [y'] -> y')
                         -> (z -> [z'] -> z')
                         -> dag e  n  u  v  w  x  y  z
                         -> dag e' n' u' v' w' x' y' z'
    postorderDAG f1 f2 f3 f4 f5 f6 f7 f8
        = postorderEdge        f1
        . postorderNode        f2
        . postorderContinuous  f3
        . postorderNonAdditive f4
        . postorderAdditive    f5
        . postorderMetric      f6
        . postorderNonMetric   f7
        . postorderDynamic     f8


    postorderSequence    :: (u -> [u'] -> u')
                         -> (v -> [v'] -> v')
                         -> (w -> [w'] -> w')
                         -> (x -> [x'] -> x')
                         -> (y -> [y'] -> y')
                         -> (z -> [z'] -> z')
                         -> dag e n u  v  w  x  y  z
                         -> dag e n u' v' w' x' y' z'
    postorderSequence      = postorderDAG id' id'


    postorderEdge        :: (e -> [e'] -> e')
                         -> dag e  n u v w x y z
                         -> dag e' n u v w x y z
    postorderEdge        f = postorderDAG f id' id' id' id' id' id' id'


    postorderNode        :: (n -> [n'] -> n')
                         -> dag e n  u v w x y z
                         -> dag e n' u v w x y z
    postorderNode        f = postorderDAG id' f id' id' id' id' id' id'


    postorderContinuous  :: (u -> [u'] -> u')
                         -> dag e n u  v w x y z
                         -> dag e n u' v w x y z
    postorderContinuous  f = postorderDAG id' id' f id' id' id' id' id'


    postorderNonAdditive :: (v -> [v'] -> v')
                         -> dag e n u v  w x y z
                         -> dag e n u v' w x y z
    postorderNonAdditive f = postorderDAG id' id' id' f id' id' id' id'


    postorderAdditive    :: (w -> [w'] -> w')
                         -> dag e n u v w  x y z
                         -> dag e n u v w' x y z
    postorderAdditive    f = postorderDAG id' id' id' id' f id' id' id'


    postorderMetric      :: (x -> [x'] -> x')
                         -> dag e n u v w x  y z
                         -> dag e n u v w x' y z
    postorderMetric      f = postorderDAG id' id' id' id' id' f id' id'


    postorderNonMetric   :: (y -> [y'] -> y')
                         -> dag e n u v w x y  z
                         -> dag e n u v w x y' z
    postorderNonMetric   f = postorderDAG id' id' id' id' id' id' f id'


    postorderDynamic     :: (z -> [z'] -> z')
                         -> dag e n u v w x y z
                         -> dag e n u v w x y z'
    postorderDynamic       = postorderDAG id' id' id' id' id' id' id'









    preorderDAG         :: (e -> [(Word,e')] -> e')
                        -> (n -> [(Word,n')] -> n')
                        -> (u -> [(Word,u')] -> u')
                        -> (v -> [(Word,v')] -> v')
                        -> (w -> [(Word,w')] -> w')
                        -> (x -> [(Word,x')] -> x')
                        -> (y -> [(Word,y')] -> y')
                        -> (z -> [(Word,z')] -> z')
                        -> dag e  n  u  v  w  x  y  z
                        -> dag e' n' u' v' w' x' y' z'
    preorderDAG f1 f2 f3 f4 f5 f6 f7 f8
        = preorderEdge        f1
        . preorderNode        f2
        . preorderContinuous  f3
        . preorderNonAdditive f4
        . preorderAdditive    f5
        . preorderMetric      f6
        . preorderNonMetric   f7
        . preorderDynamic     f8


    preorderSequence    :: (u -> [(Word,u')] -> u')
                        -> (v -> [(Word,v')] -> v')
                        -> (w -> [(Word,w')] -> w')
                        -> (x -> [(Word,x')] -> x')
                        -> (y -> [(Word,y')] -> y')
                        -> (z -> [(Word,z')] -> z')
                        -> dag e n u  v  w  x  y  z
                        -> dag e n u' v' w' x' y' z'
    preorderSequence     = preorderDAG id' id'


    preorderEdge        :: (e -> [(Word,e')] -> e')
                        -> dag e  n u v w x y z
                        -> dag e' n u v w x y z
    preorderEdge        f = preorderDAG f id' id' id' id' id' id' id'


    preorderNode        :: (n -> [(Word,n')] -> n')
                        -> dag e n  u v w x y z
                        -> dag e n' u v w x y z
    preorderNode        f = preorderDAG id' f id' id' id' id' id' id'


    preorderContinuous  :: (u -> [(Word,u')] -> u')
                        -> dag e n u  v w x y z
                        -> dag e n u' v w x y z
    preorderContinuous  f = preorderDAG id' id' f id' id' id' id' id'


    preorderNonAdditive :: (v -> [(Word,v')] -> v')
                        -> dag e n u v  w x y z
                        -> dag e n u v' w x y z
    preorderNonAdditive f = preorderDAG id' id' id' f id' id' id' id'


    preorderAdditive    :: (w -> [(Word,w')] -> w')
                        -> dag e n u v w  x y z
                        -> dag e n u v w' x y z
    preorderAdditive    f = preorderDAG id' id' id' id' f id' id' id'


    preorderMetric      :: (x -> [(Word,x')] -> x')
                        -> dag e n u v w x  y z
                        -> dag e n u v w x' y z
    preorderMetric      f = preorderDAG id' id' id' id' id' f id' id'


    preorderNonMetric   :: (y -> [(Word,y')] -> y')
                        -> dag e n u v w x y  z
                        -> dag e n u v w x y' z
    preorderNonMetric   f = preorderDAG id' id' id' id' id' id' f id'


    preorderDynamic     :: (z -> [(Word,z')] -> z')
                        -> dag e n u v w x y z
                        -> dag e n u v w x y z'
    preorderDynamic       = preorderDAG id' id' id' id' id' id' id'


id' :: a -> b -> a
id' x _ = x
