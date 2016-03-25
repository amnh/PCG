-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Nexus.Partition
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Partitioning functions, which take a list of some type and produce a tuple.
-- Where there is a block with multiple optional fields or a field with multiple optional
-- subfields these take the output and put it into a tuple which can then be decomposed
-- and its fields used as arguments to a constructor.
-- I'm wondering if there's isn't a more efficient way to do this.
-- Also, can these be reduced to a single function, since they're all doing the same thing?

module File.Format.Nexus.Partition where

import File.Format.Newick
import File.Format.Nexus.Data


partitionAssumptionBlock :: [AssumptionField] -> ([StepMatrix],[Bool])
partitionAssumptionBlock = foldr f ([],[])
    where
        f (TCMMat n) (a,b) = (n:a,   b)
        f (Add    n) (a,b) = (  a, n:b)
        f (IgnAF  _)    vs = vs

partitionSequenceBlock :: [SeqSubBlock] -> ([[String]],[CharacterFormat],[DimensionsFormat],[String],[[String]],[[String]])
partitionSequenceBlock = foldr f ([],[],[],[],[],[])
    where
        f (Matrix      e)  (v,w,x,y,z,a) = (e:v,   w,   x,   y,   z,   a)
        f (Format      e)  (v,w,x,y,z,a) = (  v, e:w,   x,   y,   z,   a)
        f (Dims        e)  (v,w,x,y,z,a) = (  v,   w, e:x,   y,   z,   a)
        f (Eliminate   e)  (v,w,x,y,z,a) = (  v,   w,   x, e:y,   z,   a)
        f (Taxa        e)  (v,w,x,y,z,a) = (  v,   w,   x,   y, e:z,   a)
        f (CharLabels  e)  (v,w,x,y,z,a) = (  v,   w,   x,   y,   z, e:a)
        f _                           ws = ws

partitionTaxaBlock :: [SeqSubBlock] -> (Int, [String])
partitionTaxaBlock = foldr f (0,[])
    where
        f (Dims n) (_,z) = (num, z)
            where
                num = numTaxa n
        f (Taxa n) (y,_) = (  y, n)
        f _           ws = ws

partitionNexusBlocks :: [NexusBlock] -> ([PhyloSequence], [TaxaSpecification], [TreeBlock], [AssumptionBlock], [IgnBlock])
partitionNexusBlocks = foldr f ([],[],[],[],[])
  where
    f (CharacterBlock   n) (xs,ys,zs,as,bs) = (n:xs,   ys,   zs,   as,   bs)
    f (TaxaBlock        n) (xs,ys,zs,as,bs) = (  xs, n:ys,   zs,   as,   bs)
    f (TreesBlock       n) (xs,ys,zs,as,bs) = (  xs,   ys, n:zs,   as,   bs)
    f (AssumptionsBlock n) (xs,ys,zs,as,bs) = (  xs,   ys,   zs, n:as,   bs)
    f (SkippedBlock     n) (xs,ys,zs,as,bs) = (  xs,   ys,   zs,   as, n:bs)
    --f _                                  ws = ws

partitionCharFormat :: [CharFormatField] -> (String, Either String [String], Either String [String], String, String, String, String, Bool, Bool, Bool, Bool, Bool)
partitionCharFormat = foldr f ("", Right [""], Right [""], "", "", "", "", False, False, False, False, False)
    where
        f (CharDT      n) (_,q,r,s,t,u,v,w,x,y,z,o) = (n,q,r,s,t,u,v,w,x,y,z,o)
        f (SymStr      n) (p,_,r,s,t,u,v,w,x,y,z,o) = (p,n,r,s,t,u,v,w,x,y,z,o)
        f (EqStr       n) (p,q,_,s,t,u,v,w,x,y,z,o) = (p,q,n,s,t,u,v,w,x,y,z,o)
        f (MissStr     n) (p,q,r,_,t,u,v,w,x,y,z,o) = (p,q,r,n,t,u,v,w,x,y,z,o)
        f (GapChar     n) (p,q,r,s,_,u,v,w,x,y,z,o) = (p,q,r,s,n,u,v,w,x,y,z,o)
        f (MatchChar   n) (p,q,r,s,t,_,v,w,x,y,z,o) = (p,q,r,s,t,n,v,w,x,y,z,o)
        f (Items       n) (p,q,r,s,t,u,_,w,x,y,z,o) = (p,q,r,s,t,u,n,w,x,y,z,o)
        f (RespectCase n) (p,q,r,s,t,u,v,_,x,y,z,o) = (p,q,r,s,t,u,v,n,x,y,z,o)
        f (Tokens      n) (p,q,r,s,t,u,v,w,_,y,z,o) = (p,q,r,s,t,u,v,w,n,y,z,o)
        f (Transpose   n) (p,q,r,s,t,u,v,w,x,_,z,o) = (p,q,r,s,t,u,v,w,x,n,z,o)
        f (Interleave  n) (p,q,r,s,t,u,v,w,x,y,_,o) = (p,q,r,s,t,u,v,w,x,y,n,o)
        f (Unlabeled   n) (p,q,r,s,t,u,v,w,x,y,z,_) = (p,q,r,s,t,u,v,w,x,y,z,n)
        f (IgnFF       _)                        ws = ws

partitionTreeBlock :: [TreeField] -> ([[String]], [(String,NewickForest)])
partitionTreeBlock = foldr f ([],[])
    where
        f (Translation n) (ys,zs) = (n:ys,   zs)
        f (Tree n)        (ys,zs) = (  ys, n:zs)
        f _                    ws = ws
