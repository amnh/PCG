----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Partitioning
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Collects a subset of all TNT commands representing the set of commands
-- relevant to processing character data.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module File.Format.TNT.Partitioning where

import Data.Char (isAlpha)
import File.Format.TNT.Command.CCode
import File.Format.TNT.Command.CNames
import File.Format.TNT.Command.Cost
import File.Format.TNT.Command.NStates
import File.Format.TNT.Command.Procedure
import File.Format.TNT.Command.TRead
import File.Format.TNT.Command.XRead
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim (MonadParsec)

-- | The TNT command subset which is parsed.
data Part
   = CC CCode
   | CN CNames
   | CO Cost
   | NS NStates
   | TR TRead 
   | XR XRead
   | Ignore

-- | A collection of the TNT commands found in the file.
type Commands = ([CCode],[CNames],[Cost],[NStates],[TRead],[XRead])

-- | Collects the subset of the TNT commands related to processing character
--   sequences. Returns the relavent commands in a tuple.
gatherCommands :: (MonadParsec e s m, Token s ~ Char) => m Commands
gatherCommands = partition <$> many commands
  where
    commands  = choice
              [ CC     <$> try ccodeCommand
              , CN     <$> try cnamesCommand
              , CO     <$> try costCommand
              , NS     <$> try nstatesCommand
              , TR     <$>     treadCommand
              , XR     <$>     xreadCommand
              , Ignore <$      procedureCommand
              , Ignore <$      ignoredCommand
              ]
    partition = foldr f ([],[],[],[],[],[])
      where
        f (CC e) (u,v,w,x,y,z) = (e:u,  v,  w,  x,  y,  z)
        f (CN e) (u,v,w,x,y,z) = (  u,e:v,  w,  x,  y,  z)
        f (CO e) (u,v,w,x,y,z) = (  u,  v,e:w,  x,  y,  z)
        f (NS e) (u,v,w,x,y,z) = (  u,  v,  w,e:x,  y,  z)
        f (TR e) (u,v,w,x,y,z) = (  u,  v,  w,  x,e:y,  z)
        f (XR e) (u,v,w,x,y,z) = (  u,  v,  w,  x,  y,e:z)
        f Ignore x             = x

-- | A parser for consuming a command that is not part of the subset of TNT
--   commands relavent to Character sequence processing.
ignoredCommand :: (MonadParsec e s m, Token s ~ Char) => m String
ignoredCommand = commandKeyword <* optional commandBody <* terminal
  where
    commandKeyword = somethingTill $ satisfy (not . isAlpha)
    commandBody    = trim $ anythingTill terminal
    terminal       = symbol $ char ';'
