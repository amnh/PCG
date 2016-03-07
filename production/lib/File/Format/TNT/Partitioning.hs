{-# LANGUAGE FlexibleContexts #-}
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

data Part
   = CC CCode
   | CN CNames
   | CO Cost
   | NS NStates
   | TR TRead 
   | XR XRead
   | Ignore

type Commands = ([CCode],[CNames],[Cost],[NStates],[TRead],[XRead])

gatherCommands :: MonadParsec s m Char => m Commands
gatherCommands = partition <$> many commands
  where
    commands  = choice
              [ CC     <$> try ccodeCommand
              , CN     <$> try cnamesCommand
              , CO     <$> try costCommand
              , NS     <$> try nstatesCommand
              , TR     <$> try treadCommand
              , XR     <$> try xreadCommand
              , Ignore <$  try procedureCommand
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
        f Ignore x           = x

ignoredCommand :: MonadParsec s m Char => m String
ignoredCommand = commandKeyword <* optional (commandBody) <* terminal
  where
    commandKeyword = somethingTill $ satisfy (not . isAlpha)
    commandBody    = trim $ anythingTill terminal
    terminal       = symbol $ char ';'
