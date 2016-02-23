{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Partitioning where

import           File.Format.TNT.Command.CCode
import           File.Format.TNT.Command.Procedure
import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Prim (MonadParsec)

data Part
   =
   | CC CCode
   | TR TRead 
   | XR XRead
   | Ignore

gatherCommands :: MonadParsec s m Char => m ([CCode],[TRead],[XRead])
gatherCommands = partition <$> many commands
  where
    commands  = choice
              [ CC     <$> ccodeCommand
              , TR     <$> treadCommand
              , XR     <$> xreadCommand
              , Ignore <$  procedureCommand
              ]
    partition = foldr f ([],[],[])
      where
        f (CC e) (x,y,z) = (e:x,  y,  z)
        f (TR e) (x,y,z) = (  x,e:y,  z)
        f (XR e) (x,y,z) = (  x,  y,e:z)
        f Ignore x       = x
