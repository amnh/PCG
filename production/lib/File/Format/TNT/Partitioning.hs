{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Partitioning where

import           File.Format.TNT.Command.CCode
import           File.Format.TNT.Command.CNames
import           File.Format.TNT.Command.Procedure
import           File.Format.TNT.Command.TRead
import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Prim (MonadParsec)

data Part
   = CC CCode
   | CN CNames
   | TR TRead 
   | XR XRead
   | Ignore

gatherCommands :: MonadParsec s m Char => m ([CCode],[CNames],[TRead],[XRead])
gatherCommands = partition <$> many commands
  where
    commands  = choice
              [ CC     <$> ccodeCommand
              , CN     <$> cnamesCommand
              , TR     <$> treadCommand
              , XR     <$> xreadCommand
              , Ignore <$  procedureCommand
              , Ignore <$  ignoredCommand
              ]
    partition = foldr f ([],[],[],[])
      where
        f (CC e) (w,x,y,z) = (e:w,  x,  y,  z)
        f (CN e) (w,x,y,z) = (  w,e:x,  y,  z)
        f (TR e) (w,x,y,z) = (  w,  x,e:y,  z)
        f (XR e) (w,x,y,z) = (  w,  x,  y,e:z)
        f Ignore x         = x

ignoredCommand :: MonadParsec s m Char => m String
ignoredCommand = commandKeyword <* commandBody <* symbol terminal
  where
    commandKeyword = somethingTill inlineSpaceChar
    commandBody    = trim $ somethingTill terminal
    terminal       = char ';'
