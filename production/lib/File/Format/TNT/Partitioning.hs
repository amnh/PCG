{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Partitioning where

import           File.Format.TNT.Command.CCode
import           File.Format.TNT.Command.Procedure
import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Prim (MonadParsec)

data Part
   = XR XRead
   | CC CCode
   | PR

gatherCommands :: MonadParsec s m Char => m ([XRead],[CCode],[()])
gatherCommands = partition <$> many commands
  where
    commands = choice [XR <$> xreadCommand, CC <$> ccodeCommand, PR <$ procedureCommand]
    partition = foldr f ([],[],[])
      where
        f (XR e) (x,y,z) = (e:x,  y,   z)
        f (CC e) (x,y,z) = (  x,e:y,   z)
        f  PR    (x,y,z) = (  x,  y,():z)
