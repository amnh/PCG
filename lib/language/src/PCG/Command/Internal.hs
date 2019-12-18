{-# LANGUAGE DerivingStrategies #-}

module PCG.Command.Internal where

data  Command
    = PCG_READ [FileSpecification]
    | PCG_REPORT OutputTarget OutputFormat
    | PCG_ECHO String
    | PCG_ANALYZE String
    | PCG_EXIT
    deriving stock(Show)

