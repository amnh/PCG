{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PCG.CommandLineOptions.Display
  ( gatherDisplayInformation
  ) where

import Control.Arrow                ((&&&))
import Data.Bimap                   (toMap)
import Data.Foldable
import Data.Key
import Data.List                    (intersperse)
import Data.MonoTraversable
import Data.Semigroup               ((<>))
import Data.Text                    hiding (filter, intersperse, replicate)
import Data.Text.IO
import PCG.CommandLineOptions.Types
import PCG.Software.Credits
import PCG.Software.Metadata
import Prelude                      hiding (putStrLn, unlines, unwords)
import System.ErrorPhase
import System.Exit
import TextShow


gatherDisplayInformation :: CommandLineOptions -> Maybe (IO ())
gatherDisplayInformation cmdOpts =
    case printingActions of
      []  -> Nothing
      [x] -> Just x
      xs  -> Just . sequenceA_ . (\x -> pNL : x <> [pNL]) $ intersperse pNL xs
  where
    pNL = putStrLn ""
    printingActions = fmap fst . filter snd $
        [ const printVersionInformation    &&& printVersion
        , const printSplashImage           &&& printSplash
        , const printContributions         &&& printCredits
        , const printExitCodeDocumentation &&& printExitCodes
        ] <*> [cmdOpts]


printVersionInformation :: IO ()
printVersionInformation = putStrLn fullVersionInformation


printSplashImage :: IO ()
printSplashImage = putStrLn $ unlines
  [ "  ______ _           _                             _   _      "
  , "  | ___ \\ |         | |                           | | (_)     "
  , "  | |_/ / |__  _   _| | ___   __ _  ___ _ __   ___| |_ _  ___ "
  , "  |  __/| '_ \\| | | | |/ _ \\ / _` |/ _ \\ '_ \\ / _ \\ __| |/ __|"
  , "  | |   | | | | |_| | | (_) | (_| |  __/ | | |  __/ |_| | (__ "
  , "  \\_|   |_| |_|\\__, |_|\\___/ \\__, |\\___|_| |_|\\___|\\__|_|\\___|"
  , "                __/ |         __/ |                           "
  , "               |___/         |___/                            "
  , "   _____                                              _       "
  , "  /  __ \\                                            | |      "
  , "  | /  \\/ ___  _ __ ___  _ __   ___  _ __   ___ _ __ | |_     "
  , "  | |    / _ \\| '_ ` _ \\| '_ \\ / _ \\| '_ \\ / _ \\ '_ \\| __|    "
  , "  | \\__/\\ (_) | | | | | | |_) | (_) | | | |  __/ | | | |_     "
  , "   \\____/\\___/|_| |_| |_| .__/ \\___/|_| |_|\\___|_| |_|\\__|    "
  , "                        | |                                   "
  , "                        |_|                                   "
  , "   _____                 _                                    "
  , "  |  __ \\               | |      W H E E L E R  L A B  A M N H "
  , "  | |  \\/_ __ __ _ _ __ | |__    │ └┬┘ └┬┘ └┬┘  └┮┷┭┘  │ │ └┬┘ "
  , "  | | __| '__/ _` | '_ \\| '_ \\   │  │   │ ┍━┷┑   │ │   │ │  │ "
  , "  | |_\\ \\ | | (_| | |_) | | | \\  └┬─┘   └─┤  │   └┬┘   │ └┬─┘  "
  , "   \\____/_|  \\__,_| .__/|_| |_|   └───┬───┘  └──┬─┘    └┬─┘   "
  , "                  | |                 ╵         ╵       ╵     "
  , "                  |_|                                              "
  ]


printContributions :: IO ()
printContributions = putStrLn "" *> printAuthorList *> printFunderList


printAuthorList :: IO ()
printAuthorList = putStrLn $ renderedHeaderLines <> renderedAuthorLines
  where
    renderedHeaderLines = unlines
      [ "  │ Project Contributors: │"
      , "  ╘═══════════════════════╛"
      , ""
      ]
    renderedAuthorLines = (<>"\n\n") . intercalate "\n\n" $ fmap ("    • " <>) rawAuthorLines
    rawAuthorLines = mempty -- $(authorsList)


printFunderList :: IO ()
printFunderList = putStrLn $ renderedHeaderLines <> renderedFundingSources
  where
    renderedHeaderLines = unlines
      [ "  │ Funding Provided By:  │"
      , "  ╘═══════════════════════╛"
      , ""
      ]
    renderedFundingSources = (<>"\n") . intercalate "\n\n" $ renderSource <$> rawFundingSources
    rawFundingSources = mempty --  $(fundingList)
    renderSource (src, url) = "    • " <> src <> maybe "" ("\n      › " <>) url


printExitCodeDocumentation :: IO ()
printExitCodeDocumentation = putStrLn . intercalate "\n" . (preamble:) . foldMapWithKey f
                           $ toMap errorPhaseToExitCode
  where
    f :: ErrorPhase -> ExitCode -> [Text]
    f ep ec = pure $ unlines
        [ errorPhaseTag
        , "       …" <> g ep
        ]
      where
        errorPhaseTag = unwords
            [ "    •"
            , rPad 10 (showt ep <> ":")
            , "[" <> (lPad 2 . showt $ getValue ec) <> "]"
            ]

    getValue :: ExitCode -> Word
    getValue  ExitSuccess    = 0
    getValue (ExitFailure i) = toEnum $ fromEnum i

    lPad n txt = pack (replicate (n - len) ' ') <> txt
      where
        len = olength txt

    rPad n txt = txt <> pack (replicate (n - len) ' ')
      where
        len = olength txt

    g  Inputing = "PCG attempted to retrieve input streams"
    g   Parsing = "interpreting input streams that were successfully retrieved"
    g  Unifying = "combining multiple data sets into a coherent composite"
    g Computing = "running PCG, please report at https://github.com/amnh/PCG/issues"
    g Outputing = "outputing data streams from PCG"

    preamble = intercalate "\n" $ ("  "<>) <$>
        [ ""
        , "PCG emits specific exit codes to indicate in which \"phase\" of the runtime the"
        , "error(s) occured. These specific exit codes are listed below. If a different"
        , "exit code is emitted, an error that could not be recovered from occurred."
        , ""
        , "Error(s) occurred while…"
        , ""
        ]
