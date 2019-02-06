{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module PCG.CommandLineOptions.Display
  ( gatherDisplayInformation
  , printVersionInformation
  , printSplashImage
  , printContributions
  ) where

import Control.Arrow                ((&&&))
import Data.Foldable
import Data.List                    (intercalate, intersperse)
import Data.Semigroup               ((<>))
import PCG.CommandLineOptions.Types
import PCG.Software.Credits
import PCG.Software.Metadata


gatherDisplayInformation :: CommandLineOptions -> Maybe (IO ())
gatherDisplayInformation cmdOpts =
    case printingActions of
      []  -> Nothing
      [x] -> Just x
      xs  -> Just . sequenceA_ . (\x -> pNL : x <> [pNL]) $ intersperse pNL xs
  where
    pNL = putStrLn ""
    printingActions = fmap fst . filter snd $
        [ const printVersionInformation &&& printVersion
        , const printSplashImage        &&& printSplash
        , const printContributions      &&& printCredits
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
  , "  | |  \\/_ __ __ _ _ __ | |__    ┃ ┗┳┛ ┗┳┛ ┗┳┛  ┗┳╩┳┛  ┃ ┃ ┗┳┛ "
  , "  | | __| '__/ _` | '_ \\| '_ \\   ┃  ┃   ┃ ╔═╩╗   ┃ ┃   ┃ ┃  ┃ "
  , "  | |_\\ \\ | | (_| | |_) | | | \\  ┗┳━┛   ┗━┫  ┃   ┗┳┛   ┃ ┗┳━┛  "
  , "   \\____/_|  \\__,_| .__/|_| |_|   ┗━━━┳━━━┛  ┗━━┳━┛    ┗┳━┛   "
  , "                  | |                 ╹         ╹       ╹     "
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
    rawAuthorLines = $(authorsList)


printFunderList :: IO ()
printFunderList = putStrLn $ renderedHeaderLines <> renderedFundingSources
  where
    renderedHeaderLines = unlines
      [ "  │ Funding Provided By:  │"
      , "  ╘═══════════════════════╛"
      , ""
      ]
    renderedFundingSources = (<>"\n") . intercalate "\n\n" $ renderSource <$> rawFundingSources
    rawFundingSources = $(fundingList)
    renderSource (src, url) = "    • " <> src <> maybe "" ("\n      › " <>) url
