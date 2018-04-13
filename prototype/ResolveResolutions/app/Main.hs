{-# LANGUAGE DeriveGeneric, FlexibleContexts, TemplateHaskell #-}

module Main (main) where


import Data.Semigroup ((<>))
import Data.Void
import Development.GitRev (gitCommitCount, gitHash)
import File.Format.Newick
import GHC.Generics
import Options.Applicative hiding (ParseError)
import System.IO
import Text.Megaparsec (Parsec, ParseError, Token, parse, parseErrorPretty')
import Text.PrettyPrint.ANSI.Leijen ((<+>), (</>), align, indent, int, line, string, text)


-- main :: IO ()
-- main = do
--     hSetBuffering stdout NoBuffering
--     inputStream  <- readFile $ inputFile opts
--     outputStream <- case parse' computationalStreamParser (inputFile opts) inputStream of
--                     Left  err -> pure $ parseErrorPretty' (inputFile opts) err
--                     Right val -> renderSearchState <$> runEvaluation (evaluate (optimizeComputation val))
--     writeFile (outputFile opts) outputStream
--   where
--      parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
--      parse' = parse


resolve :: String -> String
resolve input = output
    where
        output =



\For {$e \in graph$}
    \If {$e$ is leaf}
        \Return $e$    \quad \algorithmiccomment{$e$ is its own root}
    \EndIf
    \State \textit{resolutions} $\gets \emptyset$

    \State $t_l \gets$ first tree subtending $e$
    \State $t_r \gets$ second tree subtending $e$


    \For {$subtree_l$ in \Call{enumerate}{$t_l$}}
        \For {$subtree_r$ in \Call{enumerate}{$t_r$}}

            \State $trees_{final} \cup (subtree_l \oplus subtree_r)$

            \quad \quad \quad \quad \quad \algorithmiccomment {Function $\oplus$ joins the roots of }

            \quad \quad \quad \quad \quad \algorithmiccomment {$subtree_l$ and $subtree_r$ with an edge}

            \quad \quad \quad \quad \quad \algorithmiccomment {and subtends a root to that edge,}

            \quad \quad \quad \quad \quad \algorithmiccomment {so a rooted tree is always returned.}
        \EndFor
    \EndFor
%    \If {$v_1 \in \textit{done}$ \textbf{or} $v_2 \in \textit{done}$}
%       \State $resolutions \gets resolutions$
%    \Else
%        \If {$prevValue = v.unambiguousFinal$}
%            \State $v.updated \gets False$
%        \Else
%            \State $v.updated \gets$ \; True
%            \State $convergence \gets$ \; False
%        \EndIf
%    \EndIf
\EndFor
