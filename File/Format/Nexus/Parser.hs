{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus.Parser where

import Data.Char (isSpace,toLower,toUpper)
import qualified Data.Map as M (Map, fromList)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Vector (Vector)
import Text.Parsec hiding (label)
import Text.Parsec.Custom
import Debug.Trace

data ParseResult = ParseResult [PhyloSequence] [TaxaSpecification] [TreeBlock] deriving (Show)

data NexusBlock
   = TaxaBlock      TaxaSpecification
   | CharacterBlock PhyloSequence
   | TreesBlock     TreeBlock 
   | IgnoredBlock   String
   deriving (Show)

-- | DimensionsFormat is format of dimensions field in characters and unaligned nexus blocks.
-- It could also actually be used for the dimensions in the taxa block, with newTaxa = False
-- and numChars = 0
data DimensionsFormat
   = DimensionsFormat
   { newTaxa  :: Bool
   , numTaxa  :: Int
   , numChars :: Int
   } deriving (Show)

-- | Phylosequence is general sequence type, to be used for both characters (aligned) and unaligned blocks.
data PhyloSequence
   = PhyloSequence
   { aligned         :: Bool
   , matrix          :: [String]
   , format          :: [CharacterFormat]
   , charDims        :: [DimensionsFormat] -- holds length of sequence, as well as info on new taxa
   , elims           :: [String]
   } deriving (Show)

data TreeBlock 
   = TreeBlock 
   { translate :: [[String]]
   , trees     :: [(String,String)]
   } deriving (Show)

-- | SeqSubBlock is list of fields available in sequence blocks. It's set up as an enumeration
-- so that it can be "looped" over when parsing the sequence blocks, as the order of the fields
-- is not documented.
data SeqSubBlock 
   = Matrix          String
   | Format          CharacterFormat
   | Dims            DimensionsFormat
   -- | Items           ItemType
   | Eliminate       String
   | CharStateLabels [CharStateFormat]
   | Ignored         String 
   deriving (Show)

data TaxaSpecification
   = TaxaSpecification
   { taxaDims   :: Int
   , taxaLabels :: [String]
   } deriving (Show)

-- | The different subfields of the Format field in the sequence blocks.
-- As with SeqSubBlock, listed simply so that it can be "looped" over. Will eventually be 
-- coverted to CharacterFormat data type for export
data CharFormatField
   = CharDT      String
   | SymStr      String -- the list of symbols
   | EqStr       String -- the equate (symbol -> symbols) will be processed into Map Char String
   | MissStr     String -- "missing" char
   | GapChar     String -- gap char
   | MatchChar   String -- "match" char
   | Items       String
   | RespectCase Bool 
   | Tokens      Bool 
   | Transpose   Bool
   | Interleave  Bool
   | IgnFF       String -- for non-standard inputs, plus notokens, which is the default anyway
   deriving (Show)


data TreeField
   = Translation [String]
   | Tree        (String,String)
   | IgnTF       String

data CharStateFormat
   = CharStateFormat
   { charNum   :: Int
   , charName  :: String
   , stateName :: [String]
   } deriving (Show)

data CharacterFormat 
   = CharacterFormat 
   { charDataType :: String
   , symbols      :: String
   , equate       :: String
   , missing      :: String
   , gap          :: String
   , matchChar    :: String
   , items        :: String
   , respectCase  :: Bool
   , tokens       :: Bool
   , transpose    :: Bool
   , interleave   :: Bool
   } deriving (Show)

data CharDataType = STANDARD | DNA | RNA | NUCLEOTIDE | PROTEIN | CONTINUOUS deriving (Read, Show)

data Nexus
   = Nexus
   { taxa :: [String] 
   -- , characters :: [[String]]
   } deriving (Show)

parseNexusStream :: String -> Either ParseError Nexus
parseNexusStream = parse (validateParseResult =<< parseNexus <* eof) "PCG encountered a parsing error it could not overcome: "

validateParseResult :: ParseResult -> Parsec s u Nexus
validateParseResult (ParseResult sequences taxas trees)
  | null errors = pure $ Nexus (taxaLabels $ head taxas) (convertMatrix $ head sequences)
  | otherwise   = fails errors
  where
        errors = taxaErrors ++ charErrors
        taxaErrors = catMaybes $ correctTaxaCount : [multipleTaxaBlocks]
        charErrors = catMaybes [multipleSequenceBlocks]
        f [x] = Just x
        f _   = Nothing

        multipleTaxaBlocks = case taxas of
                            []      -> Just "No taxa block supplied." -- TODO: This should probably only fail if newTaxa isn't spec'ed in the sequence blocks.
                            (_:_:_) -> Just "Multiple taxa blocks supplied."
                            _       -> Nothing 
        correctTaxaCount = f taxas >>= \(TaxaSpecification num taxons) -> if num /= length taxons 
                then Just "Incorrect number of taxa."
                else Nothing
        multipleSequenceBlocks = case sequences of 
                                 []      -> Just "No characters or unalignd blocks provided."
                                 (x:y:z:_) -> Just "Too many sequence blocks provided. Only one each of characters and unaligned blocks are allowed."
                                 (x:y:_) -> if (aligned x) && (aligned y)
                                                then Just "More than one characters block provided."
                                                else 
                                                    if not ((aligned x) || (aligned y))
                                                        then Just "More than one aligned block provided."
                                                        else Nothing
                                 _       -> Nothing
        
        convertSeqs = concatSeqs . cleanSeqs sequences  -- convert each sequence, then

                            
    --rows = length protoMatrix
    --badCols = foldr f [] $ zip [(1::Int)..] protoMatrix
    --f (n,e) a = let x = length e in if x /= size then (n,x) : a else a  
    --g (x,y) = (:) (Just $ "Matrix row " ++ show x ++ " has " ++ show y ++ " columns but " ++ show size ++ " columns were expected")
    --errors = alphabetErrors ++ matrixErrors
    --alphabetErrors = catMaybes   [emptyAlphabet,doubleAlphabet]
    --matrixErrors   = catMaybes $ [emptyMatrix,badRowCount] ++ badColCount
    --emptyAlphabet  = case alphabet of 
    --                   [] -> Just "No alphabet specified"
    --                   _  -> Nothing
    --doubleAlphabet = case dupes alphabet of
    --                   [] -> Nothing
    --                   xs -> Just $ "The following symbols were listed multiple times in the custom alphabet: " ++ show xs
    --emptyMatrix    = case protoMatrix of
    --                   [] -> Just "No matrix specified"
    --                   _  -> Nothing
    --badRowCount    = if   rows /= size 
    --                 then Just $ "Matrix has "++show rows++" rows but "++show size++" rows were expected"
    --                 else Nothing
    --badColCount    = foldr g [] badCols

parseNexus :: Parsec String u ParseResult
parseNexus = nexusFileDefinition 

nexusFileDefinition :: Parsec String u ParseResult
nexusFileDefinition = do 
    _       <- string "#NEXUS"
    _       <- spaces
    comment <- optionMaybe commentDefinition
    _       <- spaces
    (x,y,z)   <- partitionNexusBlocks <$> (many nexusBlock)
    pure $ ParseResult x y z

ignoredBlockDefinition :: Parsec String u String
ignoredBlockDefinition = do
    title <- many letter
    _     <- symbol (caseInsensitiveString ";")
    _     <- whitespace
    _     <- anyTill $ symbol (caseInsensitiveString "END;")
    pure $ title

nexusBlock :: Parsec String u NexusBlock
nexusBlock = do
        _      <- symbol (caseInsensitiveString "BEGIN")
        block' <- symbol block
        _      <- symbol (caseInsensitiveString "END;")
        pure $ block'
    where
        block =  (CharacterBlock <$> try (characterBlockDefinition "characters" True))
             <|> (CharacterBlock <$> try (characterBlockDefinition "unaligned" False))
             <|> (TaxaBlock      <$> try taxaBlockDefinition)
             <|> (TreesBlock     <$> try treeBlockDefinition)
             <|> (IgnoredBlock   <$> try ignoredBlockDefinition) 

characterBlockDefinition :: String -> Bool -> Parsec String u PhyloSequence
characterBlockDefinition which aligned = do
    _     <- symbol (caseInsensitiveString $ which ++ ";")
    (v,w,x,y) <- partitionSequenceBlock <$> (many seqSubBlock)
    pure $ PhyloSequence aligned v w x y

taxaBlockDefinition :: Parsec String u TaxaSpecification
taxaBlockDefinition = do
    _          <- symbol (caseInsensitiveString "taxa;")
    _          <- symbol (caseInsensitiveString "dimensions")
    _          <- symbol (caseInsensitiveString "ntax")
    _          <- symbol (char '=')
    taxaCount' <- read <$> symbol integer
    _          <- symbol (char ';')
    theTaxa    <- symbol $ stringListDefinition "taxlabels"
    pure $ TaxaSpecification taxaCount' theTaxa
  where
    plus    = char '+' *> number
    minus   = char '-' <:> number
    number  = many1 digit
    integer = plus <|> minus <|> number
    decimal = integer <++> (char '.' <:> number)
 
treeBlockDefinition :: Parsec String u TreeBlock
treeBlockDefinition = do
        _     <- symbol (caseInsensitiveString "trees;")
        (x,y) <- partitionTreeBlock <$> (many treeFieldDef)
        pure $ TreeBlock x y

seqSubBlock :: Parsec String u SeqSubBlock
seqSubBlock = do
        _      <- optionMaybe whitespace
        block' <- symbol block
        pure block'
    where
        block =  (Dims <$> try dimensionsDefinition)
             <|> (Format <$> try formatDefinition)
             <|> (Eliminate <$> try (stringDefinition "eliminate"))
             <|> (Matrix <$> try matrixDefinition)
             <|> (Ignored <$> try (ignoredSubBlockDef ';'))

dimensionsDefinition :: Parsec String u DimensionsFormat
dimensionsDefinition = do
        _         <- symbol (caseInsensitiveString "dimensions")
        newTaxa   <- optionMaybe (try (symbol (caseInsensitiveString "newTaxa")))
        _         <- optionMaybe (try (symbol (caseInsensitiveString "nTax")))
        _         <- optionMaybe $ try (symbol (char '='))
        numTaxa   <- optionMaybe $ try (symbol integer)
        _         <- optionMaybe $ symbol (caseInsensitiveString "nchar")
        _         <- oprionMaybe $ symbol (char '=')
        charCount <- optionMaybe $ try (symbol integer)
        _         <- symbol $ char ';'
        pure $ DimensionsFormat (newTaxa /= Nothing) 
                                (if numTaxa == Nothing then 0 else read (fromJust numTaxa) :: Int)
                                (if charCount == Nothing then 0 else read (fromJust charCount) :: Int)
    where
        plus    = char '+' *> number
        minus   = char '-' <:> number
        number  = many1 digit
        integer = plus <|> minus <|> number

formatDefinition :: Parsec String u CharacterFormat
formatDefinition = do
        _                       <- symbol (caseInsensitiveString "format")
        (p,q,r,s,t,u,v,w,x,y,z) <- partitionCharFormat <$> charFormatFieldDef
        _                       <- symbol $ char ';'
        pure $ CharacterFormat p q r s t u v w x y z

charFormatFieldDef :: Parsec String u [CharFormatField]
charFormatFieldDef = do
        block' <- many block
        pure block'
    where
        block =  (CharDT <$> try (stringDefinition "datatype"))
             <|> (SymStr <$> try (quotedStringDefinition "symbols"))
             <|> (Transpose <$> try (booleanDefinition "transpose"))
             <|> (Interleave <$> try (booleanDefinition "interleave"))
             <|> (Tokens <$> try (booleanDefinition "tokens"))
             <|> (EqStr <$> try (quotedStringDefinition "equate"))
             <|> (MissStr <$> try (stringDefinition "missing"))
             <|> (GapChar <$> try (stringDefinition "gap"))
             <|> (MatchChar <$> try (stringDefinition "matchcar"))
             <|> (Items <$> try (stringDefinition "items"))
             <|> (RespectCase <$> try (booleanDefinition "respectcase"))
             <|> (IgnFF <$> try (ignoredSubBlockDef ' '))

treeFieldDef :: Parsec String u TreeField
treeFieldDef = do
        block' <- block
        pure block'
    where
        block =  (Translation <$> try (delimitedStringListDefinition "translate" ','))
             <|> (Tree <$> try treeDefinition)
             <|> (IgnTF <$> try (ignoredSubBlockDef ';'))
        


booleanDefinition :: String -> Parsec String u Bool
booleanDefinition blockTitle = do
    title <- symbol (caseInsensitiveString blockTitle)
    _     <- symbol $ char ';'
    pure $ ((map toUpper title) == (map toUpper blockTitle))

stringDefinition :: String -> Parsec String u String
stringDefinition blockTitle = do
    _     <- symbol (caseInsensitiveString blockTitle)
    _     <- symbol $ char '='
    value <- symbol $ many (noneOf "; \t\"")
    _     <- symbol $ char ';'
    pure $ value

quotedStringDefinition :: String -> Parsec String u String
quotedStringDefinition blockTitle = do
    _     <- symbol (caseInsensitiveString blockTitle)
    _     <- symbol $ char '='
    _     <- symbol $ char '"'
    value <- symbol $ many (noneOf "\";")
    _     <- symbol $ char '"'
    _     <- symbol $ char ';'
    pure $ value

stringListDefinition :: String -> Parsec String u [String]
stringListDefinition label = do
        _        <- symbol (caseInsensitiveString label)
        theItems <- (symbol itemDefinition) `manyTill` symbol (char ';')
        pure $ theItems
    where
        itemDefinition = many1 $ noneOf " \n\r\t\f\v;"

delimitedStringListDefinition :: String -> Char -> Parsec String u [String]
delimitedStringListDefinition label delimiter = do
    _        <- symbol (caseInsensitiveString label)
    theItems <- many (noneOf $ delimiter : ";") `sepBy` (char delimiter)
    _        <- symbol $ char ';'
    pure $ theItems
    

treeDefinition :: Parsec String u (String, String)
treeDefinition = do
    _     <- symbol (caseInsensitiveString "tree")
    label <- symbol $ many (noneOf ";=")
    _     <- symbol $ char '='
    trees <- symbol $ many (noneOf ";")
    _     <- symbol $ char ';'
    pure (label, trees)

matrixDefinition :: Parsec String u String
matrixDefinition = do
    first     <- symbol (caseInsensitiveString "matrix")
    goodStuff <- many $ noneOf ";"
    _         <- symbol $ char ';'
    pure goodStuff

ignoredSubBlockDef :: Char -> Parsec String u String
ignoredSubBlockDef endChar = do
    title <- anyTill (symbol (caseInsensitiveString "end;") 
                      <|> symbol (caseInsensitiveString ";") 
                      <|> symbol (caseInsensitiveString [endChar])
                     )
    _     <- symbol $ char endChar -- didn't think I needed this, 
                                   -- but otherwise I get 
                                   -- "many applied to parser that accepts empty string"
    pure title

-- -------------------------------------------------------------------------------------------------
-- | Partitioning functions, which take a list of some type and produce a tuple.
-- Where there is a block with multiple optional fields or a field with multiple optional
-- subfields these take the output and put it into a tuple which can then be decomposed
-- and its fields used as arguments to a constructor.
-- I'm wondering if there's isn't a more efficient way to do this. 
-- Also, can these be reduced to a single function, since they're all doing the same thing?

partitionSequenceBlock :: [SeqSubBlock] -> ([String],[CharacterFormat],[DimensionsFormat],[String])
partitionSequenceBlock = foldr f ([],[],[],[])
    where
        f (Matrix n)     (v,w,x,y) = (n:v,   w,   x,   y)
        f (Format n)     (v,w,x,y) = (  v, n:w,   x,   y)
        f (Dims n)       (v,w,x,y) = (  v,   w, n:x,   y)
        f (Eliminate n)  (v,w,x,y) = (  v,   w,   x, n:y)
        f (Ignored n)             ws = ws

partitionNexusBlocks :: [NexusBlock] -> ([PhyloSequence], [TaxaSpecification], [TreeBlock])
partitionNexusBlocks = foldr f ([],[],[])
  where
    f (CharacterBlock n) (xs,ys,zs) = (n:xs,   ys,   zs)
    f (TaxaBlock n)      (xs,ys,zs) = (  xs, n:ys,   zs)
    f (TreesBlock n)     (xs,ys,zs) = (  xs,   ys, n:zs)
    f _                          ws = ws

partitionCharFormat :: [CharFormatField] -> (String, String, String, String, String, String, String, Bool, Bool, Bool, Bool)
partitionCharFormat = foldr f ("", "", "", "", "", "", "", False, False, False, False)
    where
        f (CharDT n)      (p,q,r,s,t,u,v,w,x,y,z) = (n,q,r,s,t,u,v,w,x,y,z)
        f (SymStr n)      (p,q,r,s,t,u,v,w,x,y,z) = (p,n,r,s,t,u,v,w,x,y,z)
        f (EqStr n)       (p,q,r,s,t,u,v,w,x,y,z) = (p,q,n,s,t,u,v,w,x,y,z)
        f (MissStr n)     (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,n,t,u,v,w,x,y,z)
        f (GapChar n)     (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,n,u,v,w,x,y,z)
        f (MatchChar n)   (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,t,n,v,w,x,y,z)
        f (Items n)       (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,t,u,n,w,x,y,z)
        f (RespectCase n) (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,t,u,v,n,x,y,z)
        f (Tokens n)      (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,t,u,v,w,n,y,z)
        f (Transpose n)   (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,t,u,v,w,x,n,z)
        f (Interleave n)  (p,q,r,s,t,u,v,w,x,y,z) = (p,q,r,s,t,u,v,w,x,y,n)
        f (IgnFF n)                            ws = ws

partitionTreeBlock :: [TreeField] -> ([[String]], [(String,String)])
partitionTreeBlock = foldr f ([],[])
    where
        f (Translation n) (ys,zs) = (n:ys,   zs)
        f (Tree n)        (ys,zs) = (  ys, n:zs)
        f _                    ws = ws


trimmed :: Parsec String u a -> Parsec String u a
trimmed x = whitespace *> x <* whitespace

symbol :: Parsec String u a -> Parsec String u a
symbol x = x <* whitespace

whitespace :: Parsec String u ()
whitespace = (many1 (commentDefinition) *> pure ())
          <|> spaces 
          <?> "white space"

commentDefinition :: Parsec String u String
commentDefinition = commentDefinition' False
    where 
        commentContent = many (noneOf "[]")
        commentDefinition' enquote = do
            _        <- char '[' <?> "\"[\" to begin a comment definition"
            before   <- commentContent
            comments <- many (commentDefinition' True <++> commentContent)
            _        <- char ']' <?> "\"]\" to correctly close comment"
            _        <- spaces
            pure . concat $
                if enquote
                then "[" : before : comments ++ ["]"]
                else       before : comments


spaces1 :: Parsec String u ()
spaces1 = space *> spaces

lstrip :: String -> String
lstrip "" = ""
lstrip input = dropWhile (\x -> x `elem` " \t\n\r") input

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip