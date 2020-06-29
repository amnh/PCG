# How-to: Add a new command to PCG

### Step 1
Create a new module beneath `PCG.Command` within the `language` library. For example:

```
$ emacs lib/language/src/PCG/Command/Version.hs
```

### Step 2
Within your new module, define the data types containing all the information required to *evaluate* your command.

```haskell
newtype VersionCommand = 
        VersionCommand 
        { fullVersion :: Bool 
        }
```

### Step 3
Within the same module, we will use the combinators provided by the `PCG.Syntax.Combinators` module to *embedded* our command data type inside the Free Applicative structure used to automatically generate the script parser.

```haskell
versionCommandSpecification :: CommandSpecification VersionCommand
versionCommandSpecification = command "version" . argList $ version
  where
    version = VersionCommand <$> options `withDefault` False
    options = choiceFrom [ value "full" $> True, value "short" $> False] 
```

### Step 4
Add our new command to the PCG script parser in the `PCG.Syntax.Parser` module. To do this we must edit the `Command` sum-type to include our new command.

```haskell
data  Command
    = BUILD   !BuildCommand
    | ECHO    !EchoCommand
    | LOAD    !LoadCommand
    | READ    !ReadCommand
    | REPORT  !ReportCommand
    | SAVE    !SaveCommand
    | VERSION !VersionCommand -- Added our new command here
    deriving stock (Show)
```

### Step 5
Within the same `PCG.Syntax.Parser`module, we must also add our new embedded command to the`commandStreamParser`.

```haskell
commandStreamParser = whitespace *> choice
    [ BUILD   <$> parseCommand   buildCommandSpecification
    , ECHO    <$> parseCommand    echoCommandSpecification
    , READ    <$> parseCommand    readCommandSpecification
    , REPORT  <$> parseCommand  reportCommandSpecification
    , SAVE    <$> parseCommand    saveCommandSpecification
    , LOAD    <$> parseCommand    loadCommandSpecification
    , VERSION <$> parseCommand versionCommandSpecification -- Added to the PCG Script parser!
    ] <* whitespace
```

Great! now we have added our new command to the PCG scripting language! However, in order for PCG *run* our new command, we need to also add an evaluator for it.

### Step 6
Create a new module `PCG.Command.Version.Evaluate` within the `pcg` executable. For example:

```
$ emacs app/pcg/PCG/Command/Version/Evaluate.hs
```

### Step 7

Within your new module, define a function `evaluate` which pattern matches on your new command type, takes the current `GraphState` and produces a new `SearchState`.

```haskell
evaluate :: VersiomCommand -> GraphState -> SearchState
evaluate (VersionCommand printFullVersion) graphState =
    liftIO putStrLn versionStr $> graphState
  where
    versionStr
      | printFullVersion = fullVersionInformation
      | otherwise        = shortVersionInformation
```

### Step 8
Now that we have an evaluator defined for our new command, we need to add this evaluator to the evaluation loop, located in the `evaluate` function of the `PCG.Computation.Internal` module.

```haskell
evaluate = ...
  where
    f :: SearchState -> Command -> SearchState
    f s = \case
             BUILD   c -> s >>=   Build.evaluate c
             ECHO    c -> s >>=    Echo.evaluate c
             LOAD    c -> s *>     Load.evaluate c
             READ    c -> s *>     Read.evaluate c
             REPORT  c -> s >>=  Report.evaluate c
             SAVE    c -> s >>=    Save.evaluate c
             VERSION c -> s >>= Version.evaluate c -- Added to the evaluation loop here
```
