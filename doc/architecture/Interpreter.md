
# Interpreter
---

### Related packages:
 * `pcg`
 * `evaluation`
 * `language`
 * `validation-transformer`

### Related modules:

 - **`PCG.Command.Build`**
 - **`PCG.Command.Build.Evaluate`**
 - **`PCG.Command.Echo`**
 - **`PCG.Command.Echo.Evaluate`**
 - **`PCG.Command.Load`**
 - **`PCG.Command.Load.Evaluate`**
 - **`PCG.Command.Read`**
 - **`PCG.Command.Read.Evaluate`**
 -  *`PCG.Command.Read.InputStreams`*
 -  *`PCG.Command.Read.ParseStreams`*
 -  *`PCG.Command.Read.ReadCommandError`*
 - **`PCG.Command.Report`**
 -  *`PCG.Command.Report.Distance`*
 - **`PCG.Command.Report.Evaluate`**
 -  *`PCG.Command.Report.GraphViz`*
 -  *`PCG.Command.Report.ImpliedAlignment`*
 -  *`PCG.Command.Report.Metadata`*
 - **`PCG.Command.Save`**
 - **`PCG.Command.Save.Evaluate`**
 - **`PCG.Computation.Internal`**
 - **`Control.Evaluation`**
 -  *`Control.Evaluation.Notification`*
 -  *`Control.Evaluation.Result`*
 - **`Control.Evaluation.Trans`**
 -  *`Control.Monad.Trans.Validation`*
 - **`System.ErrorPhase`**

The interpreter for PCG is broadly broken up into two components, the evaluator and the computation. The evaluator constitutes the code for managing the working state of PCG between steps of the computation along with error handling and notification reporting. The evaluator defines how PCG runs. The command is a list of commands to be run sequentially. The computation defines what PCG runs.

The evaluator is built around the `EvaluationT` monad transformer, a newtyped `RWST`. It stores a `GlobalState` parameter and embeds actions in the `IO` monad. It has a specialized writer instance which keeps a sequence of notifications. It can represent a single failure state which occurred in one of five phases, short circuiting the computation as soon as the error state is entered. PCG uses the `EvaluationT` monad to process information through five phases:

| Phase      | Actions|
|------------|--------|
|`Inputing`  | attempts to read information off disk or over the network into the program
|`Parsing`   | takes read in information and attempts to intelligently interpret it
|`Unifying`  | takes the individually well-understood inputs and attempts to holistically assemble them
|`Computing` | performs the specified operations on the input data to produce a phylogenetic solution
|`Outputting`| takes the operating state of PCG and uses it to render information to disk or the terminal

Within each of the five phases, the `ValidationT` monad transformer is used to embed component actions within `IO` and collate all errors which occurred in a single phase. When used in conjunction with the `EvaluationT` monad transformer, this has the effect of PCG collecting all errors that potentially could occur in a single phase, then either reporting the errors and stopping the computation, or in the case of no errors in the given phase, continuing on to the next phase.

The notifications stored within an `EvaluationT` can be either informational or a warning. Additionally, a single piece of error information can be provided with the short-circuiting error state.

The computation is a nonempty list of commands, as specified by the PCG scripting language. These commands determine how data is input, output, and operated on by PCG. The `READ` and `REPORT` commands are the most complex as the handle the two crucial phases of the programs life cycle, the beginning and end. The PCG scripting language is designed to be extensible, allowing for more commands to be easily added at a later time.
