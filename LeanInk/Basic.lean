import LeanInk.DataTypes
import LeanInk.LeanContext
import LeanInk.InfoTreeTraversal

import Lean

namespace LeanInk

open Lean Elab System

/-! ## Analysis -/

/-- Extracts the list of `TacticFragmentWithContent`s from the specified file. -/
def analyzeInput (file : System.FilePath) (fileContents : String) : IO <| Array TacticFragmentWithContent := do
  -- Parse the header of the provided file
  let context := Parser.mkInputContext fileContents file.toString
  let (header, state, messages) ← Parser.parseHeader context
  -- Manually enable the `tactic.simp.trace` option
  let fileContents' := 
    fileContents.extract ⟨0⟩ state.pos ++ 
    "\nset_option tactic.simp.trace true\n" ++ 
    fileContents.extract state.pos fileContents.endPos
  let context' := Parser.mkInputContext fileContents' file.toString
  -- Load the imports
  initializeLakeContext lakeFile header
  let options := Options.empty |>.setBool `trace.Elab.info true |>.setBool `tactic.simp.trace true
  let (environment, messages) ← processHeader header options messages context' 0
  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        IO.throwServerError <| ← msg.toString
  -- Process the remaining file
  let commandState := { Command.mkState environment messages with infoState := { enabled := true } }
  let s ← IO.processCommands context' state commandState
  -- Resolve the list of tactics from the file's infotrees
  let result ← resolveTactics s.commandState.infoState.trees.toArray
  -- Load the messages in the file
  let messages := s.commandState.messages.msgs.toList.filter (·.endPos.isSome)
  -- Add content to the tactic fragments
  result.mapM <| TacticFragment.withContent fileContents' messages commandState.env
  
/-- Analyse and output the tactic fragment data to a file. -/
def runAnalysis (file : System.FilePath) (fileContents : String) : IO UInt32 := do
  let result ← analyzeInput file fileContents
  let rawContents := toJson result |>.compress
  IO.FS.writeFile s!"TacticExtractionData/{file.components.drop 3 |> String.intercalate "-"}.json" rawContents
  IO.println s!"Results of \"{file.toString}\" written to file."
  return 0

/-! ## Execution -/

/-- Load the content from the specified file and run the analysis. -/
def execAux (file : String) : IO UInt32 := do
  unless (file : System.FilePath).extension == "lean" do
    IO.throwServerError s!"Provided file \"{file}\" is not lean file."
  
  IO.println s!"Starting Analysis for: \"{file}\""
  let contents ← IO.FS.readFile file
  runAnalysis file contents
  
/-
`enableInitializersExecution` is usually only run from the C part of the
frontend and needs to be used with care but it is required in order
to work with custom user extensions correctly.
-/
@[implemented_by enableInitializersExecution]
private def enableInitializersExecutionWrapper : IO Unit := pure ()

/-- The main function for analysing files. -/
def exec : List String -> IO UInt32
  | [] => IO.throwServerError s!"No input files provided"
  | files => do
    enableInitializersExecutionWrapper
    -- Span task for every file?
    for file in files do
      unless (← execAux file) == 0 do
        IO.throwServerError s!"Analysis for \"{file}\" failed!"
    return 0