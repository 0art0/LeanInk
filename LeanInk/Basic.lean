import LeanInk.Logger
import LeanInk.DataTypes
import LeanInk.LeanContext
import LeanInk.InfoTreeTraversal
import LeanInk.Logger

import Lean.Util.Trace
import Lean.Util.Path

namespace LeanInk.Analysis

open Lean Elab System

def analyzeInput (file : System.FilePath) (fileContents : String) : IO (List TacticFragmentWithContent) := do
  let context := Parser.mkInputContext fileContents file.toString
  let (header, state, messages) ← Parser.parseHeader context
  let fileContents' := 
    fileContents.extract ⟨0⟩ state.pos ++ 
    "\nset_option tactic.simp.trace true\n" ++ 
    fileContents.extract state.pos fileContents.endPos
  let context' := Parser.mkInputContext fileContents' file.toString
  initializeLakeContext lakeFile header
  let options := Options.empty |>.setBool `trace.Elab.info true |>.setBool `tactic.simp.trace true
  let (environment, messages) ← processHeader header options messages context' 0
  logInfo s!"Header: {environment.header.mainModule}"
  logInfo s!"Header: {environment.header.moduleNames}"
  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        let _ ← logError <$> msg.toString
    throw <| IO.userError "Errors during import; aborting"
  let commandState := { Command.mkState environment messages with infoState := { enabled := true } }
  let s ← IO.processCommands context' state commandState
  let result ← resolveTacticList s.commandState.infoState.trees.toList
  let messages := s.commandState.messages.msgs.toList.filter (·.endPos.isSome)
  result.mapM <| TacticFragment.withContent fileContents' messages

def runAnalysis (file : System.FilePath) (fileContents : String) : IO UInt32 := do
  -- logInfo s!"Starting process with lean file: {config.inputFileName}"
  logInfo "Analyzing..."
  let result ← analyzeInput file fileContents
  logInfo "Outputting..."
  let rawContents := toJson result |>.pretty
  let dirEntry : IO.FS.DirEntry := { 
    root := ← IO.currentDir,
    fileName := file.toString ++ ".json"
  }
  IO.FS.writeFile dirEntry.path rawContents
  logInfo s!"Results written to file: {dirEntry.path}!"
  return 0

-- EXECUTION

def execAux (file : String) : IO UInt32 := do
  if ! (file : System.FilePath).extension == "lean" then do
    Logger.logError s!"Provided file \"{file}\" is not lean file."
  else
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

def exec : List String -> IO UInt32
  | [] => do Logger.logError s!"No input files provided"
  | files => do
    enableInitializersExecutionWrapper
    -- Span task for every file?
    for file in files do
      if (← execAux file) != 0 then
        return ← Logger.logError s!"Analysis for \"{file}\" failed!"
    return 0
