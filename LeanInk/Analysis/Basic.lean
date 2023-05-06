import LeanInk.Configuration
import LeanInk.Annotation.DataTypes
import LeanInk.Annotation.Alectryon
import LeanInk.Logger
import LeanInk.CLI

import LeanInk.Analysis.Analysis

import Lean.Util.Path

namespace LeanInk.Analysis

open LeanInk.Annotation
open LeanInk.CLI
open Lean
open System

private def _buildConfiguration (arguments: List ResolvedArgument) (file: FilePath) : IO Configuration := do
  let contents ← IO.FS.readFile file
  return {
    inputFilePath := file
    inputFileContents := contents
    lakeFile := getLakeFile? arguments
  }
where
  getLakeFile? (arguments : List ResolvedArgument) : Option FilePath :=
    match environmentValue arguments "--lake" with
    | none => none
    | some string => some (FilePath.mk string)

def annotateFile (analysis : List Tactic) : AnalysisM (List Annotation) := matchCompounds <| toFragmentIntervals analysis

def runAnalysis (output : Output) : AnalysisM UInt32 := do
  let config ← read
  logInfo s!"Starting process with lean file: {config.inputFileName}"
  logInfo "Analyzing..."
  let result ← analyzeInput config
  logInfo "Annotating..."
  let annotation ← annotateFile result
  logInfo "Outputting..."
  return ← output.genOutput annotation

-- EXECUTION
def execAuxM : AnalysisM UInt32 := do
  return ← runAnalysis {
    name := "Alectryon"
    genOutput := Alectryon.genOutput
  }

def execAux (args: List ResolvedArgument) (file: String) : IO UInt32 := do
  if ! (file : System.FilePath).extension == "lean" then do
    Logger.logError s!"Provided file \"{file}\" is not lean file."
  else
    IO.println s!"Starting Analysis for: \"{file}\""
    let config ← _buildConfiguration args file
    return ← (execAuxM.run config)

/-
`enableInitializersExecution` is usually only run from the C part of the
frontend and needs to be used with care but it is required in order
to work with custom user extensions correctly.
-/
@[implemented_by enableInitializersExecution]
private def enableInitializersExecutionWrapper : IO Unit := pure ()

def exec (args: List ResolvedArgument) : List String -> IO UInt32
  | [] => do Logger.logError s!"No input files provided"
  | files => do
    enableInitializersExecutionWrapper
    -- Span task for every file?
    for file in files do
      if (← execAux args file) != 0 then
        return ← Logger.logError s!"Analysis for \"{file}\" failed!"
    return 0
