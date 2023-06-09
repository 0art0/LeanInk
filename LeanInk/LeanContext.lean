import Lean.Util.Path
import Lean.Parser.Module
import Lean.Elab
import Lean.Util.Paths

import LeanInk.Logger

namespace LeanInk.Analysis

open System Lean IO

-- LEAN
def initializeLeanContext : IO Unit := do
  let leanPath ← Lean.findSysroot
  Lean.initSearchPath leanPath

-- LAKE
def lakeEnvName := "LAKE"
def lakeCmdName := "lake"
def lakePrintPathsCmd := "print-paths"

def getLakePath : IO String := do
  match (← IO.getEnv lakeEnvName) with
  | some path => return path
  | none => return lakeCmdName

def lakeFile : FilePath := "./lake-packages/mathlib/lakefile.lean"

def initializeLakeContext (lakeFile : FilePath) (header : Syntax) : IO Unit := do
  if !(← lakeFile.pathExists) then
    throw <| IO.userError s!"lakefile does not exist: {lakeFile}"
  else if lakeFile.fileName != some "lakefile.lean" then
    match lakeFile.fileName with
    | none => throw <| IO.userError s!"lakefile is not a valid file!"
    | some fileName => throw <| IO.userError s!"lakefile [{fileName}] not called: lakefile.lean"
  else
    logInfo s!"Loading Lake Context with lakefile ({lakeFile})..."
    let imports := Lean.Elab.headerToImports header
    let arguments := #[lakePrintPathsCmd] ++ imports.map (toString ·.module)
    let lakeProcess ← Process.spawn {
      stdin := Process.Stdio.null
      stdout := Process.Stdio.piped
      stderr := Process.Stdio.inherit
      cmd := ← getLakePath
      args := arguments
    }
    let stdout := String.trim (← lakeProcess.stdout.readToEnd)
    match (← lakeProcess.wait) with
    | 0 => do
      let stdout := stdout.split (· == '\n') |>.getLast!
      match Json.parse stdout with
      | Except.error _ => throw <| IO.userError s!"Failed to parse lake output: {stdout}"
      | Except.ok val => match fromJson? val with
        | Except.error _ => throw <| IO.userError s!"Failed to decode lake output: {stdout}"
        | Except.ok (paths : LeanPaths) => do
          initializeLeanContext
          initSearchPath (← findSysroot) paths.oleanPath
          logInfo s!"{paths.oleanPath}"
          logInfo s!"Successfully loaded lake search paths"
    | 2 => logInfo s!"No search paths required!"
    | _ => throw <| IO.userError s!"Using lake failed! Make sure that lake is installed!"
