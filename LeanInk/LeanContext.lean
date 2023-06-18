import Lean

namespace LeanInk

open System Lean IO Elab

/-! ## Lean -/

def initializeLeanContext : IO Unit := do
  let leanPath ← Lean.findSysroot
  Lean.initSearchPath leanPath

/-! ## Lake -/

def lakeEnvName := "LAKE"
def lakeCmdName := "lake"
def lakePrintPathsCmd := "print-paths"

def getLakePath : IO String := do
  match (← IO.getEnv lakeEnvName) with
  | some path => return path
  | none => return lakeCmdName

def lakeFile : FilePath := "./lake-packages/mathlib/lakefile.lean"

def initializeLakeContext (lakeFile : FilePath) (header : Syntax) : IO Unit := do
  unless ← lakeFile.pathExists do
    IO.throwServerError s!"lakefile does not exist: {lakeFile}"
  
  if lakeFile.fileName != some "lakefile.lean" then
    match lakeFile.fileName with
    | none => IO.throwServerError s!"lakefile is not a valid file!"
    | some fileName => IO.throwServerError s!"lakefile [{fileName}] not called: lakefile.lean"
  else
    -- logInfo s!"Loading Lake Context with lakefile ({lakeFile})..."
    let imports := headerToImports header
    let arguments := #[lakePrintPathsCmd] ++ imports.map (toString ·.module)
    let lakeProcess ← Process.spawn {
      stdin := Process.Stdio.null
      stdout := Process.Stdio.piped
      stderr := Process.Stdio.inherit
      cmd := ← getLakePath
      args := arguments
    }
    let stdout := (← lakeProcess.stdout.readToEnd).trim
    match (← lakeProcess.wait) with
    | 0 => do
      let stdout := stdout.split (· == '\n') |>.getLast!
      match Json.parse stdout with
      | .error _ => IO.throwServerError s!"Failed to parse lake output: {stdout}"
      | .ok val => match fromJson? val with
        | .error _ => IO.throwServerError s!"Failed to decode lake output: {stdout}"
        | .ok (paths : LeanPaths) => do
          initializeLeanContext
          initSearchPath (← findSysroot) paths.oleanPath
          -- logInfo s!"{paths.oleanPath}"
          -- logInfo s!"Successfully loaded lake search paths"
    | 1 => IO.throwServerError "Exiting `lake` with error code 1."
    | 2 => IO.throwServerError s!"No search paths required!"
    | _ => IO.throwServerError s!"Using lake failed! Make sure that lake is installed!"
