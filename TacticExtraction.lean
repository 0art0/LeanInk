import LeanInk.Analysis
import LeanInk.Logger

/-! Code for tactic step data extraction from `Mathlib`. -/

def main : IO Unit := do
  if !(← System.FilePath.pathExists "TacticExtractionData") then  
    IO.FS.createDir "TacticExtractionData"

  let files := (← System.FilePath.walkDir "./lake-packages/mathlib/Mathlib").filter (·.extension = some "lean")
  let tasks ← files.mapM (IO.asTask <| LeanInk.Analysis.execAux ·.toString)
  
  let mut count := 0
  let total := files.size
  
  for task in tasks do
    IO.println s!"File {count} out of {total}."
    let _ ← IO.wait task
    count := count + 1