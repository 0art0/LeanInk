import LeanInk.Analysis
import LeanInk.Logger

/-! Code for tactic step data extraction from `Mathlib`. -/

def main : IO Unit := do
  let files := (← System.FilePath.walkDir "./lake-packages/mathlib/Mathlib").filter (·.extension = some "lean")
  let tasks ← files.mapM (IO.asTask <| LeanInk.Analysis.execAux ·.toString)
  for task in tasks do
    let _ ← IO.wait task