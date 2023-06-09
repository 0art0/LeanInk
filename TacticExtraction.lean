import LeanInk.Analysis
import LeanInk.Logger

/-! Code for tactic step data extraction from `Mathlib`. -/

def main : IO Unit := do
  for file in ← System.FilePath.walkDir "./lake-packages/mathlib/Mathlib" do
    if file.extension = some "lean" then
      let task ← IO.asTask <| LeanInk.Analysis.execAux file.toString
      let _ ← IO.wait task