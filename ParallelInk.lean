import Lean

open System IO IO.Process

def main : IO Unit := do
  if !(← FilePath.pathExists "TacticExtractionData") then  
    FS.createDir "TacticExtractionData"

  let dir := "./lake-packages/mathlib/Mathlib/ModelTheory"
  let files := (← FilePath.walkDir dir).filter (·.extension = some "lean")
  let tasks : Array (Child {}) ← files.mapM (spawn { cmd := "./build/bin/leanInk", args := #[·.toString] })
  for (file, task) in (Array.zip files tasks) do
    if (← task.wait) != 0 then
      IO.println s!"Error in processing {file.toString}."
  IO.println "\n\n Tactic extraction complete." 