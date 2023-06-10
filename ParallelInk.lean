import Lean

open System IO IO.Process

def main (args : List String) : IO Unit := do
  if !(← FilePath.pathExists "TacticExtractionData") then  
    FS.createDir "TacticExtractionData"

  let [path] := args | IO.throwServerError "The `parallelInk` script takes only a single argument."
  let path : FilePath := "lake-packages/mathlib/Mathlib/" ++ path
  if ← FilePath.isDir path then
    let files := (← FilePath.walkDir path).filter (·.extension = some "lean")
    let tasks : Array (Child {}) ← files.mapM (spawn { cmd := "./build/bin/leanInk", args := #[·.toString] })
    for (file, task) in (Array.zip files tasks) do
      if (← task.wait) != 0 then
        IO.println s!"Error in processing {file.toString}."
  else IO.println <| ← run { cmd := "./build/bin/leanInk", args := #[path.toString]}

  IO.println "\n\n Tactic extraction complete." 