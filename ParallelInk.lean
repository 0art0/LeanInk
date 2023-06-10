import Lean

open System IO IO.Process

def extractTacticDataFromFiles (files : Array FilePath) : IO (Array FilePath) := do
  let mut failures : Array FilePath := #[]

  let tasks : Array (Child {}) ← files.mapM (spawn { cmd := "./build/bin/leanInk", args := #[·.toString] })
  for (file, task) in (Array.zip files tasks) do
    if (← task.wait) != 0 then
      failures := failures.push file
      IO.println s!"Error in processing {file.toString}."

  return failures

def main (args : List String) : IO Unit := do
  if !(← FilePath.pathExists "TacticExtractionData") then  
    FS.createDir "TacticExtractionData"

  let [n, path] := args | IO.throwServerError "The `parallelInk` script takes in a single number and a file path."
  let iters : Nat := n.toNat!
  let path : FilePath := "lake-packages/mathlib/Mathlib/" ++ path
  
  if ← FilePath.isDir path then

    let mut files := (← FilePath.walkDir path).filter (·.extension = some "lean")

    for i in [0:iters] do
      IO.println s!"\n\nIteration {i} of tactic extraction ..."
      let failures ← extractTacticDataFromFiles files
      if !failures.isEmpty then
        let fileName := s!"failures-{i}.txt"
        IO.FS.writeFile fileName <| failures.foldl (fun x y ↦ x ++ "\n" ++ (toString y)) ""
        IO.println s!"Failures written to {fileName}."
      
      files := failures
      

  else 
    IO.println <| ← run { cmd := "./build/bin/leanInk", args := #[path.toString]}

  IO.println "\n\nTactic extraction complete." 