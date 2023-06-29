import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath IO IO.FS

package leanInk where
  precompileModules := true

lean_lib LeanInk where

lean_lib Scripts where

@[default_target]
lean_exe leanInk where
  root := `LeanInk
  supportInterpreter := true

@[default_target]
lean_exe depGen where
  root := `Scripts.DependencyGen
  supportInterpreter := true  

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"@"master"

script mkImports do
  let ws ← Lake.getWorkspace
  let [pkg] := ws.packageList.filter (·.dir = ⟨"."⟩) | throw <| IO.userError "Current package not found"
  for (libName, _) in pkg.leanLibConfigs do
    let dir ← FilePath.walkDir libName.toString >>= Array.filterM (not <$> ·.isDir)
    let filePathToImport : FilePath → String := fun fp ↦ fp.toString.takeWhile (· != FilePath.extSeparator) |>.map <| 
      fun c ↦ if c = FilePath.pathSeparator then '.' else c
    let imports := dir.foldl (init := "") <| fun s f ↦ s ++ s!"import {filePathToImport f}\n" 
    IO.FS.writeFile (libName.toString ++ ".lean") imports

  return 1