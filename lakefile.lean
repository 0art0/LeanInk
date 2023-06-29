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

def getCurrentPackage : ScriptM Package := do
  let ws ← Lake.getWorkspace
  let [pkg] := ws.packageList.filter (·.dir = ⟨"."⟩) | throw <| IO.userError "Current package not found"
  return pkg

def importsForLib (lib : Name) : IO String := do
  let dir ← FilePath.walkDir lib.toString >>= Array.filterM (not <$> ·.isDir)
  let filePathToImport : FilePath → String := fun fp ↦ fp.toString.takeWhile (· != FilePath.extSeparator) |>.map <| 
    fun c ↦ if c = FilePath.pathSeparator then '.' else c
  let imports := dir.foldl (init := "") <| fun s f ↦ s ++ s!"import {filePathToImport f}\n"
  return imports

script import_all do
  let pkg ← getCurrentPackage 
  IO.println s!"Creating imports for package {pkg.name} ...\n"
  for (lib, _) in pkg.leanLibConfigs do
    let fileName : FilePath := lib.toString ++ ".lean"
    let imports ← importsForLib lib
    IO.FS.writeFile fileName imports
    IO.println s!"Created imports file for {lib} library."
  return 0

script import_all? do
  let pkg ← getCurrentPackage
  IO.println s!"Checking imports for package {pkg.name} ...\n"
  for (lib, _) in pkg.leanLibConfigs do
    let fileName : FilePath := lib.toString ++ ".lean"
    let allImports ← importsForLib lib 
    let existingImports ← IO.FS.readFile fileName
    unless existingImports = allImports do
      IO.eprintln s!"Invalid import list for {lib} library."
      IO.eprintln s!"Try running `lake run mkImports`."
      return 1
  IO.println s!"The imports for package {pkg.name} are up to date."
  return 0
