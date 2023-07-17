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


section Scripts

open System

partial def moduleNamesIn (dir : FilePath) (ext := "lean") : IO (Array Name) :=
  dir.readDir >>= Array.concatMapM fun entry ↦ do
    if (← entry.path.isDir) then
      let n := entry.fileName.toName
      let mods ← Array.map (n ++ ·) <$> moduleNamesIn entry.path ext
      if mods.isEmpty then
        return #[]
      else
        return mods.push n
    else if entry.path.extension == some ext then
      return #[FilePath.withExtension entry.fileName "" |>.toString.toName]
    else return #[]

def importsForLib (dir : FilePath) (root : Name) : IO String := do
  moduleNamesIn (dir / root.toString) >>=
    Array.mapM (return .mkSimple root.toString ++ ·) >>=
    Array.foldlM (init := "") fun imports fileName ↦
      return imports ++ s!"import {fileName.toString}\n"

script import_all do
  let pkg ← Workspace.root <$> getWorkspace
  IO.println s!"Creating imports for package {pkg.name} ...\n"
  for lib in pkg.leanLibs do
    for root in lib.config.roots do
      let dir := lib.srcDir.normalize
      let fileName : FilePath := dir / (root.toString ++ ".lean")
      let imports ← importsForLib dir root
      IO.FS.writeFile fileName imports
      IO.println s!"Created imports file {fileName} for {root} library."
  return 0

script import_all? do
  let pkg ← Workspace.root <$> getWorkspace
  IO.println s!"Checking imports for package {pkg.name} ...\n"
  for lib in pkg.leanLibs do
    for root in lib.config.roots do
      let dir := lib.srcDir.normalize
      let fileName : FilePath := dir / (root.toString ++ ".lean")
      let allImports ← importsForLib dir root
      let existingImports ← IO.FS.readFile fileName
      unless existingImports == allImports do
        IO.eprintln s!"Invalid import list for {root} library."
        IO.eprintln s!"Try running `lake run mkImports`."
        return 1
  IO.println s!"The imports for package {pkg.name} are up to date."
  return 0

end Scripts
