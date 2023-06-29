import Lean
open Lean

def Lean.Environment.getModuleNameFor? (env : Environment) (nm : Name) : Option Name := do
  let idx ← env.getModuleIdxFor? nm
  env.header.moduleNames[idx.toNat]?

def createMathlibEnv : IO Environment := do
  initSearchPath (← getBuildDir) ["build/lib", "lake-packages/mathlib/build/lib/",  "lake-packages/std/build/lib/", "lake-packages/Qq/build/lib/", "lake-packages/aesop/build/lib/", "lake-packages/proofwidgets/build/lib/"]
  importModules [{module := `Init}, {module := `Mathlib}] .empty

def isValidMathlibConstant (mathlibEnv : Environment) (nm : Name) : Bool := Option.toBool (α := Unit) <| do
  guard <| ! nm.isImplementationDetail
  guard <| ! nm.isInaccessibleUserName
  guard <| ! nm.isInternal 
  let mod ← mathlibEnv.getModuleNameFor? nm
  guard <| (`Mathlib).isPrefixOf mod
  return ()

def mathlibDepsOf (mathlibEnv : Environment) (nm : Name) : IO <| Array (Name × Name) := do  
  let some constinfo := mathlibEnv.constants.find? nm | IO.throwServerError s!"The constant {nm} is not in the `Mathlib` environment."
  let some term := constinfo.value? | do
    IO.println s!"No value found for the term {nm}."
    return #[]
  return term.foldConsts #[] <| fun c consts ↦
    if isValidMathlibConstant mathlibEnv c then
      -- the `get!` is justified because the declaration is in `Mathlib`
      consts.push (mathlibEnv.getModuleNameFor? c |>.get!, c)
    else consts

def main : IO Unit := do
  IO.println "Starting analysis ..."
  let env ← createMathlibEnv
  IO.println "Loaded environment."

  let folder : System.FilePath := "DependencyData"
  
  unless ← folder.isDir do
    IO.FS.createDir folder
    IO.println s!"Created folder {folder} for data extraction."

  let tasks ← env.constants.toList.mapM <| fun (nm, _) ↦ IO.asTask <| do
    if isValidMathlibConstant env nm then
      let deps ← mathlibDepsOf env nm
      let data := deps.foldl (init := "") <| fun s (mod, dep) ↦ s ++ s!"{mod}, {dep}\n"
      IO.FS.writeFile (folder / (nm.toString ++ ".txt")) data
      IO.println s!"Written data for {nm}."

  for task in tasks do
    let _ ← IO.wait task