import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath IO IO.FS

package leanInk where
  precompileModules := true

lean_lib LeanInk where

@[default_target]
lean_exe leanInk where
  root := `LeanInk
  supportInterpreter := true

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"@"master"