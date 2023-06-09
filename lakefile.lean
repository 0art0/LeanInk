import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath IO IO.FS

package leanInk where

lean_lib LeanInk where

@[default_target]
lean_exe leanInk where
  root := `LeanInk
  supportInterpreter := true