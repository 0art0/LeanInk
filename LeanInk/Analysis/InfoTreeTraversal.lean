import Init
import Lean
import LeanInk.Configuration

namespace LeanInk.Analysis

open Lean Lean.Elab Lean.Meta IO
set_option autoImplicit false

abbrev AnalysisResult := List String

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : AnalysisResult := .nil) : InfoTree → IO AnalysisResult
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree -- TODO Fix
  | InfoTree.node info children => do
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM <| _resolveTacticList ctx? aux
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl .append .nil
      pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : AnalysisResult)
| error (e : IO.Error)

def _resolveTask (tree : InfoTree) : AnalysisM (Task TraversalEvent) := do
  let taskBody : AnalysisM TraversalEvent :=
    _resolveTacticList none .nil tree >>= pure ∘ .result
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | .ok ev => ev
    | .error e => .error e

def resolveTasks (tasks : List (Task TraversalEvent)) : IO <| Option (List AnalysisResult) := do
  let mut results : List AnalysisResult := []
  for task in tasks do
    let result ← BaseIO.toIO <| IO.wait task
    match result with
    | .result r => results := r :: results
    | _ => return none
  return results

def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let tasks ← trees.mapM _resolveTask
  match (← resolveTasks tasks) with
  | some auxResults =>
    return auxResults.foldl .append .nil
  | _ => return .nil
