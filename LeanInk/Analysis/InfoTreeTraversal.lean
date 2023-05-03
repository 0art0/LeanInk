import Init
import Lean
import LeanInk.Configuration

namespace LeanInk.Analysis

open Lean Lean.Elab Lean.Meta IO
set_option autoImplicit false

structure AnalysisResult where
  tokens : List Token
  sentences : List Sentence

def AnalysisResult.empty : AnalysisResult := { tokens := [], sentences := [] }

def AnalysisResult.merge (x y : AnalysisResult) : AnalysisResult := {
    tokens := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.tokens y.tokens
    sentences := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.sentences y.sentences
  }

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : AnalysisResult := AnalysisResult.empty) : InfoTree → IO AnalysisResult
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree -- TODO Fix
  | InfoTree.node info children => do
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM <| _resolveTacticList ctx? aux
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl .merge .empty
      pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : AnalysisResult)
| error (e : IO.Error)

def _resolveTask (tree : InfoTree) : AnalysisM (Task TraversalEvent) := do
  let taskBody : AnalysisM TraversalEvent :=
    _resolveTacticList none .empty tree >>= pure ∘ .result
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
    return auxResults.foldl .merge .empty
  | _ => return { tokens := [], sentences := [] }
