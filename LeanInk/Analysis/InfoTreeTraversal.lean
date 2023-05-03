import Init.System.IO
import Init.Control.Except

import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.SemanticToken

import LeanInk.ListUtil
import LeanInk.Configuration

import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis

open Lean Lean.Elab Lean.Meta IO

set_option autoImplicit false

#check Meta.ppGoal

/- Traversal -/
structure AnalysisResult where
  tokens : List Token
  sentences : List Sentence
  deriving Inhabited

namespace AnalysisResult

  def empty : AnalysisResult := { tokens := [], sentences := [] }

  def merge (x y : AnalysisResult) : AnalysisResult := {
    tokens := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.tokens y.tokens
    sentences := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.sentences y.sentences
  }

end AnalysisResult

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : AnalysisResult := AnalysisResult.empty) (tree : InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  match tree with
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree -- TODO Fix
  | InfoTree.node info children =>
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
  let taskBody : AnalysisM TraversalEvent := do
    let res ← _resolveTacticList none .empty tree
    return TraversalEvent.result res
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | Except.ok ev => ev
    | Except.error e => TraversalEvent.error e

def _resolve (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  let auxResults ← (trees.map <| _resolveTacticList none .empty).mapM id
  return auxResults.foldl AnalysisResult.merge AnalysisResult.empty

def resolveTasks (tasks : Array (Task TraversalEvent)) : AnalysisM (Option (List AnalysisResult)) := do
  let mut results : List AnalysisResult := []
  for task in tasks do
    let result ← BaseIO.toIO (IO.wait task)
    match result with
    | TraversalEvent.result r => results := r::results
    | _ => return none
  return results

def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  let tasks ← trees.toArray.mapM _resolveTask
  match (← resolveTasks tasks) with
  | some auxResults => do
    return auxResults.foldl AnalysisResult.merge AnalysisResult.empty
  | _ => return { tokens := [], sentences := []}
