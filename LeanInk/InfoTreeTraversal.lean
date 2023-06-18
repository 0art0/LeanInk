import Init
import Lean
import LeanInk.DataTypes

namespace LeanInk
open Lean Elab Meta IO

/-- Generates a `TacticFragment` from a `TacticInfo`. -/
def genFragment (ctx : ContextInfo) (info : TacticInfo) : IO TacticFragment := do
  let goalsBefore ← genGoals ctx info true
  let goalsAfter ← genGoals ctx info false
  return { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, 
                goalsBefore := goalsBefore, goalsAfter := goalsAfter }
where
  genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode : Bool) : IO (List String) :=
    if beforeNode then _genGoals ctx info.goalsBefore info.mctxBefore
    else _genGoals ctx info.goalsAfter info.mctxAfter
  _genGoals (ctx : ContextInfo) (goals : List MVarId) (metaCtx : MetavarContext) : IO (List String) := 
    { ctx with mctx := metaCtx }.runMetaM {} <| goals.mapM evalGoal >>= List.filterMapM pure
  evalGoal (mvarId : MVarId) : MetaM (Option String) := (some ∘ toString) <$> ppGoal mvarId

/-- Generates and inserts a tactic fragment into a list of `TacticFragment`s. -/
def insertFragment (fragments : Array TacticFragment) (ctx : ContextInfo) (info : TacticInfo) :=
  if fragments.any (fun t ↦ t.headPos == info.stx.getPos? && t.tailPos == info.stx.getTailPos?) then 
    return fragments
  else 
    fragments.push <$> genFragment ctx info

/-- Generates a list of `TacticFragment`s by recursively traversing the given `InfoTree`. -/
partial def _resolveTactics (ctx? : Option ContextInfo := none) (aux : Array TacticFragment := #[]) : InfoTree → IO (Array TacticFragment)
  | .context ctx tree => _resolveTactics ctx aux tree
  | .node info children =>
    match ctx? with
    | some ctx => do
      let sortedChildrenLeafs ← children.toArray.concatMapM <| _resolveTactics (info.updateContext? ctx) aux 
      if info.stx.isExpanded then
        return sortedChildrenLeafs
      else match info with
      | .ofTacticInfo tacticInfo => insertFragment sortedChildrenLeafs ctx tacticInfo
      | _ => return sortedChildrenLeafs
    | none => return aux
  | _ => return aux

/-- Concurrently resolves the tactics for all the `InfoTree`s in the specified array. -/
def resolveTactics (trees : Array InfoTree) : IO <| Array TacticFragment := do
  let tasks ← BaseIO.toIO <| trees.mapM <| IO.asTask ∘ _resolveTactics none #[]
  let mut results := #[]
  for task in tasks do
    let tacs ← IO.wait task >>= IO.ofExcept
    results := results ++ tacs
  return results