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

open Lean
open Lean.Elab
open Lean.Meta
open IO

structure InfoTreeContext where 
  hasSorry : Bool 
  isCalcTatic : Bool
deriving Repr

partial def _hasSorry (t : InfoTree) : Bool := 
  let rec go (ci? : Option ContextInfo) (t : InfoTree) : Bool :=
    match t with
    | InfoTree.context ci t => go ci t
    | InfoTree.node i cs =>
      if let (some _, .ofTermInfo ti) := (ci?, i) then 
        ti.expr.hasSorry
      else 
        cs.any (go ci?)
    | _ => false
  go none t

partial def _isCalcTatic (tree: InfoTree) : Bool :=
  match tree with
  | .context _ _ => false
  | .node i _ => 
    if let .ofTacticInfo ti := i then 
      ti.stx.getKind == ``calcTactic
    else 
      false
  | _ => false

--!
def _buildInfoTreeContext (config : Configuration) (tree : InfoTree) : InfoTreeContext := 
  let hasSorry := config.experimentalSorryConfig && (_hasSorry tree)
  let isCalcTatic := config.experimentalCalcConfig && (_isCalcTatic tree)
  InfoTreeContext.mk hasSorry isCalcTatic

def _updateIsCalcTatic (config : Configuration) (ctx : InfoTreeContext) (tree : InfoTree) : InfoTreeContext := 
  { ctx with isCalcTatic := if config.experimentalCalcConfig then ctx.isCalcTatic || _isCalcTatic tree else false }

structure ContextBasedInfo (β : Type) where
  ctx : ContextInfo
  info : β

inductive TraversalFragment where
| tactic (info: ContextBasedInfo TacticInfo)
| term (info: ContextBasedInfo TermInfo)
| field (info: ContextBasedInfo FieldInfo)
| unknown (info: ContextBasedInfo ElabInfo)

namespace TraversalFragment
  def headPos : TraversalFragment -> String.Pos
  | term fragment => (fragment.info.toElabInfo.stx.getPos? false).getD 0
  | field fragment => (fragment.info.stx.getPos? false).getD 0
  | tactic fragment => (fragment.info.toElabInfo.stx.getPos? false).getD 0
  | unknown fragment => (fragment.info.stx.getPos? false).getD 0

  def tailPos : TraversalFragment -> String.Pos
  | term fragment => (fragment.info.toElabInfo.stx.getTailPos? false).getD 0
  | field fragment => (fragment.info.stx.getTailPos? false).getD 0
  | tactic fragment => (fragment.info.toElabInfo.stx.getTailPos? false).getD 0
  | unknown fragment => (fragment.info.stx.getTailPos? false).getD 0

  def create (ctx : ContextInfo) (info : Info) : AnalysisM ((Option TraversalFragment) × (Option SemanticTraversalInfo)) := do
    if Info.isExpanded info then
      pure (none, none)
    else
      let mut semantic : Option SemanticTraversalInfo := none 
      if (← read).experimentalSemanticType then
        semantic := some { node := info, stx := info.stx }
      match info with 
      | Info.ofTacticInfo info => pure (tactic { info := info, ctx := ctx }, semantic)
      | Info.ofTermInfo info => pure (term { info := info, ctx := ctx }, semantic)
      | Info.ofFieldInfo info => pure (field { info := info, ctx := ctx }, semantic)
      | _ => pure (none, semantic)

  def runMetaM { α : Type } (func : TraversalFragment -> MetaM α) : TraversalFragment -> AnalysisM α
  | term fragment => fragment.ctx.runMetaM fragment.info.lctx (func (term fragment))
  | field fragment => fragment.ctx.runMetaM fragment.info.lctx (func (field fragment))
  | tactic fragment => fragment.ctx.runMetaM {} (func (tactic fragment))
  | unknown fragment => fragment.ctx.runMetaM {} (func (unknown fragment))

  /- Sentence Generation -/
  private def genGoal (goalType : Format) (hypotheses : List Hypothesis): Name -> MetaM (Goal)
    | Name.anonymous => do
      return { 
        name := ""
        conclusion := toString goalType
        hypotheses := hypotheses 
      }
    | name => do
      let goalFormatName := format name.eraseMacroScopes
      return { 
        name := toString goalFormatName
        conclusion := toString goalType
        hypotheses := hypotheses 
      }

  /-- 
  This method is a adjusted version of the Meta.ppGoal function. As we do need to extract the goal informations into seperate properties instead
  of a single formatted string to support the Alectryon.Goal datatype.
  -/
  private def evalGoal (mvarId : MVarId) (infoTreeCtx : InfoTreeContext) : MetaM (Option Goal) := do
    match (← getMCtx).findDecl? mvarId with
    | none => return none
    | some decl => do
      if infoTreeCtx.hasSorry || infoTreeCtx.isCalcTatic then 
        return none
      else
        let ppAuxDecls := pp.auxDecls.get (← getOptions)
        let ppImplDetailHyps := pp.implementationDetailHyps.get (← getOptions)
        let lctx := decl.lctx.sanitizeNames.run' { options := (← getOptions) }
        withLCtx lctx decl.localInstances do
          let pushPending (list : List Hypothesis) (type? : Option Expr) : List Name -> MetaM (List Hypothesis)
          | [] => pure list
          | ids => do
            match type? with
              | none      => pure list
              | some type => do
                let typeFmt ← ppExpr type
                let names := ids.reverse.map (λ n => n.toString)
                return list.append [{ names := names, body := "", type := s!"{typeFmt}" }]
          let evalVar (varNames : List Name) (prevType? : Option Expr) (hypotheses : List Hypothesis) (localDecl : LocalDecl) : MetaM (List Name × Option Expr × (List Hypothesis)) := do
              match localDecl with
              | LocalDecl.cdecl _ _ varName type _ _ =>
                let varName := varName.simpMacroScopes
                let type ← instantiateMVars type
                if prevType? == none || prevType? == some type then
                  pure (varName::varNames, some type, hypotheses)
                else do
                  let hypotheses ← pushPending hypotheses prevType? varNames
                  pure ([varName], some type, hypotheses)
              | LocalDecl.ldecl _ _ varName type val _ _ => do
                let varName := varName.simpMacroScopes
                let hypotheses ← pushPending hypotheses prevType? varNames
                let type ← instantiateMVars type
                let val  ← instantiateMVars val
                let typeFmt ← ppExpr type
                let valFmt ← ppExpr val
                pure ([], none, hypotheses.append [{ names := [varName.toString], body := s!"{valFmt}", type := s!"{typeFmt}" }])
          let (varNames, type?, hypotheses) ← lctx.foldlM (init := ([], none, [])) λ (varNames, prevType?, hypotheses) (localDecl : LocalDecl) =>
          if !ppAuxDecls && localDecl.isAuxDecl || !ppImplDetailHyps && localDecl.isImplementationDetail then
              pure (varNames, prevType?, hypotheses)
            else
              evalVar varNames prevType? hypotheses localDecl
          let hypotheses ← pushPending hypotheses type? varNames 
          let typeFmt ← ppExpr (← instantiateMVars decl.type)
          return (← genGoal typeFmt hypotheses decl.userName)

  private def _genGoals (contextInfo : ContextBasedInfo TacticInfo) (goals: List MVarId) (metaCtx: MetavarContext) (infoTreeCtx : InfoTreeContext) : AnalysisM (List Goal) := 
    let ctx := { contextInfo.ctx with mctx := metaCtx }
    return (← ctx.runMetaM {} (goals.mapM (fun x => evalGoal x infoTreeCtx))).filterMap id

  private def genGoals (contextInfo : ContextBasedInfo TacticInfo) (beforeNode: Bool) (infoTreeCtx : InfoTreeContext) : AnalysisM (List Goal) :=
    if beforeNode then
      _genGoals contextInfo contextInfo.info.goalsBefore contextInfo.info.mctxBefore infoTreeCtx
    else
      _genGoals contextInfo contextInfo.info.goalsAfter contextInfo.info.mctxAfter infoTreeCtx

  def genTactic? (self : TraversalFragment) (infoTreeCtx : InfoTreeContext) : AnalysisM (Option Tactic) := do
    match self with
    | tactic fragment => do 
      let goalsBefore ← genGoals fragment true infoTreeCtx
      let goalsAfter ← genGoals fragment false infoTreeCtx
      if infoTreeCtx.hasSorry || infoTreeCtx.isCalcTatic then do 
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := [], goalsAfter := [] }
      if goalsAfter.isEmpty then  
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := [{ name := "", conclusion := "Goals accomplished! 🐙", hypotheses := [] }] }
      else
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := goalsAfter }
    | _ => pure none

  def genSentences (self : TraversalFragment) (infoTreeCtx : InfoTreeContext) : AnalysisM (List Sentence) := do
    if let some t ← self.genTactic? infoTreeCtx then
      return [Sentence.tactic t]
    else
      return []
end TraversalFragment

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

  def Position.toStringPos (fileMap: FileMap) (pos: Lean.Position) : String.Pos :=
    FileMap.lspPosToUtf8Pos fileMap (fileMap.leanPosToLspPos pos)

  private def genMessage (fileMap : FileMap) (message : Lean.Message) : AnalysisM Message := do
    let headPos := Position.toStringPos fileMap message.pos
    let tailPos := Position.toStringPos fileMap (message.endPos.getD message.pos)
    let mut string ← message.data.toString
    if message.caption != "" then
      string := message.caption ++ ":¬" ++ string
    if message.severity == MessageSeverity.warning then
      string := "Warning: " ++ string
    else if message.severity == MessageSeverity.error then
      string := "Error: " ++ string
    return { headPos := headPos, tailPos := tailPos, msg := string }

  def insertMessages (self : AnalysisResult) (messages : List Lean.Message) (fileMap : FileMap) : AnalysisM AnalysisResult := do
    let messages ← messages.mapM (genMessage fileMap)
    let sortedMessages := List.sort (λ x y => x.headPos < y.headPos) messages
    let newSentences := sortedMessages.map (λ x => Sentence.message x)
    let mergedSentences := List.mergeSortedLists (λ x y => (Positional.headPos x) < (Positional.headPos y)) newSentences self.sentences
    return { self with sentences := mergedSentences }
end AnalysisResult

structure TraversalAux where
  allowsNewField : Bool := true
  allowsNewTerm : Bool := true
  allowsNewSemantic : Bool := true
  result : AnalysisResult := AnalysisResult.empty

namespace TraversalAux
  def merge (x y : TraversalAux) : TraversalAux := {
    allowsNewField := x.allowsNewField ∧ y.allowsNewField
    allowsNewTerm := x.allowsNewTerm ∧ y.allowsNewTerm
    result := AnalysisResult.merge x.result y.result
  }

end TraversalAux

--!
partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : TraversalAux := {}) (tree : InfoTree) (infoTreeCtx : InfoTreeContext) : AnalysisM TraversalAux := do
  let config ← read
  match tree with
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree (_updateIsCalcTatic config infoTreeCtx tree) 
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM (fun x => _resolveTacticList ctx? aux x (_updateIsCalcTatic config infoTreeCtx x)) 
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl TraversalAux.merge {}
      match (← TraversalFragment.create ctx info) with
      | (some fragment, some semantic) => pure sortedChildrenLeafs        
      | (some fragment, none) => pure sortedChildrenLeafs       
      | (none, some semantic) => pure sortedChildrenLeafs
      | (_, _) => pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : TraversalAux)
| error (e : IO.Error)

--!
def _resolveTask (tree : InfoTree) (infoTreeCtx : InfoTreeContext) : AnalysisM (Task TraversalEvent) := do
  let taskBody : AnalysisM TraversalEvent := do
    let res ← _resolveTacticList none {} tree infoTreeCtx
    return TraversalEvent.result res
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | Except.ok ev => ev
    | Except.error e => TraversalEvent.error e

def _resolve (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  let auxResults ← (trees.map (λ t => 
    _resolveTacticList none {} t (_buildInfoTreeContext config t))).mapM (λ x => x)
  let results := auxResults.map (λ x => x.result)
  return results.foldl AnalysisResult.merge AnalysisResult.empty

def resolveTasks (tasks : Array (Task TraversalEvent)) : AnalysisM (Option (List TraversalAux)) := do
  let mut results : List TraversalAux := []
  for task in tasks do
    let result ← BaseIO.toIO (IO.wait task)
    match result with
    | TraversalEvent.result r => results := r::results
    | _ => return none
  return results

--!
def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  let tasks ← trees.toArray.mapM (λ t => _resolveTask t (_buildInfoTreeContext config t))
  match (← resolveTasks tasks) with
  | some auxResults => do
    let results := auxResults.map (λ x => x.result)
    return results.foldl AnalysisResult.merge AnalysisResult.empty
  | _ => return { tokens := [], sentences := []}
