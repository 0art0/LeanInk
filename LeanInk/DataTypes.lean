import Lean
import LeanInk.TreeConstruction

/-!
# Datatypes

Datatypes for storing tactic step data.
-/

/-! ## Syntax -/

/-- Whether a piece of `Syntax` is fully expanded, 
  i.e., corresponds to an actual segment of a file (see `Lean.SourceInfo` in `Init/Prelude`). -/
def Lean.Syntax.isExpanded (stx : Syntax) : Bool :=
  match stx.getHeadInfo, stx.getTailInfo with
  | .original .., .original .. => false
  | _, _ => true

/-- Traverse a syntax tree, gathering all sub-trees satisfying property `p`. -/
partial def Lean.Syntax.findAll (stx : Syntax) (p : Syntax → Bool) : Array Syntax :=
  let r := stx.getArgs.concatMap (·.findAll p)
  if p stx then
    r.push stx
  else r

/-- All identifiers occurring in a given `Syntax` object. -/
def Lean.Syntax.getIdents (stx : Syntax) (env : Environment) : TSyntaxArray `ident :=
  stx.findAll (·.isOfKind `ident) |>.filter (env.contains ·.getId) |>.map .mk

open Lean.Parser in
/-- All terms occurring in a given `Syntax` object. -/
def Lean.Syntax.getTerms (stx : Syntax) (env : Environment) : TSyntaxArray `term :=
  match Parser.getCategory (parserExtension.getState env).categories `term with
    | some termCat =>
      let termKinds := termCat.kinds
      stx.findAll (termKinds.contains ·.getKind) |>.map .mk
    | none => #[]

open Lean in
instance : ToJson Syntax where
  toJson stx := 
    match stx.reprint with
      | some str => toJson str.trim
      | none => .null

open Lean in
instance : ToJson (TSyntax k) where
  toJson tstx := toJson tstx.raw

/-! ## String manipulation -/

/-- Replace the portion of the string between `firstPos` and `lastPos` with the provided replacement. -/
def String.replaceSegment (s : String) (firstPos lastPos : String.Pos) (r : String) : String :=
  s.extract ⟨0⟩ firstPos ++ r ++ s.extract lastPos s.endPos

/-- Replace several segments in a string at once. To avoid relabelling positions, replacements at the end of the string are performed first. -/
def String.replaceSegments (s : String) (replacements : Array <| String.Pos × String.Pos × String) : String :=
  -- assumes that the replacements are disjoint
  let replacements' := replacements.insertionSort <| fun ⟨b, _, _⟩ ⟨_, e', _⟩ ↦ e' ≤ b
  replacements'.foldl (fun s ⟨b, e, r⟩ ↦ s.replaceSegment b e r) s

namespace LeanInk
open Lean Elab Meta

deriving instance ToJson for String.Pos
instance (priority := high) : ToJson String.Pos := ⟨(toJson ·.byteIdx)⟩
instance (priority := high) : FromJson String.Pos where
  fromJson? json := String.Pos.mk <$> fromJson? (α := Nat) json 

/-! ## Fragments -/

/-- A `Fragment` is a simple structure that describes an interval within the source text. -/
structure Fragment where
  /-- The start position of the fragment. -/
  headPos : String.Pos
  /-- The end position of the fragment. -/
  tailPos : String.Pos
  deriving Inhabited, ToJson, FromJson

instance : Ranged Fragment where
  tgt := Nat
  range fragment := ⟨fragment.headPos.byteIdx, fragment.tailPos.byteIdx⟩

/-! ## Tactics -/

/-- A `TacticFragment` is a `Fragment` describing a tactic in a Lean file together with the goal states before and after. -/
structure TacticFragment extends Fragment where
  /-- The goals before the start position. -/
  goalsBefore : List String
  /-- The goals after the end position. -/
  goalsAfter : List String
  deriving Inhabited, ToJson, FromJson

instance : Ranged TacticFragment where
  range fragment := Ranged.range fragment.toFragment

/-- A `TacticFragmentWithContent` describes not just the position and goals but also the actual content of the tactic. -/
structure TacticFragmentWithContent extends TacticFragment where
  /-- The name of the main tactic invoked in the fragment.. -/
  mainTactic : String
  /-- The tactic script used in the fragment. -/
  content : String
  deriving Inhabited, ToJson, FromJson

instance : Ranged TacticFragmentWithContent where
  range fragment := Ranged.range fragment.toFragment

/-- A `TacticFragmentWithIngredients` captures the *ingredients* that go into the tactic invocation,
    particularly the identifiers and terms used. -/
structure TacticFragmentWithIngredients extends TacticFragmentWithContent where
  /-- The identifiers used in the tactic invocation. -/
  identifiers : TSyntaxArray `ident
  /-- The terms used in the tactic invocation. -/
  terms : TSyntaxArray `term
deriving Inhabited, ToJson

instance : Ranged TacticFragmentWithIngredients where
  range fragment := Ranged.range fragment.toFragment

structure TacticFragmentWithEventualIngredients extends TacticFragmentWithIngredients where
  eventualIdentifiers : TSyntaxArray `ident
  eventualTerms : TSyntaxArray `term
deriving Inhabited, ToJson

instance : Ranged TacticFragmentWithEventualIngredients where
  range fragment := Ranged.range fragment.toFragment

/-- Extracting the suggestion text from a "Try this: ..." message in the infoview. -/
def extractSuggestion (msg : Message) : OptionT IO String := do
  let raw ← msg.data.toString
  guard <| raw.startsWith "Try this: "
  return raw.drop "Try this: ".length

/-- The name of the main tactic invoked in a line of a tactic block, together with its main ingredients (identifiers and terms). -/
def extractMainTacticAndIngredients (s : String) (env : Environment) : String × TSyntaxArray `ident × TSyntaxArray `term :=
  Option.getD do
    let stx ← Except.toOption <| Parser.runParserCategory env `tactic s
    let head ← stx.getHead?
    let tac ← head.reprint
    return ⟨tac.trim, stx.getIdents env, stx.getTerms env⟩ 
  ⟨"", #[], #[]⟩

open FileMap in
/-- Generates ` TacticFragmentWIthIngredients` from a bare `TacticFragment` using the file contents and associated messages. 
  If a message matches the position of the tactic fragment exactly, the actual content of the fragment is discarded and replaced with that of the message. 
  The main use case is in replacing `simp` calls with `simp only ...` trace messages that contain the full data of the premises used. -/
def TacticFragment.withIngredients (input : String) (messages : List Message) (env : Environment) 
    (fragment : TacticFragment) : IO TacticFragmentWithIngredients := do
  let fileMap := ofString input
  let content := input.extract fragment.headPos fragment.tailPos
  let suggestions ← messages.toArray.filterMapM <| fun msg ↦ OptionT.run do
     let msgHeadPos := (fileMap.lspPosToUtf8Pos ∘ fileMap.leanPosToLspPos) msg.pos
     let msgTailPos := (fileMap.lspPosToUtf8Pos ∘ fileMap.leanPosToLspPos) (← .ok msg.endPos)
     guard <| fragment.headPos ≤ msgHeadPos
     guard <| msgTailPos ≤ fragment.tailPos  
     let raw ← extractSuggestion msg
     return (msgHeadPos - fragment.headPos, msgTailPos - fragment.headPos, raw)
  let newContent := content.replaceSegments suggestions
  let ⟨tac, idents, terms⟩ := extractMainTacticAndIngredients newContent env
  return { fragment with
           mainTactic := tac, 
           content := newContent,
           identifiers := idents,
           terms := terms }

def withEventualIngredients (fragments : Array TacticFragmentWithIngredients) : Array TacticFragmentWithEventualIngredients :=
  fragments.toTrees.concatMap <| Tree.fold 
    fun fragment children ↦
      let childrenFlat := children.concatMap id
      childrenFlat.push { 
        fragment with
        eventualIdentifiers := childrenFlat.concatMap (·.eventualIdentifiers) ++ fragment.identifiers,
        eventualTerms := childrenFlat.concatMap (·.eventualTerms) ++ fragment.terms
      }