import Lean

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

/-! ## Fragments -/

/-- A `Fragment` is a simple structure that describes an interval within the source text. -/
structure Fragment where
  headPos : String.Pos
  tailPos : String.Pos
  deriving Inhabited, ToJson

/-! ## Tactics -/

/-- A `TacticFragment` is a `Fragment` describing a tactic in a Lean file together with the goal states before and after. -/
structure TacticFragment extends Fragment where
  goalsBefore : List String
  goalsAfter : List String
  deriving Inhabited, ToJson

/-- A `TacticFragmentWithContent` describes not just the position and goals but also the actual content of the tactic. -/
structure TacticFragmentWithContent extends TacticFragment where
  content : String
  deriving Inhabited, ToJson

/-- Extracting the suggestion text from a "Try this: ..." message in the infoview. -/
def extractSuggestion (msg : Message) : OptionT IO String := do
  let raw ← msg.data.toString
  guard <| raw.startsWith "Try this: "
  return raw.drop "Try this: ".length

open FileMap in
/-- Generates ` TacticFragmentWithContent` from a bare `TacticFragment` using the file contents and associated messages. 
  If a message matches the position of the tactic fragment exactly, the actual content of the fragment is discarded and replaced with that of the message. 
  The main use case is in replacing `simp` calls with `simp only ...` trace messages that contain the full data of the premises used. -/
def TacticFragment.withContent (input : String) (messages : List Message) (fragment : TacticFragment) : IO TacticFragmentWithContent := do
  let fileMap := ofString input
  let content := input.extract fragment.headPos fragment.tailPos
  let suggestions ← messages.toArray.filterMapM <| fun msg ↦ OptionT.run do
     let msgHeadPos := (fileMap.lspPosToUtf8Pos ∘ fileMap.leanPosToLspPos) msg.pos
     let msgTailPos := (fileMap.lspPosToUtf8Pos ∘ fileMap.leanPosToLspPos) (← .ok msg.endPos)
     guard <| fragment.headPos ≤ msgHeadPos
     guard <| msgTailPos ≤ fragment.tailPos  
     let raw ← extractSuggestion msg
     return (msgHeadPos - fragment.headPos, msgTailPos - fragment.headPos, raw)
  return ⟨fragment, content.replaceSegments suggestions⟩