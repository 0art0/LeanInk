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
  If a message matches the position of the tactic fragment exactly, the actual content of the fragment is discarded and replaced with that of the message. -/
def TacticFragment.withContent (contents : String) (messages : List Message) (fragment : TacticFragment) : IO TacticFragmentWithContent := do
  let fileMap := ofString contents
  let msgContent? : Option String ← OptionT.run do
    let suggestion := messages.find? <| fun msg ↦ 
      msg.pos == fileMap.toPosition fragment.headPos && 
      msg.endPos == some (fileMap.toPosition fragment.tailPos)
    (.ok suggestion : OptionT IO Message) >>= extractSuggestion
  return ⟨fragment, msgContent?.getD (contents.extract fragment.headPos fragment.tailPos)⟩
