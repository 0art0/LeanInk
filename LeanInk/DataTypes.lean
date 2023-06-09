import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis
open Lean Elab Meta

instance : ToJson String.Pos := ⟨fun ⟨n⟩ => toJson n⟩

/- Fragment -/
/--
  A `Fragment` is a simple structure that describes an interval within the source text.
  This is similar to the `Positional` type class. However the structure is used as a parent for other structures
  in the Analysis. As a result every `Fragment` automatically conforms to `Positional`
-/
structure Fragment where
  headPos : String.Pos
  tailPos : String.Pos
  deriving Inhabited, ToJson

/- Tactics -/

structure TacticFragment extends Fragment where
  goalsBefore : List String
  goalsAfter : List String
  deriving Inhabited, ToJson

structure TacticFragmentWithContent extends TacticFragment where
  content : String
  deriving Inhabited, ToJson

def extractSuggestion (msg : Message) : OptionT IO String := do
  let raw ← msg.data.toString
  guard <| raw.startsWith "Try this: "
  return raw.drop "Try this: ".length

open FileMap in
def TacticFragment.withContent (contents : String) (messages : List Message) (fragment : TacticFragment) : IO TacticFragmentWithContent := do
  let fileMap := ofString contents
  let msgContent? : Option String ← OptionT.run do
    let suggestion := messages.find? <| fun msg ↦ 
      msg.pos = fileMap.toPosition fragment.headPos && 
      msg.endPos = some (fileMap.toPosition fragment.tailPos)
    (.ok suggestion : OptionT IO Message) >>= extractSuggestion
  return ⟨fragment, msgContent?.getD (contents.extract fragment.headPos fragment.tailPos)⟩
  
/- InfoTree -/
def Info.isExpanded (self : Info) : Bool :=
  let stx := Info.stx self
  match stx.getHeadInfo, stx.getTailInfo with
  | SourceInfo.original .., SourceInfo.original .. => false
  | _, _ => true