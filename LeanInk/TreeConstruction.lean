structure Range (α) [LE α] where
  start : α
  stop : α
deriving Repr

instance [Inhabited α] [LE α] : Inhabited (Range α) where
  default := ⟨default, default⟩

def Range.contains [LE α] (r r' : Range α) :=
  r.start ≤ r'.start ∧ r'.stop ≤ r.stop

instance [LE α] [DecidableRel LE.le (α := α)] : DecidableRel <| Range.contains (α := α) := by
  dsimp [Range.contains]; infer_instance

def Range.contains' [LE α] [DecidableRel LE.le (α := α)] (r r' : Range α) : Bool :=
  r.contains r'


class Ranged (α) where
  tgt : Type _
  tgtLE : LE tgt
  tgtLEdec : DecidableRel tgtLE.le
  range : α → Range tgt

instance [Ranged α] : LE (Ranged.tgt α) := Ranged.tgtLE (α := α)
instance [Ranged α] : DecidableRel (LE.le (α := Ranged.tgt α)) := Ranged.tgtLEdec (α := α)

instance [LE α] [DecidableRel (LE.le (α := α))] : Ranged (Range α) where
  tgt := α
  tgtLE := by assumption
  tgtLEdec := by assumption
  range := id

def Ranged.contains [Ranged α] (a a' : α) : Bool :=
  Range.contains' (Ranged.range a) (Ranged.range a')


inductive Tree (α : Type _) where
  | node (label : α) (children : Array <| Tree α)
deriving Repr

namespace Tree

instance [Inhabited α] : Inhabited (Tree α) where
  default := .node default #[]

def label : Tree α → α
  | .node label _ => label

def children : Tree α → Array (Tree α)
  | .node _ children => children

partial def map [Inhabited β] (φ : α → β) : Tree α → Tree β
  | .node label children => .node (φ label) (children.map <| map φ)

partial def fold [Inhabited β] (φ : α → Array β → β) : Tree α → β
  | .node label children => φ label (children.map <| fold φ)

end Tree


mutual

variable {α} [Inhabited α] [Ranged α]

open Ranged

partial def insertInTree (elem : α) : Tree α → Tree α
  | .node label children => .node label (insertInTreeArray elem children)

partial def insertInTreeArray (elem : α) (τs : Array <| Tree α) : Array <| Tree α :=
  match τs.findIdx? (contains ·.label elem) with
    | some idx =>
      let τ := τs.get! idx
      τs.set! idx (insertInTree elem τ)
    | none => 
      if τs.all (contains elem ·.label) then
        #[.node elem τs]
      else
        τs.push <| .node elem #[]

end

def Array.toTrees [Inhabited α] [Ranged α] : Array α → Array (Tree α) :=
  Array.foldl (init := #[]) fun trees elem ↦ 
    insertInTreeArray elem trees