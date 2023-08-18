structure Range (α) [LE α] where
  start : α
  stop : α

def Range.contains [LE α] (r r' : Range α) :=
  r.start ≤ r'.start ∧ r'.stop ≤ r.stop

instance [LE α] [DecidableRel LE.le (α := α)] : DecidableRel <| Range.contains (α := α) := by
  dsimp [Range.contains]; infer_instance

def Range.contains' [LE α] [DecidableRel LE.le (α := α)] (r r' : Range α) : Bool :=
  r.contains r'


class Ranged (α β) [LE β] where
  range : α → Range β 

def Ranged.contains [LE β] [Ranged α β] (a a' : α) :=
  Range.contains (α := β) (Ranged.range a) (Ranged.range a')

def Ranged.contains' [LE β] [DecidableRel LE.le (α := β)] [Ranged α β] (a a' : α) :=
  Range.contains' (α := β) (Ranged.range a) (Ranged.range a')

inductive Tree (α : Type _) where
  | node (label : α) (children : Array <| Tree α)

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

variable {α β} [Inhabited α] [LE β] [DecidableRel LE.le (α := β)] [Ranged α β]

open Ranged

partial def insertInTree (elem : α) : Tree α → Tree α
  | .node label children => .node label (insertInTreeArray elem children)

partial def insertInTreeArray (elem : α) (τs : Array <| Tree α) : Array <| Tree α :=
  match τs.findIdx? (contains' (β := β) ·.label elem) with
    | some idx =>
      let τ := τs.get! idx
      τs.set! idx (insertInTree elem τ)
    | none => τs.push <| .node elem #[]

end

variable {α β} [Inhabited α] [LE β] [DecidableRel LE.le (α := β)] [Ranged α β]

def Array.toTrees : Array α → Array (Tree α) :=
  Array.foldl (init := #[]) fun trees elem ↦ 
    insertInTreeArray (β := β) elem trees