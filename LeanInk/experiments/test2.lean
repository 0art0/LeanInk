set_option tactic.simp.trace true

example : ∀ a, a + 0 = 0 + a := by
  intro a
  have : 0 = 0 := by rfl
  simp