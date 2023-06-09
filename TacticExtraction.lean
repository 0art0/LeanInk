import LeanInk.Analysis
import LeanInk.Logger

/-! Code for tactic step data extraction from `Mathlib`. -/

def main : IO UInt32 := LeanInk.Analysis.mathlibExtract