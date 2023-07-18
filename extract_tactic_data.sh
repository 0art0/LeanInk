#!/usr/bin/env bash

# Bump toolchain to the latest version
echo Fetching latest Lean toolchain ...
curl -O https://raw.githubusercontent.com/leanprover-community/mathlib4/master/lean-toolchain
echo Updating Lean ...
lake update

# Fetch the `Mathlib` cache
echo && echo Fetching cache ...
lake exe cache get

# Build the repository
echo && echo Building repository ...
lake build

# Run the tactic extraction script
echo && echo Running tactic extraction script ...
python tactic_extraction.py

# Run the tactic census script
echo && echo Running tactic census script ...
python tactic_census.py

echo && echo Finished.