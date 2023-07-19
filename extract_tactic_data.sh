#!/usr/bin/env bash

echo "Starting Mathlib tactic extraction ..." && echo

# Bump toolchain to the latest version
echo Fetching latest Lean toolchain ...
curl -O https://raw.githubusercontent.com/leanprover-community/mathlib4/master/lean-toolchain
echo Updating Lean ...
lake update

# Fetch the `Mathlib` cache
echo && echo Fetching cache ...
lake exe cache get

# Building `mathlib`, just in case getting the cache fails
echo && echo Attempting to build `mathlib` ...
lake build mathlib

# Build the repository
echo && echo Building repository ...
lake build

# Run the tactic extraction script
echo && echo Running tactic extraction script ...
python3 tactic_extraction.py $1 $2

# Run the tactic census script
echo && echo Running tactic census script ...
python3 tactic_census.py

# Zipping the results
echo && echo Zipping the results ...
zip -r tactic_extraction_data.zip TacticExtractionData

echo && echo "
    Tactic extraction complete.

    The tactic extraction results are in \`tactic_extraction_data.zip\`. 
    The tactic census data is stored in \`tactic_census.json\`. 
    The log file for the tactic extraction is \`tactic_extraction.log\`.
    "