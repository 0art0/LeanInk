import os
import fnmatch
import sys
import subprocess
import concurrent.futures

"""
This script attempts to run the tactic extraction script (`./build/bin/leanInk`) over all files of `Mathlib` in parallel.
Parts of this code were written with help from ChatGPT.
"""

def find_lean_files(directory):
    """Finds all `.lean` files in the specified directory (including sub-folders)."""
    if not os.path.exists(directory):
        print("The specified directory " + dir + " does not exist.")

    return [os.path.join(root, file) 
        for root, dirs, files in os.walk(directory) 
        for file in files 
            if fnmatch.fnmatch(file, "*.lean")]

# the location of the `Mathlib` source code
repository_path = "./lake-packages/mathlib/Mathlib/"
# the `Mathlib` folder path, which can be modified to a specific sub-folder like `Combinatorics/Regularity` or `Data/Int`
folder_path = "Analysis/ODE"
# the maximum number of workers in the concurrent code
num_workers = 3

def process_file(file):
    """Attempt to run the `leanInk` script on the specified file."""
    command = ['./build/bin/leanInk', file]
    try:
        subprocess.run(command, check=True)
        print(f"LeanInk executed successfully on {file}\n")
    except subprocess.CalledProcessError as e:
        print(f"ERROR executing LeanInk on {file}: {e}\n")

# Create the `TacticExtraction` folder if it does not already exist
if not os.path.exists("TacticExtractionData"):
    os.makedirs("TacticExtractionData")

# Concurrent execution of the `leanInk` script as *processes* (not *threads*) using the `concurrent.futures` framework
with concurrent.futures.ProcessPoolExecutor(max_workers=num_workers) as executor:
   executor.map(process_file, find_lean_files(repository_path + folder_path))