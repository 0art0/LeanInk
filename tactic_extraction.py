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
    return [os.path.join(root, file) 
        for root, dirs, files in os.walk(directory) 
        for file in files 
            if fnmatch.fnmatch(file, "*.lean")]

repository_path = "./lake-packages/mathlib/Mathlib/" # the location of the `Mathlib` source code
folder_path = "CategoryTheory/Functor" # can be modified to a specific sub-folder of `Mathlib`, like `Combinatorics/Regularity` or `Data/Int`
num_workers = 3 # the maximum number of workers in the concurrent code

def process_file(file):
    command = ['./build/bin/leanInk', file]
    try:
        subprocess.run(command, check=True)
        print(f"Command executed successfully on {file}")
    except subprocess.CalledProcessError as e:
        print(f"Error executing command on {file}: {e}")

# Concurrent execution of the `leanInk` script as *processes* (not *threads*) using the `concurrent.futures` framework
with concurrent.futures.ProcessPoolExecutor(max_workers=num_workers) as executor:
   executor.map(process_file, find_lean_files(repository_path + folder_path))