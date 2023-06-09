import os
import fnmatch
import subprocess
import concurrent.futures

# Written with help from ChatGPT

def find_lean_files(directory):
    lean_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if fnmatch.fnmatch(file, '*.lean'):
                lean_files.append(os.path.join(root, file))
    return lean_files

repository_path = './lake-packages/mathlib/Mathlib/ModelTheory/'

def process_file(file):
    command = ['./build/bin/leanInk', file]
    try:
        subprocess.run(command, check=True)
        print(f"Command executed successfully on {file}")
    except subprocess.CalledProcessError as e:
        print(f"Error executing command on {file}: {e}")

# Use concurrent.futures for concurrent execution
with concurrent.futures.ThreadPoolExecutor() as executor:
    executor.map(process_file, find_lean_files(repository_path))
