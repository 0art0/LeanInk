![LeanInkLogo](https://user-images.githubusercontent.com/24965150/145307859-30350f23-4f7f-4aab-a1ab-34889ad44d9a.png)

[![CI](https://github.com/leanprover/LeanInk/actions/workflows/build.yml/badge.svg)](https://github.com/insightmind/LeanInk/actions/workflows/build.yml)
[![LƎⱯN - 4](https://img.shields.io/static/v1?label=LƎⱯN&message=4&color=black)](https://github.com/leanprover/lean4)

This branch of `LeanInk` is a bare-bones version meant for extracting tactic data from `mathlib4`. It was derived from the original `LeanInk` source by iteratively simplifying and customising the code for tactic data extraction.

To extract the tactic data from the latest version of `Mathlib`, run the `extract_tactic_data` script as `bash extract_tactic_data.sh . 4`. The first argument indicates the directory relative to the main `Mathlib` folder, and the second indicates the number of threads to use while running the tactic extraction in parallel.

LeanInk is a command line helper tool for [Alectryon](https://github.com/cpitclaudel/alectryon) which aims to ease the integration and support of [Lean 4](https://github.com/leanprover/lean4).
Alectryon uses the information provided by LeanInk to create a static code visualization for Lean 4 code.
For more information about Alectryon make sure to take a look at their repository.

> The official version of Alectryon does not yet support LeanInk, as LeanInk is still in active development. Please use our [Alectryon Fork](https://github.com/insightmind/alectryon/tree/lean4) to test LeanInk.

# Installation

Building from source is the recommended way of trying out this fork of `LeanInk`.

## Building from source

Before you can build LeanInk from source make sure to install the latest version of [Lean 4](https://github.com/leanprover/lean4) using `elan`.
This will also automatically install the [Lake](https://github.com/leanprover/lake) package manager.

```bash
git clone https://github.com/0art0/LeanInk --branch tactic-extraction
cd LeanInk
lake exe cache get
lake build
```

To install this built version it is recommended you simply add the `LeanInk/build/bin` folder to your PATH environment.

# Usage

Analyzing a simple lean program `Input.lean` is very straightforward. To do so you simply provide `LeanInk` the input file.

```bash
./build/bin/leanInk Input.lean
```

This creates a file `Input.lean.json` in the `TacticExtractionData` folder with the data of the tactic states. If the `TacticExtractionData` folder does not already exist, it can be created by running

```bash
mkdir TacticExtractionData
```

---

However, in practice the code is designed to generate tactic-step data in bulk from `mathlib4` files. The Python script `tactic_extraction.py` runs the `leanInk` script concurrently on the entire `mathlib4` directory (using the version specified in the `lakefile` or `manifest.json` file), handling the creation of the `TacticExtractionData` folder in the process. The variable `num_workers` in the `tactic_extraction.py` file can be modified to set the maximum number of processes that are allowed to run in parallel.

The entire tactic extraction process is bundled up into the `extract_tactic_data.sh` script in the top-level directory. Instructions for use are mentioned in the second paragraph of this `README` file.

# Contributing

LeanInk enforces the same [Contribution Guidelines](https://github.com/leanprover/lean4/blob/master/CONTRIBUTING.md) as Lean 4. Before contributing, make sure to read it.

We also highly encourage you to sign your commits.