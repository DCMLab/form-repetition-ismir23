# Form Schemata and Repetition Grammars

This repository contains code and data related to the paper "Repetition Structure Inference with Formal Prototypes".

The repo contains the following code and data:
- `generate-data/`: code for extracting the datasets (Python)
  - `extract-essen.py`: converting the Essen Folksong Collection to tsv
  - `extract-mtc.py`: converting the Meertens Tune Collection to tsv (not used in the paper)
  - `requirements.txt`: Python dependencies
- `inference/`: code for grammar inference (Julia)
  - `Manifest.toml`, `Project.toml`: Julia dependencies
  - `parser.jl`: parsing sequences into rulesets
  - `optimize_grammar.jl`: inferring minimal grammars from rulesets
  - `experiments.jl`: the experiments described in the paper
  - `plotting.jl`: helper code for plotting
  - `plots.ipynb`: notebook for generating the plots and analyses shown in the paper
- `data/`: data and results
  - `essen.tsv`: Essen Folksong Collection as note lists
  - `mtc.tsv`: Meertens Tune Collection melodies as note lists (not used)
  - `[grammar_]{xxy,xxyx,example1,example2}.json`: rulesets and grammars for example pieces.
  - `melodies/`: computed rulesets and grammars for short tunes from the Essen corpus
    - `rulesets/`: computed rulesets
    - `grammars/`: optimal grammars
    - `mc_grammars/`: Monte-Carlo minimized grammars
- `plots/`: plots for the paper
