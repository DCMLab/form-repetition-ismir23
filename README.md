# Form Schemata and Repetition Grammars

This repository contains code and data related to the paper "Repetition Structure Inference with Formal Prototypes".

## Running the Code

### Dependencies

The code for extracting the source data is written in Python
and can be found in `generate-data`.
The dependencies are listed there in `requirements.txt`.

The inference code is written in Julia and can be found in `inference`.
Dependencies are listed in a Julia project file,
so you can install them by running Julia in the directory,
enter package mode by hitting `]` and running
```
(Julia) pkg> activate .
(inference) pkg> instantiate
```

The inference code uses a solver, which by default is set to the commercial solver Gurobi.
To run the code, you can [get an academic license](https://www.gurobi.com/features/academic-wls-license/).
Alternatively, you can use a different solver supported by the
[JuMP framework](https://jump.dev/JuMP.jl/stable/installation/#Supported-solvers),
but that will require some changes to the code in `optimize_grammar.jl` (see comments)
and will likely be slower.

The plot notebook requires [`IJulia`](https://github.com/JuliaLang/IJulia.jl),
a Jupyter kernel for Julia.

### Reproducing the results from the paper

You can run all or only some of the following steps, as the intermediate results
are already stored in the repository

1. Extract the data from the source collection:
   ```shell
   $ cd generate-data
   $ python extract-essen.py ../data/essen.tsv
   ```
2. Run the experiments:
   ```
   $ cd inference
   $ julia -L experiments.jl
   ```
   - compute minimal grammars for Essen melodies:
     ```julia
     julia> run_long_experiment(3)
     ```
     The above line runs on the 3 shortest tunes,
     if you want to run the full experiment as in the paper, use `298` instead of `3`
     (this will take several days).
     The code will save the ruleset for every piece in `data/melodies/rulesets/`
     and the inferred grammra in `data/melodies/grammars/`.
   - run Monte-Carlo minimization:
     ```julia
     julia> run_monte_carlo()
     ```
     This will take the same melodies as processed in the next step
     (based on the saved results in `data/melodies/grammars`),
     estimate a minimal grammar by sampling random grammars and picking the smallest one,
     and write the resulting grammar to `data/melodies/mc_grammars`.
     By default, 10,000 grammars are sampled,
     but `run_monte_carlo()` takes a parameter to control this.
   - Generate the plots:
     Run the notebook `inference/plots.ipynb`.
     The resulting plots are stored in `plots/`.
     
### Using the Code for Custom Data

The code is not designed as a library and rather specialized
to the expirements in the paper.
If you would like to reuse some of the code,
take a look at `run_examples()` and `run_long_experiment()` in `inference/experiments.jl`
for some inspiration.

If you are interested in packaging the functionality into a library, please get in touch!

## Repo Overview

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
