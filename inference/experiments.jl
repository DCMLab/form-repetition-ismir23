import Pkg; Pkg.activate(".")
import JSON
import Glob
using DataFrames
using Distributed: pmap

module Parse
include("./parser.jl")
end

include("./optimize_grammar.jl")

include("./plotting.jl")

"""
    run_long_experiment(n=nothing)

Runs the optimization on the `n` shortest Essen melodies.
Stores the resulting rulesets in `../data/melodies/rulesets/`
and the grammars in `../data/melodies/grammars/`.
If `n` is omitted, the grammars for all melodies are inferred (not recommended).
This is slow for long sequences!
"""
function run_long_experiment(n=nothing)

    function process_melody((i, melody))
        println("processing melody $(i) (length ($(length(melody))))...")

        # create ruleset
        ruleset_path = "melodies/rulesets/ruleset_essen_$(i).json"
        if !isfile(joinpath("../data", ruleset_path))
            Parse.seq2json(melody, ruleset_path)
        end

        # minimize ruleset
        grammar_path = "../data/melodies/grammars/grammar_essen_$(i).json"
        if !isfile(grammar_path)
            ruleset = load_ruleset(joinpath("../data", ruleset_path))
            min_rules, t = minimize_ruleset(ruleset)
            out = minimal_ruleset_to_json(ruleset, min_rules, t)
            open(grammar_path, "w") do f
                JSON.print(f, out)
            end
        end

        println("done melody $(i)")
    end

    println("loading melodies...")
    melodies = Parse.load_melodies()
    melodies_sorted = sort(melodies, by=x->length(x))
    if n == nothing
        n = length(melodies_sorted)
    end

    pmap(process_melody, enumerate(melodies_sorted[1:n]));
end

"""
    list_existing_grammars(directory)

Returns the numbers of the Essen melodies in `<directory>/grammars/`
for which a grammar has already been produced.
"""
function list_existing_grammars(directory)
    fns = Glob.glob("grammars/grammar_essen_*.json", directory)
    [basename(fn)[9:end-5] for fn in fns]
end

"""
    run_monte_carlo(n=10000; directory="../data/melodies")

Runs a Monte-Carlo minimization for each Essen melody
that already has an optimal grammar in `<directory>/grammars/`.
The resulting estimated grammars are stored in `<directory>/mc_grammars/`.
Uses `n` random grammars to find a minimum.
"""
function run_monte_carlo(n=10000; directory="../data/melodies")
    for name in list_existing_grammars(directory)
        println("running $(name)")
        ruleset = load_ruleset(joinpath(directory, "rulesets", "ruleset_$(name).json"))
        min_rules, cost = minimize_ruleset_mc(ruleset, n=n)
        out = minimal_ruleset_to_json(ruleset, min_rules)
        open(joinpath(directory, "mc_grammars", "mc_grammar_$(name).json"), "w") do f
            JSON.print(f, out)
        end
    end
end

"""
    compare_grammars(directory="../data/melodies/")

A helper function that compares the sizes of optimal and Monte-Carlo grammars
for a the melodies in `directory`.
Returns a `DataFrame` with the columns
- `name` (number of the melody)
- `opt` (size of the optimal grammar)
- `mc` (size of the Monte-Carlo grammars)
"""
function compare_grammars(directory="../data/melodies")
    function collect_point(name)
        grammar_opt = load_minimal_ruleset(joinpath(directory, "grammars", "grammar_$(name).json"))
        grammar_mc = load_minimal_ruleset(joinpath(directory, "mc_grammars", "mc_grammar_$(name).json"))

        (opt=sum(rule_dl_cost_humphreyslike.(grammar_opt.rules)),
         mc=sum(rule_dl_cost_humphreyslike.(grammar_mc.rules)),
         name=name)
    end
    
    DataFrame([collect_point(name) for name in list_pieces(directory)])
end

"""
    run_examples()

A test function that computes minimal grammars for a few examples.
The results are stored in `../data/`.
The corresponding rulesets are expected to exist in `../data/` already,
they can be generated using `Parse.write_examples()`.
"""
function run_examples()
    for piece in ["xxy", "xxyx", "example1", "example2"]
        println("running ", piece)
        
        ruleset = load_ruleset("../data/$(piece).json")
        min_rules, t = minimize_ruleset(ruleset)
        out = minimal_ruleset_to_json(ruleset, min_rules, t)
        open("../data/grammar_$(piece).json", "w") do f
            JSON.print(f, out)
        end
    end
end
