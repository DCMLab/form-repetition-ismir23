import Pkg; Pkg.activate(".")
import JSON
import Glob
using DataFrames

module Parse
include("./parser.jl")
end

module Opt
include("./optimize_grammar.jl")
end

function run_timing()
    Parse.write_subsequences()
    Opt.time_subs()
end

function run_long_experiment(n=nothing)

    function process_melody((i, melody))
        println("processing melody $(i) (length ($(length(melody))))...")

        # create ruleset
        Parse.seq2json(melody, "melodies/rulesets/ruleset_essen_$(i).json")

        # minimize ruleset
        ruleset = Opt.load_ruleset("../data/melodies/rulesets/ruleset_essen_$(i).json")
        min_rules, t = Opt.minimize_ruleset(ruleset)
        out = Opt.minimal_ruleset_to_json(ruleset, min_rules, t)
        open("../data/melodies/grammars/grammar_essen_$(i).json", "w") do f
            JSON.print(f, out)
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

function list_pieces(directory)
    # TODO: change to fixed 1-300
    fns = Glob.glob("grammars/grammar_essen_*.json", directory)
    [basename(fn)[9:end-5] for fn in fns]
end

function run_monte_carlo(n=100; directory="../data/melodies")
    for name in list_pieces(directory)
        println("running $(name)")
        ruleset = Opt.load_ruleset(joinpath(directory, "rulesets", "ruleset_$(name).json"))
        min_rules, cost = Opt.minimize_ruleset_mc(ruleset, n=n)
        out = Opt.minimal_ruleset_to_json(ruleset, min_rules)
        open(joinpath(directory, "mc_grammars", "mc_grammar_$(name).json"), "w") do f
            JSON.print(f, out)
        end
    end
end

function compare_grammars(directory="../data/melodies")
    function collect_point(name)
        grammar_opt = Opt.load_minimal_ruleset(joinpath(directory, "grammars", "grammar_$(name).json"))
        grammar_mc = Opt.load_minimal_ruleset(joinpath(directory, "mc_grammars", "mc_grammar_$(name).json"))

        (opt=sum(Opt.rule_dl_cost_humphreyslike.(grammar_opt.rules)),
         mc=sum(Opt.rule_dl_cost_humphreyslike.(grammar_mc.rules)),
         name=name)
    end
    
    DataFrame([collect_point(name) for name in list_pieces(directory)])
end
