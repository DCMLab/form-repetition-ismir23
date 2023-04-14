import Pkg; Pkg.activate(".")
import JSON
import Glob
using DataFrames

module Parse
include("./parser.jl")
end

include("./optimize_grammar.jl")

include("./plotting.jl")

function run_timing()
    Parse.write_subsequences()
    time_subs()
end

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

function list_pieces(directory)
    # TODO: change to fixed 1-300
    fns = Glob.glob("grammars/grammar_essen_*.json", directory)
    [basename(fn)[9:end-5] for fn in fns]
end

function run_monte_carlo(n=100; directory="../data/melodies")
    for name in list_pieces(directory)
        println("running $(name)")
        ruleset = load_ruleset(joinpath(directory, "rulesets", "ruleset_$(name).json"))
        min_rules, cost = minimize_ruleset_mc(ruleset, n=n)
        out = minimal_ruleset_to_json(ruleset, min_rules)
        open(joinpath(directory, "mc_grammars", "mc_grammar_$(name).json"), "w") do f
            JSON.print(f, out)
        end
    end
end

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
