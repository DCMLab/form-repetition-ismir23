import Pkg; Pkg.activate(".")
import JSON

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

    pmap(process_melody, enumerate(melodies_sorted[1:10]));
end
