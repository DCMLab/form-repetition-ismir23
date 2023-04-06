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

function run_long_experiment()
    melodies = SubstringParser.load_melodies()
    melodies_sorted = sort(melodies, by=x->length(x))

    # TODO
end
