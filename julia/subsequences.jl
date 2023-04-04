using CategoricalArrays
using DataFrames
using CSV
using SumTypes
import JSON
import Base: push!, keys

# grammar interface

abstract type Grammar{Rules,Items} end

function push_tcompletions! end

function push_ntcompletions! end

"""
map_rule(f, rule)

Applies a function (e.g. hash) to each argument of the rule.
"""
function map_rule end

# cell datatype

struct Cell{C,R}
    cell :: Dict{C,Union{Vector{R},Nothing}}
end

Cell{C,R}() where {C, R} = Cell{C,R}(Dict{C,Union{Vector{R},Nothing}}())

function Base.push!(c::Cell{C,R}, cat::C, rule::R) where {C, R}
    if cat ∈ keys(c.cell)
        push!(c.cell[cat], rule)
    else
        c.cell[cat] = [rule]
    end
end

function Base.push!(c::Cell{C,R}, cat::C) where {C, R}
    if cat ∉ keys(c.cell)
        c.cell[cat] = nothing
    end
end

function Base.show(io::IO, x::Cell)
    println(io, "Cell:")
    for (nt, rules) in x.cell
        println(io, "    $(nt) => $(rules)")
    end
end

Base.keys(c::Cell) = Base.keys(c.cell)

# parser

function subsequences(seq)
    """
    Finds the first occurrence of each subsequence of seq.
    Returns an array of dictionaries
    where `result[l][sub]` contains the range of the first occurrence of `sub` in `seq`,
    and `l` is `length(sub)`
    """
    subs = [Dict() for i in 1:length(seq)]
    
    saved = 0
    
    # every length
    for l in 1:length(seq)
        # every index per length
        for i in 1 : length(seq)-l+1
            sub = seq[i:i+l-1]
            if sub ∉ keys(subs[l])
                subs[l][sub] = i:i+l-1
            end
        end
    end
    subs
end

function parse(grammar::G, input::A) where {R, C, G <: Grammar{C,R}, A}
    substrings = subsequences(input)

    chart = [Dict{A,Cell{C,R}}() for i in 1:length(input)]

    # terminal rules
    for str in keys(substrings[1])
        cell = Cell{C,R}()
        push_tcompletions!(grammar, cell, str[1])
        chart[1][str] = cell
    end

    # non-terminal rules
    for len in 2 : length(input)
        for str in keys(substrings[len])
            cell = Cell{C,R}()
            for split in 1:len-1
                leftstr = str[1:split]
                rightstr = str[split+1:end]
                for leftcat in keys(chart[split][leftstr])
                    for rightcat in keys(chart[len-split][rightstr])
                        #println("completing $(str) --> $(leftcat)$(leftstr) $(rightcat)$(rightstr)")
                        push_ntcompletions!(grammar, cell, leftstr, leftcat, rightstr, rightcat)
                    end
                end
            end
            chart[len][str] = cell
        end
    end

    return chart
end

# working with the chart

chartrules(chart) =
    Dict(sub => get(cell.cell, NT, [])
         for layer in chart
             for (sub, cell) in layer)

function symbolize_rules(rules)
    # symbolize all rules
    srules = Dict(hash(lhs) => [map_rule(hash, rhs) for rhs in rhss]
                  for (lhs, rhss) in rules)

    # check for hash collisions
    if length(Set(keys(srules))) != length(keys(rules))
        error("hash collision!")
    end

    srules
end

@sum_type GrammarSymbol begin
    Term(x)
    NonTerm(x)
end

SumTypes.show_sumtype(io::IO, x::GrammarSymbol) = @cases x begin
    Term(t) => print(io, "T($t)")
    NonTerm(nt) => print(io, "NT($nt)")
end

JSON.lower(symb::GrammarSymbol) = @cases symb begin
    Term(t) => (symbtype="T", value=t)
    NonTerm(nt) => (symbtype="NT", value=nt)
end

function determ_rules(rules)
    # find terminals in a rule and symbolize non-terminals
    determ(arg) = if length(arg) == 1
        Term(arg[1])
    else
        NonTerm(hash(arg))
    end

    # determ and symbolize all rules, remove terminal rules
    drules = Dict(hash(lhs) => [map_rule(determ, rhs) for rhs in rhss]
                  for (lhs, rhss) in rules
                      if length(lhs) > 1)

    # check for hash collisions
    origkeys = [key for key in keys(rules) if length(key) > 1]
    if length(Set(keys(drules))) != length(origkeys)
        error("hash collision!")
    end

    drules
end

# repetition grammar

@sum_type RepRule begin
    Terminate(t)
    AA(a)
    AB(a, b)
    ABA(a, b)
    AAB(a, b)
    AABA(a, b)
end

SumTypes.show_sumtype(io::IO, x::RepRule) = @cases x begin
    Terminate(t) => print(io, "Term($t)")
    AA(a) => print(io, "AA($(a))")
    AB(a, b) => print(io, "AB($(a), $(b))")
    ABA(a, b) => print(io, "ABA($(a), $(b))")
    AAB(a, b) => print(io, "AAB($(a), $(b))")
    AABA(a, b) => print(io, "AABA($(a), $(b))")
end

map_rule(f, rule::RepRule) = @cases rule begin
    Terminate(t) => Terminate(t) # don't apply f here
    AA(a) => AA(f(a))
    AB(a, b) => AB(f(a), f(b))
    ABA(a, b) => ABA(f(a), f(b))
    AAB(a, b) => AAB(f(a), f(b))
    AABA(a, b) => AABA(f(a), f(b))
end

JSON.lower(rule::RepRule) = @cases rule begin
    Terminate(t) => (ruletype="Term", params=[t])
    AA(a) => (ruletype="AA", params=[a])
    AB(a, b) => (ruletype="AB", params=[a, b])
    ABA(a, b) => (ruletype="ABA", params=[a, b])
    AAB(a, b) => (ruletype="AAB", params=[a, b])
    AABA(a, b) => (ruletype="AABA", params=[a, b])
end

@sum_type NonTerminal begin
    NT
    NTAA(a)
    NTAB(a, b)
end

SumTypes.show_sumtype(io::IO, x::NonTerminal) = @cases x begin
    NT => print(io, "NT")
    NTAA(a) => print(io, "AA*($(a))")
    NTAB(a, b) => print(io, "AB*($(a),$(b))")
end

struct RepGrammar <: Grammar{NonTerminal,RepRule} end

function push_tcompletions!(::RepGrammar, cell, terminal)
    # println(terminal)
    push!(cell, NT, Terminate(terminal))
end

function push_ntcompletions!(::RepGrammar, cell, leftstr, leftcat, rightstr, rightcat)
    # AA
    if leftstr == rightstr && leftcat == rightcat == NT
        push!(cell, NT, AA(leftstr))
    end

    # AB
    if leftstr != rightstr && leftcat == rightcat == NT
        push!(cell, NT, AB(leftstr, rightstr))
    end

    # incomplete AA
    if leftstr == rightstr && leftcat == rightcat == NT
        push!(cell, NTAA(leftstr))
    end

    # incomplete AB
    if leftstr != rightstr && leftcat == rightcat == NT
        push!(cell, NTAB(leftstr, rightstr))#ABA(leftstr, rightstr))
    end

    # complete ternatiry rules
    @cases leftcat begin
        NT => nothing
        NTAB(a, b) =>
            # ABA
            if a == rightstr && rightcat == NT
                push!(cell, NT, ABA(a, b))
            end
        NTAA(a) =>
            @cases rightcat begin
                NT =>
                    # AAB
                    if a != rightstr
                        push!(cell, NT, AAB(a, rightstr))
                    end
                NTAB(b, a2) =>
                    # AABA = AA + BA
                    if a == a2
                        push!(cell, NT, AABA(a, b))
                    end
                NTAA => nothing
            end
    end
end

# scripts

example = categorical(["g2", "e2", "g2", "e2", "f4", "e4", "f4", "g4", "e1"])
example2 = categorical(["g2", "e2", "g2", "e2", "f4", "e4", "f4", "g4", "e1",
  "d4", "d4", "f4", "f4", "e4", "f4", "g2",
  "d4", "d4", "f4", "f4", "e4", "f4", "g2",
  "g2", "e2", "g2", "e2", "f4", "e4", "f4", "g4", "e1"])

function load_melodies()
    melodies_df = DataFrame(CSV.File("../essen.tsv"))

    function df_to_melody(df)
        row_to_note(pitch, dur) = "$(pitch)-$(dur)"
        row_to_note.(df.pitch, df.duration)
    end

    [df_to_melody(sub) for sub in groupby(melodies_df, :piece)]
end

function write_examples()
    melodies = load_melodies()

    function seq2json(seq, fn)
        println("running $(fn)")
        chart = parse(RepGrammar(), seq)
        rules = chartrules(chart)
        srules = symbolize_rules(rules)
        drules = determ_rules(rules)

        out = (seq=seq, symb_rules=srules, determ_rules=drules, start=hash(seq))
        open("../data/"+fn, "w") do f
            JSON.print(f, out)
        end
    end

    seq2json("xxy", "xxy.json")
    seq2json("xxyx", "xxyx.json")
    seq2json(example, "example1.json")
    seq2json(example2, "example2.json")
    seq2json(melodies[1], "melody1.json")
    seq2json(melodies[2], "melody2.json")
end
