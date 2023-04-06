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
    AAA(a)
    ABA(a, b)
    AAB(a, b)
    ABB(a, b)
    AABA(a, b)
    ABBA(a, b)
end

SumTypes.show_sumtype(io::IO, x::RepRule) = @cases x begin
    Terminate(t) => print(io, "Term($t)")
    AA(a) => print(io, "AA($(a))")
    AB(a, b) => print(io, "AB($(a), $(b))")
    AAA(a) => print(io, "AAA($(a))")
    ABA(a, b) => print(io, "ABA($(a), $(b))")
    AAB(a, b) => print(io, "AAB($(a), $(b))")
    ABB(a, b) => print(io, "ABB($(a), $(b))")
    AABA(a, b) => print(io, "AABA($(a), $(b))")
    ABBA(a, b) => print(io, "ABBA($(a), $(b))")
end

map_rule(f, rule::RepRule) = @cases rule begin
    Terminate(t) => Terminate(t) # don't apply f here
    AA(a) => AA(f(a))
    AB(a, b) => AB(f(a), f(b))
    AAA(a) => AAA(f(a))
    ABA(a, b) => ABA(f(a), f(b))
    AAB(a, b) => AAB(f(a), f(b))
    ABB(a, b) => ABB(f(a), f(b))
    AABA(a, b) => AABA(f(a), f(b))
    ABBA(a, b) => ABBA(f(a), f(b))
end

JSON.lower(rule::RepRule) = @cases rule begin
    Terminate(t) => (ruletype="Term", params=[t])
    AA(a) => (ruletype="AA", params=[a])
    AB(a, b) => (ruletype="AB", params=[a, b])
    AAA(a) => (ruletype="AAA", params=[a])
    ABA(a, b) => (ruletype="ABA", params=[a, b])
    AAB(a, b) => (ruletype="AAB", params=[a, b])
    ABB(a, b) => (ruletype="ABB", params=[a, b])
    AABA(a, b) => (ruletype="AABA", params=[a, b])
    ABBA(a, b) => (ruletype="ABBA", params=[a, b])
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
    # complete ternatiry rules
    @cases leftcat begin
        NT =>
            if rightcat == NT
                # AA
                if leftstr == rightstr
                    push!(cell, NT, AA(leftstr))
                end
                
                # AB
                if leftstr != rightstr
                    push!(cell, NT, AB(leftstr, rightstr))
                end
                
                # incomplete AA
                if leftstr == rightstr
                    push!(cell, NTAA(leftstr))
                end
                
                # incomplete AB
                if leftstr != rightstr
                    push!(cell, NTAB(leftstr, rightstr))#ABA(leftstr, rightstr))
                end
            end
        NTAB(a, b) =>
            @cases rightcat begin
                NT => begin
                    # ABA
                    if a == rightstr
                        push!(cell, NT, ABA(a, b))
                    end

                    # ABB
                    if b == rightstr
                        push!(cell, NT, ABB(a, b))
                    end
                end
                NTAB(a2, b2) =>
                    # ABBA
                    if a == b2 && b == a2
                        push!(cell, NT, ABBA(a, b))
                    end
                NTAA => nothing # relevant cases covered below
            end
        NTAA(a) =>
            @cases rightcat begin
                NT =>
                    # AAA or AAB
                    if a == rightstr # AAA
                        push!(cell, NT, AAA(a))
                    else # AAB
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
example2 = ["g2", "e2", "g2", "e2", "f4", "e4", "f4", "g4", "e1",
  "d4", "d4", "f4", "f4", "e4", "f4", "g2",
  "d4", "d4", "f4", "f4", "e4", "f4", "g2",
  "g2", "e2", "g2", "e2", "f4", "e4", "f4", "g4", "e1"]

function load_melodies()
    melodies_df = DataFrame(CSV.File("../data/essen.tsv"))

    function df_to_melody(df)
        row_to_note(pitch, dur) = "$(pitch)-$(dur)"
        row_to_note.(df.pitch, df.duration)
    end

    [df_to_melody(sub) for sub in groupby(melodies_df, :piece)]
end


function seq2json(seq, fn)
    println("running $(fn)")
    chart = parse(RepGrammar(), seq)
    rules = chartrules(chart)
    srules = symbolize_rules(rules)
    drules = determ_rules(rules)
    
    out = (seq=seq, symb_rules=srules, determ_rules=drules, start=hash(seq))
    open("../data/$(fn)", "w") do f
        JSON.print(f, out)
    end
end

function write_examples()
    melodies = load_melodies()

    seq2json("xxy", "xxy.json")
    seq2json("xxyx", "xxyx.json")
    seq2json(example, "example1.json")
    seq2json(example2, "example2.json")
    seq2json(melodies[1], "melody1.json")
    seq2json(melodies[2], "melody2.json")
end

function write_subsequences()
    seqs = [example2[1:i] for i in 1:length(example2)]

    for i in 1:length(example2)
        sub = example2[1:i]

        seq2json(sub, "subs/n$(i).json")
    end
end

# # alternative parser

# function parse2(input::A) where A
#     substrings = subsequences(input)

#     chart = [Dict{A,Vector{RepRule}}() for i in 1:length(input)]

#     # terminal rules
#     for str in keys(substrings[1])
#         chart[1][str] = [Terminate(str[1])]
#     end

#     # non-terminal rules
#     for len in 2 : length(input)
#         for str in keys(substrings[len])
#             cell = RepRule[]

#             # split 1
#             for split1 in 1:len-1
#                 sub1 = @view str[1:split1]
#                 sub2 = @view str[split1+1:end]

#                 # binary rules
#                 if sub1 == sub2 # AA
#                     push!(cell, AA(sub1))
#                 else # AB
#                     push!(cell, AB(sub1, sub2))
#                 end

#                 # split 2
#                 for split2 in split1+1:len-1
#                     sub2 = @view str[split1+1:split2]
#                     sub3 = @view str[split2+1:end]

#                     # ternary rules
#                     # AAB
#                     if sub1 == sub2 && sub2 != sub3
#                         push!(cell, AAB(sub1, sub3))
#                     end
#                     # ABA
#                     if sub1 == sub3 && sub1 != sub2
#                         push!(cell, ABA(sub1, sub2))
#                     end

#                     # split 3
#                     for split3 in split2+1:len-1
#                         sub3 = @view str[split2+1:split3]
#                         sub4 = @view str[split3+1:end]

#                         # 4-ary rules
#                         if sub1 == sub2 && sub1 == sub4 && sub1 != sub3
#                             push!(cell, AABA(sub1, sub3))
#                         end
#                     end
#                 end
#             end
#             chart[len][str] = cell
#         end
#     end

#     return chart
# end

# chartrules2(chart) =
#     Dict(sub => cell
#          for layer in chart
#              for (sub, cell) in layer)
