using JuMP
using Gurobi
using HiGHS
import JSON
using SumTypes
using Plots
using DataFrames
import CSV

const GRB_ENV = Gurobi.Env()

function model_xxy_direct()
    model = Model(() -> Gurobi.Optimizer(GRB_ENV))

    # variables

    # rules
    @variable(model, rule1, Bin) # xxy --> AB(xx, y)
    @variable(model, rule2, Bin) # xxy --> AB(x, xy)
    @variable(model, rule3, Bin) # xx --> AA(x)
    @variable(model, rule4, Bin) # xy --> AB(x,y)

    # symbols
    @variable(model, xx, Bin)
    @variable(model, xy, Bin)

    # constaints
    
    # rules dependend on their RHS symbols
    @constraint(model, deps_rule1, xx >= rule1) # rule1 needs xx
    @constraint(model, deps_rule2, xy >= rule2) # rule2 needs xy

    # symbols depend on rules to expand them
    @constraint(model, lhs_xxy, rule1 + rule2 == 1) # =1 rule for xxy
    @constraint(model, lhs_xx, rule3 == xx) # 1 rule for xx iff xx
    @constraint(model, lhs_xy, rule4 == xy) # 1 rule for xy iff xy

    # objective function
    @objective(model, Min, rule1*4 + rule2*4 + rule3*3 + rule4*4)
    
    return model
end

function model_xxy()
    #model = Model(() -> Gurobi.Optimizer(GRB_ENV))
    model = Model(HiGHS.Optimizer)

    # variables

    # rules
    # r1: xxy --> AB(xx, y)
    # r2: xxy --> AB(x, xy)
    # r3: xx --> AA(x)
    # r4: xy --> AB(x,y)
    @variable(model, rule[1:4], Bin)
    costs = [4, 4, 3, 4]

    # symbols
    # 1: xx
    # 2: xy
    @variable(model, symb[1:2], Bin)

    # constaints
    
    # rules depend on their RHS symbols
    @constraint(model, deps_rule1, rule[1] <= prod(symb[[1]])) # rule1 needs xx
    @constraint(model, deps_rule2, rule[2] <= prod(symb[[2]])) # rule2 needs xy

    # symbols depend on rules to expand them
    @constraint(model, lhs_xxy, sum(rule[[1,2]]) == 1) # =1 rule for xxy
    @constraint(model, lhs_xx, sum(rule[[3]]) == symb[1]) # 1 rule for xx iff xx
    @constraint(model, lhs_xy, sum(rule[[4]]) == symb[2]) # 1 rule for xy iff xy

    # objective function
    @objective(model, Min, sum(costs[i] * rule[i] for i in 1:4))

    optimize!(model)
    
    return (rules = value.(rule), symbs = value.(symb))
end

# loading rulesets

@sum_type GrammarSymbol begin
    Term(x)
    NonTerm(x)
end

SumTypes.show_sumtype(io::IO, x::GrammarSymbol) = @cases x begin
    Term(t) => print(io, "T($t)")
    NonTerm(nt) => print(io, "NT($nt)")
end

struct Rule
    lhs
    type :: String
    params :: Vector{GrammarSymbol}
end

function rule_params(rule::Rule)    
    deps = []
    for p in rule.params
        @cases p begin
            Term => nothing
            NonTerm(s) => push!(deps, s)
        end
    end
    deps
end

unfold_params(rule::Rule) = [rule.params[codepoint(var)-0x40] for var in rule.type]

rule_dl_cost(rule::Rule) = 1 + length(rule.params)

rule_dl2_cost(rule::Rule) = 1 + length(rule.type) # by number of RHS symbols normally

"""
Cost in analogy to Humphreys et al. (2021):
- 1 for the meta-rule symbol (AA/AB/...)
- 1 per parameter of the meta rule
- 1 for a separator symbol to the next rule
"""
rule_dl_cost_humphreyslike(rule::Rule) = 2 + length(rule.params)

function load_ruleset(fn)
    json = JSON.parsefile(fn)

    start = json["start"]
    seq = json["seq"]
    
    symbols = [parse(UInt64, lhs) % Int for lhs in keys(json["determ_rules"])]
    symbol_indices = Dict(symbols[i] => i for i in 1:length(symbols))

    mkparam(p) = if p["symbtype"] == "T"
        Term(p["value"])
    else
        NonTerm(p["value"])
    end
    mkrule(l, r) = Rule(parse(UInt64, l) % Int, r["ruletype"], [mkparam(p) for p in r["params"]])
    
    rules = [mkrule(lhs, rhs)
             for (lhs, rhss) in json["determ_rules"]
                 for rhs in rhss]
    rule_indices = Dict(rules[i] => i for i in 1:length(rules))
    rule_groups = Dict{Int64,Vector{Rule}}()
    for rule in rules
        if rule.lhs ∈ keys(rule_groups)
            push!(rule_groups[rule.lhs], rule)
        else
            rule_groups[rule.lhs] = [rule]
        end
    end
    
    (start=start,
     seq=seq,
     rules=rules,
     rule_indices=rule_indices,
     rule_groups=rule_groups,
     symbols=symbols,
     symbol_indices=symbol_indices)
end

function load_minimal_ruleset(fn)
    json = JSON.parsefile(fn)

    mkparam(p) = if p["symbtype"] == "T"
        Term(p["value"])
    else
        NonTerm(p["value"])
    end
    mkrule(rule) = Rule(rule["lhs"], rule["ruletype"], [mkparam(p) for p in rule["params"]])

    rules = [mkrule(rule) for rule in json["rules"]]
    
    (start = json["start"],
     seq = json["seq"],
     runtime = json["runtime"],
     rules = rules)
end

function print_ruleset(ruleset)
    show_symbol(s) = "S$(ruleset.symbol_indices[s])"

    show_param(p) = @cases p begin
        Term(t) => string(t)
        NonTerm(symb) => show_symbol(symb)
    end
    
    println("start=", show_symbol(ruleset.start))
    println("seq=", ruleset.seq)

    for (lhs, rhss) in ruleset.rule_groups
        println(show_symbol(lhs))
        for rhs in rhss
            i = ruleset.rule_indices[rhs]
            t = rhs.type
            params = join([show_param(p) for p in rhs.params], ", ")
            println("  r$(i): --> $(t)($(params))")
        end
    end
end

function print_minimal_ruleset(ruleset, rules)
    show_symbol(s) = "S$(ruleset.symbol_indices[s])"

    show_param(p) = @cases p begin
        Term(t) => string(t)
        NonTerm(symb) => show_symbol(symb)
    end

    println("\nminimal ruleset:")
    println("  start=", show_symbol(ruleset.start))
    println("  seq=", ruleset.seq)
    println("  rules:")

    for rule in rules
        lhs = show_symbol(rule.lhs)
        #i = ruleset.rule_indices[rule]
        t = rule.type
        params = join([show_param(p) for p in rule.params], ", ")
        println("    $(lhs): --> $(t)($(params))")
    end
end

function minimal_ruleset_to_json(ruleset, rules, runtime=nothing)
    show_symbol(s) = "S$(ruleset.symbol_indices[s])"

    show_param(p) = @cases p begin
        Term(t) => (symbtype="T", value=string(t))
        NonTerm(symb) => (symbtype="NT", value=show_symbol(symb))
    end

    out_rules = []

    for rule in rules
        lhs = show_symbol(rule.lhs)
        t = rule.type
        params = [show_param(p) for p in rule.params]
        push!(out_rules, (lhs=lhs, ruletype=t, params=params))
    end

    (start=show_symbol(ruleset.start),
     seq=ruleset.seq,
     runtime=runtime,
     rules=out_rules)
end


function minimize_ruleset(ruleset, cost_fn=rule_dl_cost_humphreyslike)
    model = Model(() -> Gurobi.Optimizer(GRB_ENV))
    # model = Model(HiGHS.Optimizer)

    rules = ruleset.rules
    symbols = ruleset.symbols
    costs = cost_fn.(rules)

    # helpers

    symboli(s) = ruleset.symbol_indices[s]
    
    ruledeps(r) = symboli.(rule_params(r))

    ruleopts(lhs) =
        [ruleset.rule_indices[rule] for rule in ruleset.rule_groups[lhs]]

    ## model variables
    
    # one variable per rule
    @variable(model, rulevar[1:length(rules)], Bin)

    # one variable per symbol
    @variable(model, symbvar[1:length(symbols)], Bin)

    ## constraints
    
    # rules depend on their RHS symbols
    for i in 1:length(rules)
        deps = ruledeps(rules[i])
        if length(deps) > 0
            @constraint(model, rulevar[i] <= sum(symbvar[deps]) / length(deps))
        end
    end

    # symbols depend on rules that expand them
    for i in 1:length(symbols)
        @constraint(model, symbvar[i] == sum(rulevar[ruleopts(symbols[i])]))
    end

    # the starting symbol is required
    @constraint(model, symbvar[symboli(ruleset.start)] == 1)

    ## objective function

    @objective(model, Min, sum(rulevar[i] * costs[i] for i in 1:length(rules)))

    optimize!(model)
    
    t = MOI.get(model, Gurobi.ModelAttribute("Runtime"))
    
    return rules[Bool.(value.(rulevar))], t
end

function minimize_ruleset_mc(ruleset; cost_fn=rule_dl_cost_humphreyslike, n=100)
    rules = ruleset.rules
    symbols = ruleset.symbols
    costs = cost_fn.(rules)

    # helpers

    symboli(s) = ruleset.symbol_indices[s]
    
    ruledeps(r) = symboli.(rule_params(r))

    ruleopts(lhs) =
        [ruleset.rule_indices[rule] for rule in ruleset.rule_groups[lhs]]

    function sample_rules()
        agenda = [ruleset.start]
        min_rules = []
        while !isempty(agenda)
            lhs = popfirst!(agenda)
            candidates = ruleset.rule_groups[lhs]
            rule = rand(candidates)
            push!(min_rules, rule)
            push!(agenda, rule_params(rule)...)
        end
        min_rules
    end

    min_set = nothing
    min_cost = nothing
    for i in 1:n
        new_set = sample_rules()
        new_cost = sum(cost_fn.(new_set))
        if min_cost == nothing || new_cost < min_cost
            min_set = new_set
            min_cost = new_cost
        end
    end

    min_set, min_cost
end

function test_ruleset(fn)
    ruleset = load_ruleset(fn)
    min_rules, t = minimize_ruleset(ruleset)
    print_minimal_ruleset(ruleset, min_rules)
    println("solving time: ", t)
    # JSON.print(minimal_ruleset_to_json(ruleset, min_rules, t), 2)
end

function test_ruleset_mc(fn)
    ruleset = load_ruleset(fn)
    min_rules, cost = minimize_ruleset_mc(ruleset)
    print_minimal_ruleset(ruleset, min_rules)
    print("cost: ", cost)
end

function time_subs(n=32)
    times = []
    for i in 2:n
        ruleset = load_ruleset("../data/subs/n$(i).json")
        min_rules, t = minimize_ruleset(ruleset)
        push!(times, t)
    end

    df = DataFrame(length=2:n, time=times)
    CSV.write("../data/sub_times.tsv", df, delim='\t')
    
    times
end
