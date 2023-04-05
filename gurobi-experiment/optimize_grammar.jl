using JuMP
using Gurobi
using HiGHS
import JSON
using SumTypes

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
    lhs :: Int64
    type :: String
    params :: Vector{GrammarSymbol}
end

function rule_params(rule::Rule)    
    deps = Int64[]
    for p in rule.params
        @cases p begin
            Term => nothing
            NonTerm(s) => push!(deps, s)
        end
    end
    deps
end

rule_dl_cost(rule::Rule) = 1 + length(rule.params)

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


function minimize_ruleset(ruleset)
    model = Model(() -> Gurobi.Optimizer(GRB_ENV))
    # model = Model(HiGHS.Optimizer)

    rules = ruleset.rules
    symbols = ruleset.symbols
    costs = rule_dl_cost.(rules)

    # helpers

    symboli(s) = ruleset.symbol_indices[s]
    
    ruledeps(r) = symboli.(rule_params(r))

    ruleopts(lhs) =
        [ruleset.rule_indices[rule] for rule in ruleset.rule_groups[lhs]]

    # model variables
    
    # one variable per rule
    @variable(model, rulevar[1:length(rules)], Bin)

    # one variable per symbol
    @variable(model, symbvar[1:length(symbols)], Bin)

    # constraints
    
    # rules depend on their RHS symbols
    for i in 1:length(rules)
        deps = ruledeps(rules[i])
        if length(deps) > 0
            @constraint(model, rulevar[i] <= sum(symbvar[deps] / length(deps)))
        end
    end

    # symbols depend on rules that expand them
    for i in 1:length(symbols)
        @constraint(model, symbvar[i] == sum(rulevar[ruleopts(symbols[i])]))
    end

    # the starting symbol is required
    @constraint(model, symbvar[symboli(ruleset.start)] == 1)

    # objective function

    @objective(model, Min, sum(rulevar[i] * costs[i] for i in 1:length(rules)))

    optimize!(model)

    return rules[Bool.(value.(rulevar))]
end

function test_ruleset(fn)
    ruleset = load_ruleset(fn)
    min_rules = minimize_ruleset(ruleset)
    print_minimal_ruleset(ruleset, min_rules)
end
