function sort_rules(rules, start)
    rule_dict = Dict(r.lhs => r for r in rules)
    sorted_rules = []
    agenda = [start]
    while !isempty(agenda)
        lhs = popfirst!(agenda)
        rule = rule_dict[lhs]
        push!(sorted_rules, rule)
        push!(agenda, rule_params(rule)...)
    end
    sorted_rules
end

symbol_tex(s::GrammarSymbol) = @cases s begin
    Term(x) => string(x)
    NonTerm(x) => "S_{$(x[2:end])}"
end

function indent_lines(text, indent)
    lines = split(text, "\n")
    pad = repeat(" ", indent)
    join([pad * line for line in lines], "\n")
end

function grammar_tex(grammar; cost_fn=rule_dl_cost_humphreyslike)
    function rule_tex(rule)
        lhs = symbol_tex(NonTerm(rule.lhs))
        params = join(symbol_tex.(rule.params), "\\ ")
        "$(lhs) \\longrightarrow $(params)"
    end

    function rule_meta_tex(rule)
        type = replace(rule.type, "A" => "\\alpha", "B" => "\\beta")
        params = join(symbol_tex.(rule.params), ", ")
        "$(type)($params)"
    end
    
    rule_line(rule, i) = "\$r_{$(i)}\$ & \$ $(rule_tex(rule)) \$ & \$ $(rule_meta_tex(rule)) \$ & $(cost_fn(rule)) \\\\"
    
    rules_tex = [rule_line(r, i)
                 for (i, r) in enumerate(sort_rules(grammar.rules, grammar.start))]

    """
    \\begin{tabular}{llll}
    \\toprule
    rule & & meta rule & cost\\\\
    \\midrule
    $(join(rules_tex, "\n"))
    \\bottomrule
    \\end{tabular}
    """
end

function grammar_qtree(grammar)
    rule_dict = Dict(r.lhs => r for r in grammar.rules)
    
    indent(str, n) = "\n" * repeat("  ", n) * str
    
    subtree(symbol, depth) = @cases symbol begin
        Term(x) => string(x)
        NonTerm(lhs) => begin
            rule = rule_dict[lhs]
            lhs_str = "\$$(symbol_tex(symbol))\$"
            # break lines or not?
            if isempty(rule_params(rule))
                params_str = join(subtree.(unfold_params(rule), 0), " ")
                "[.$(lhs_str) $(params_str) ]"
            else
                param_strs = [indent(subtree(param, depth+1), depth+1) for param in unfold_params(rule)]
                "[.$(lhs_str)$(param_strs...)$(indent("]", depth))"
            end
        end 
    end

    "\\Tree " * subtree(NonTerm(grammar.start), 0)
end
