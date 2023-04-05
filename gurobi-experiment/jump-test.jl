using JuMP
using HiGHS
using Gurobi

const GRB_ENV = Gurobi.Env()

model = Model(() -> Gurobi.Optimizer(GRB_ENV))

@variable(model, x >= 0)
@variable(model, 0 <= y <= 3)
@objective(model, Min, 12x + 20y)
@constraint(model, c1, 6x + 8y >= 100)
@constraint(model, c2, 7x + 12y >= 120)
