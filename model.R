build_model <- function(n, dist_fun) {
  # Miller–Tucker–Zemlin (MTZ) formulation
  # see https://www.unc.edu/~pataki/papers/teachtsp.pdf
  # for mor information
  MILPModel() %>%
    # we create a variable that is 1 iff we travel from city i to j
    add_variable(x[i, j], i = 1:n, j = 1:n,
                 type = "integer", lb = 0, ub = 1) %>%

    # a helper variable for the MTZ formulation of the tsp
    add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%

    # minimize travel distance
    set_objective(sum_expr(colwise(dist_fun(i, j)) * x[i, j], i = 1:n, j = 1:n), "min") %>%

    # you cannot go to the same city
    set_bounds(x[i, i], ub = 0, i = 1:n) %>%

    # leave each city
    add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
    #
    # visit each city
    add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%

    # ensure no subtours (arc constraints)
    add_constraint(u[i] >= 2, i = 2:n) %>%
    add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
}