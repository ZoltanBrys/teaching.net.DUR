# ERGM ----
# Idea: explain an observed network (a "state") by tie-generating
# mechanisms g(y) such as edges, triangles, homophily:
#   log P(Y = y) = theta' g(y) - log k(theta)
# theta = change in log-odds of a tie per unit of g(y), given the rest.
# Estimation is MCMC-based -> check convergence!
# Package: ergm | Docs: https://statnet.org/workshop-ergm/ergm_tutorial.html

# INSTALL ----
if (!requireNamespace("ergm", quietly = TRUE)) install.packages("ergm")
library(ergm)

# DATA ----
# Florentine families: marriage ties, vertex attribute "wealth"
data(florentine)
flomarriage
plot(flomarriage, vertex.cex = 2, label = network.vertex.names(flomarriage))

# OBSERVED STATISTICS ----
summary(flomarriage ~ edges + triangle + nodecov("wealth"))

# BASELINE MODEL ----
# edges only: theta = logit(density)
fit1 <- ergm(flomarriage ~ edges)
summary(fit1)

# WEALTH MODEL ----
# do wealthier families have more ties?
# force.main = TRUE: use MCMC even though this small model has an exact
# solution, so that we can demonstrate the diagnostics below
set.seed(42)
fit2 <- ergm(flomarriage ~ edges + nodecov("wealth"),
             control = control.ergm(force.main = TRUE))
summary(fit2)
exp(coef(fit2))   # odds ratios

# CONVERGENCE ----
# trace plots should look like noise around 0
mcmc.diagnostics(fit2)

# GOODNESS OF FIT ----
# compare observed vs simulated degree, ESP, geodesics
plot(gof(fit2))

# we alse have VIF and AME-s too with ergMargins package!