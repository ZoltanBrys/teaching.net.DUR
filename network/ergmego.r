# ERGM.EGO ----
# Idea: ERGM needs a complete network. Surveys often give only EGO data
# (a sample of people + their alters). ergm.ego fits an ERGM to such
# ego-level statistics, using a simulated pseudo-population.
# Package: ergm.ego | Docs: vignette("ergm.ego"), https://statnet.org/workshop-ergm-ego/

# INSTALL ----
if (!requireNamespace("ergm.ego", quietly = TRUE)) install.packages("ergm.ego")
library(ergm.ego)

# DATA ----
# faux.mesa.high (simulated school friendships, shipped with ergm) is
# turned into ego data, as in the official example
data(faux.mesa.high)
fmh.ego <- as.egor(faux.mesa.high)
fmh.ego

# EGO STATISTICS ----
# what an ego survey can give us
summary(fmh.ego ~ edges + degree(1) + nodematch("Sex") + absdiff("Grade"))

# MODEL ----
# popsize: size of the population the sample represents
set.seed(1)
fit <- ergm.ego(
  fmh.ego ~ edges + degree(1) + nodematch("Sex") + absdiff("Grade"),
  popsize = network.size(faux.mesa.high)
)
summary(fit)

# DIAGNOSTICS ----
mcmc.diagnostics(fit)
plot(gof(fit))
