# MRQAP ----
# Idea: regress one network (matrix) on several other matrices, with
# p-values from node permutations (QAP). Extension of the Mantel test.
# Package: sna | Docs: ?sna::netlm ; Dekker et al. (2007), Psychometrika

# INSTALL ----
if (!requireNamespace("sna", quietly = TRUE)) install.packages("sna")
library("sna")

# DATA ----
# 20 nodes: outcome y depends on x1 but not on x2 (simulated)
set.seed(42)
x1 <- sna::rgraph(20, tprob = 0.3)   # e.g. friendship
x2 <- sna::rgraph(20, tprob = 0.3)   # e.g. same organisation
y  <- sna::rgraph(20, tprob = 0.1)   # e.g. collaboration
y[x1 == 1] <- rbinom(sum(x1 == 1), 1, 0.6)

# MODEL ----
# nullhyp = "qapspp": Dekker's double semi-partialling permutation,
# robust to collinear predictors
fit <- sna::netlm(y, list(x1, x2), nullhyp = "qapspp", reps = 500)
summary(fit)

# NOTE ----
# netlm = linear model (valued ties); sna::netlogit() is the logistic
# version for binary ties. p-values are in the "Pr(>=b)" columns.