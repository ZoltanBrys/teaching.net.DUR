# MANTEL TEST ----
# Idea: are two distance matrices correlated?
# Dyads share nodes, so ordinary p-values are wrong. 
# The Mantel test (1967) SCHUFFLES node order (rows and columns together) to get a valid null.
# Package: ecodist | Docs: ?ecodist::mantel


# INSTALL ----
if (!requireNamespace("ecodist", quietly = TRUE)) install.packages("ecodist")
library("ecodist")

# DATA ----
# 20 people: two simulated distance matrices
# e.g. mat1 = distance in an organization, mat2 = shortest path in emailing
set.seed(7)
xy1  <- matrix(rnorm(40), ncol = 2)
mat1 <- dist(xy1)
mat2 <- dist(matrix(rnorm(40), ncol = 2))


# TEST ----
# mantelr = matrix correlation; pval3 = two-sided permutation p-value
res <- ecodist::mantel(mat1 ~ mat2, nperm = 999)
res

# mantelr ~ 0 and pval3 >> 0.05: no association (as built)


# RELATED CASE ----
# same 20 people, but the second matrix is built from the first positions
# plus a lot of noise -> a weak-to-moderate association
mat3 <- dist(xy1 + matrix(rnorm(40, sd = 3), ncol = 2))
ecodist::mantel(mat1 ~ mat3, nperm = 999)


# Read mantelr and pval3 (two-sided). pval1 and pval2 are the one-sided
# versions and mirror each other (pval2 is about 1 - pval1).
# now mantelr ~ 0.3 and pval3 < 0.05: association detected

# NOTE ----
# Mantel handles only TWO matrices; for several predictors use MRQAP.