# ALAAM ----
# Idea: ALAAM (Auto-Logistic Actor
# Attribute Model)  the network is FIXED and a binary
# actor attribute (e.g. behavior binary) is the outcome. 
# Estimation here is Bayesian (MCMC) -> check convergence!
# Code: balaam.R (Koskinen) | Docs: Daraganova & Robins (2013);
#       Parker, Pallotti, Lomi & Koskinen (2022)

# INSTALL ----
pkgs <- c("MASS", "mvtnorm", "coda")  # needed by balaam.R
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

# SOURCE ----
source("balaam.R")

# DATA ----
# 60 actors, random undirected network, one covariate, simulated outcome y
set.seed(1)
n   <- 60
ADJ <- matrix(0, n, n)
ADJ[upper.tri(ADJ)] <- rbinom(n * (n - 1) / 2, 1, 0.08)
ADJ <- ADJ + t(ADJ)                      # symmetric, empty diagonal
age <- as.matrix(round(rnorm(n, 30, 5)) - 30) # centred age (helps mixing)

# y: logistic in age, plus a nudge from neighbours (contagion)
y <- rbinom(n, 1, plogis(-0.5 + 0.05 * age))
for (i in 1:10) {
  nb <- ADJ %*% y
  y  <- rbinom(n, 1, plogis(-1 + 0.05 * age + 0.5 * nb))
}
table(y)

# MODEL ----
# effects (in order): intercept, contagion, covariates (here: age)
# Iterations: increase (e.g. 10000) for real analyses
set.seed(1)
res <- BayesALAAM(y = y, ADJ = ADJ, directed = FALSE,
                  covariates = age, Iterations = 3000, silent = TRUE)

# RESULTS ----
res$ResTab                        # posterior mean, sd, ESS...
effnames <- c("intercept", "contagion", "age")

# TRACE + POSTERIOR ----
par(mfrow = c(3, 2))
for (k in 1:3) {
  plot(res$Thetas[, k], type = "l", main = effnames[k], ylab = "", xlab = "iteration")
  hist(res$Thetas[-(1:500), k], main = effnames[k], xlab = "", col = "grey85")
}
par(mfrow = c(1, 1))

# INTERPRETATION ----
# contagion > 0 (credible interval above 0): actors tied to y = 1 alters
# are more likely to have y = 1 themselves, given the covariates.
