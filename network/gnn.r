# GNN ----
# WHY ----
# Ordinary models treat cases as independent rows. In a network, a node's
# neighbours carry information about it (homophily, influence, shared
# context). A GNN lets every node "borrow" information from its neighbours.
# Typical uses: classify nodes (who belongs to which community / role),
# predict links, classify whole graphs (molecules), recommend.
#
# WHAT GOES IN / OUT ----
# input : adjacency matrix A (n x n) + feature matrix X (n nodes x f features)
# output: a new vector per node (embedding) -> used to predict a label
# So: X is your node attribute table, A is your network; nothing else.
#
# ONE LAYER = TWO STEPS ----
#   1) MESSAGE PASSING: every node averages its own and its neighbours'
#      features        H = A_hat %*% X   (A_hat = row-normalised A + self-loops)
#   2) NEURAL NET:     out = ReLU( H %*% W )
#      W = weights learned by backpropagation (as in neuralnet.r),
#      ReLU = max(0, x), a simple non-linearity.
# Stacking k layers lets information travel k steps along the edges.
#
# PRACTICAL NOTES ----
# - Layers: 2-3 is typical. Too many layers -> all nodes look alike
#   ("over-smoothing", see plot 3).
# - Hidden size: 8-64 neurons is plenty for most small problems.
# - The graph is used in training AND prediction: nodes of a test set may
#   be connected to training nodes (that is the point).
# - Without node attributes, use the identity matrix as features.
# - Big graphs: dense A does not scale; use sparse matrices or sampling
#   (e.g. Python PyTorch Geometric; in R, the torch package is the base).
# - Compare with a model WITHOUT the graph: only then you know the network
#   added something (done below: A_hat replaced by the identity matrix).
#
# THIS SCRIPT ----
# A tiny graph (two groups + one bridge), one noisy feature, and plots
# showing the mechanism step by step.
# Package: torch | Docs: https://torch.mlverse.org/start/ ; Kipf & Welling (2017)

# INSTALL ----
if (!requireNamespace("torch", quietly = TRUE)) install.packages("torch")
library(torch)
if (!torch_is_installed()) install_torch()  # one-time libtorch download

# DATA ----
# 10 nodes: group A = 1-5, group B = 6-10, each fully connected inside,
# and a single bridge 5 - 6
n <- 10
group <- rep(c(0, 1), each = 5)
A <- matrix(0, n, n)
A[1:5, 1:5] <- 1
A[6:10, 6:10] <- 1
diag(A) <- 0
A[5, 6] <- A[6, 5] <- 1

# one noisy feature per node: group signal + a lot of noise
set.seed(3)
x <- group + rnorm(n, sd = 0.7)

# layout for plotting: group A left, group B right
ang <- seq(0, 2 * pi, length.out = 6)[1:5]
xy  <- rbind(cbind(-2 + cos(ang), sin(ang)), cbind(2 + cos(ang), sin(ang)))

draw <- function(values, main) {
  cols <- colorRampPalette(c("white", "tomato"))(100)
  idx  <- 1 + round(99 * (values - min(values)) / diff(range(values)))
  plot(xy, type = "n", axes = FALSE, xlab = "", ylab = "", main = main,
       xlim = c(-3.3, 3.3), ylim = c(-1.3, 1.3))
  e <- which(A == 1 & upper.tri(A), arr.ind = TRUE)
  segments(xy[e[, 1], 1], xy[e[, 1], 2], xy[e[, 2], 1], xy[e[, 2], 2], col = "grey60")
  points(xy, pch = 21, cex = 5, bg = cols[idx])
  text(xy, labels = round(values, 1), cex = 0.8, font = 2)
}

# MESSAGE PASSING ----
# A_hat = D^-1 (A + I): each row is an averaging recipe (self + neighbours)
A_self <- A + diag(n)
A_hat  <- A_self / rowSums(A_self)
h1 <- A_hat %*% x     # after 1 step
h3 <- A_hat %*% A_hat %*% A_hat %*% x   # after 3 steps

# TRAIN: GCN vs NO GRAPH ----
# task: recover the group from the noisy feature (all nodes labelled)
# the same 2-layer network is run with the graph (A_hat) and without (identity)
X <- torch_tensor(matrix(x, n, 1), dtype = torch_float())
y <- torch_tensor(group + 1, dtype = torch_long())

net <- nn_module(
  initialize = function() {
    self$w1 <- nn_linear(1, 4)
    self$w2 <- nn_linear(4, 2)
  },
  forward = function(x, a) {
    h <- nnf_relu(self$w1(torch_matmul(a, x)))    # message passing + ReLU
    self$w2(torch_matmul(a, h))                   # second round of messages
  }
)

fit <- function(a) {
  a <- torch_tensor(a, dtype = torch_float())
  torch_manual_seed(1)
  m <- net()
  opt <- optim_adam(m$parameters, lr = 0.05)
  loss_hist <- numeric(150)
  for (e in 1:150) {
    opt$zero_grad()
    loss <- nnf_cross_entropy(m(X, a), y)
    loss$backward()
    opt$step()
    loss_hist[e] <- loss$item()
  }
  list(loss = loss_hist, pred = as.integer(torch_argmax(m(X, a), dim = 2)) - 1)
}
with_graph <- fit(A_hat)
no_graph   <- fit(diag(n))
mean(with_graph$pred == group)   # accuracy with graph
mean(no_graph$pred == group)     # accuracy without graph

# PLOTS ----
par(mfrow = c(2, 3), mar = c(1, 1, 3, 1))
draw(x,  "1. raw noisy feature")
draw(h1, "2. after 1 message-passing step")
draw(h3, "3. after 3 steps (smoother)")

par(mar = c(4, 4, 3, 1))
curve(pmax(0, x), -3, 3, lwd = 2, xlab = "input", ylab = "output",
      main = "4. ReLU = max(0, x)")
plot(with_graph$loss, type = "l", lwd = 2, col = "tomato", ylim = c(0, 0.8),
     xlab = "epoch", ylab = "loss", main = "5. training loss")
lines(no_graph$loss, lwd = 2, col = "grey40")
legend("topright", c("with graph", "no graph"), col = c("tomato", "grey40"),
       lwd = 2, bty = "n")

par(mar = c(1, 1, 3, 1))
draw(with_graph$pred, "6. GCN predicted group")
par(mfrow = c(1, 1))
