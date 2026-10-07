# NEURALNET ----
# Idea: a neural network is layers of "neurons". Each neuron takes a weighted
# sum of its inputs plus a bias and squashes it with an activation function:
#   h = logistic(w1*x1 + w2*x2 + b)
# Training = backpropagation: compute the error at the output, push it back
# through the layers to get the gradient of each weight, then update
#   w <- w - learningrate * gradient
# Package: neuralnet | Docs: ?neuralnet::neuralnet ; Guenther & Fritsch (2010), R Journal

# INSTALL ----
if (!requireNamespace("neuralnet", quietly = TRUE)) install.packages("neuralnet")
library("neuralnet")

# DATA ----
# probabilistic relation: P(y = 1) rises with x1 and falls with x2
set.seed(42)
n  <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
p  <- plogis(2 * x1 - 1.5 * x2)
y  <- rbinom(n, 1, p)          # noisy 0/1 outcome, not deterministic
d  <- data.frame(y, x1, x2)

train <- d[1:150, ]
test  <- d[151:200, ]

# MODEL ----
# hidden        = neurons per hidden layer: c(3) = one hidden layer with 3 neurons
# act.fct       = activation function of the neurons
# err.fct       = loss to minimise ("ce" cross-entropy for 0/1 outcomes)
# linear.output = FALSE -> logistic output = probability
# algorithm     = "backprop" is plain gradient descent (default "rprop+" is
#                 a faster variant); needs learningrate
# threshold     = stop when the error gradient is below this value
# stepmax       = max training steps; rep = number of random restarts
set.seed(1)
nn <- neuralnet::neuralnet(
  y ~ x1 + x2, data = train,
  hidden = c(3),
  act.fct = "logistic",
  err.fct = "ce",
  linear.output = FALSE,
  algorithm = "backprop",
  learningrate = 0.01,
  threshold = 0.5,
  stepmax = 1e6
)

# VISUALISE ----
# black = weights (layer to layer), blue = biases (the "1" nodes)
plot(nn, rep = "best")

# PREDICT ----
# forward pass on unseen data: probabilities, then class by 0.5 cut-off
pred <- predict(nn, test)
mean((pred > 0.5) == test$y)     # accuracy (the noise caps it below 100%)

# COMPARE ----
# the "true" probability p is known, so how close is the network?
cor(pred, p[151:200])

# NOTE ----
# More neurons/layers = more flexible but easier to overfit: always judge
# on test data. Inputs should be scaled (here already ~ N(0, 1)).
