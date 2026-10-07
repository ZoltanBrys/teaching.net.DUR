# DYNAM ----
# Idea: when events are observed in continuous time (calls, messages,
# treaties) we model the PROCESS, not a network state. DyNAM = Dynamic
# Network Actor Model: actors become active (rate) and then choose a
# partner (choice), driven by effects like inertia, reciprocity, transitivity.
# Package: goldfish | Docs: vignette("teaching2", package = "goldfish")

# INSTALL ----
if (!requireNamespace("goldfish", quietly = TRUE)) install.packages("goldfish")
library(goldfish)

# DATA ----
# Social_Evolution: students in a dorm; "calls" = timestamped phone calls
data("Social_Evolution")
head(calls)
head(actors)

# NETWORK + EVENTS ----
# the network starts empty and is updated by every call event
callNet <- defineNetwork(nodes = actors, directed = TRUE) |>
  linkEvents(changeEvent = calls, nodes = actors)

# dependent events: the calls we want to explain
callsDep <- defineDependentEvents(
  events = calls, nodes = actors, defaultNetwork = callNet
)

# MODEL ----
# subModel = "choice": given a caller, whom do they call? "rate" model is not run
fit <- estimate(
  callsDep ~ inertia(callNet) + recip(callNet) + trans(callNet),
  model = "DyNAM", subModel = "choice"
)
summary(fit)

# INTERPRETATION ----
# inertia > 0: repeat calls to the same person
# recip   > 0: calls get returned
# trans   > 0: calling friends of those I called
