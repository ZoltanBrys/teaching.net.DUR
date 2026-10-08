set.seed(42)

xs_length <- 20
n <- 200

df <- data.frame(
  y1 = sample(0:1, n, replace = TRUE),
  replicate(
    xs_length,
    sample(0:1, n, replace = TRUE)
  )
)

names(df) <- c("y1", paste0("x", 1:xs_length))

model <- glm(
  y1 ~ .,
  data = df,
  family = binomial
)

model

summary(model)
