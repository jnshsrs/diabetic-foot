n <- 4e2
X <- data.frame(
  intercept = 1,
  p = sample(1:3, size = n, replace = TRUE),
  e = sample(1:3, size = n, replace = TRUE),
  d = sample(1:3, size = n, replace = TRUE),
  i = sample(1:4, size = n, replace = TRUE),
  s = sample(1:2, size = n, replace = TRUE)
)

betas <- runif(n = 6, min = 0, 2)
y <- as.matrix(X) %*% log(betas)

y <- 1 / (1 + exp(-1 * y))

X$y <- purrr::map_int(y, rbinom, n = 1, size = 1)
fit <- glm(y ~ -1 + ., data = X, family = binomial(link = "logit"))
estimates <- exp(coef(fit))

abs(estimates - betas)

options(mc.cores = parallel::detectCores())

model <- rstanarm::stan_glm(
  y ~ -1 + .,
  data = X,
  prior_intercept = normal(),
  prior = normal(),
  family = binomial(link = "logit"),
  QR = FALSE
)
library(rstanarm)
library(bayesplot)
shuffle(1:10)
unloadNamespace("pedis")
plot(model)
unloadNamespace("pedis")

prior_summary(model)
summary(model)
estimates_bayes <- exp(coef(model))

abs(estimates_bayes - betas)

posterior <- as.matrix(model)
posterior <- apply(X = posterior, MARGIN = 2, FUN = exp)

bayesplot::mcmc_areas(posterior,
           pars = c("p", "e", "d", "i", "s"),
           prob = 0.95) + 
  geom_vline(xintercept = 1) +
  coord_cartesian(xlim = c(0, 7))



