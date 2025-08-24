## ----frame--------------------------------------------------------------------
library(sps)
set.seed(123654)

frame <- data.frame(
  revenue = round(rlnorm(1e3) * 1000),
  region = sample(1:3, 1e3, prob = c(0.2, 0.3, 0.5), replace = TRUE)
)

head(frame)


## ----outcome------------------------------------------------------------------
sales <- round(frame$revenue * runif(1e3, 0.5, 2))


## ----allocation---------------------------------------------------------------
allocation <- with(frame, prop_allocation(revenue, 100, region))
allocation


## ----sample-------------------------------------------------------------------
sample <- with(frame, sps(revenue, allocation, region))

survey <- cbind(frame[sample, ], sales = sales[sample])

head(survey)


## ----weights------------------------------------------------------------------
survey$weight <- weights(sample)

head(survey)


## ----estimate-----------------------------------------------------------------
ht <- with(survey, sum(sales * weight))
ht


## ----bias---------------------------------------------------------------------
ht / sum(sales) - 1


## ----variance-----------------------------------------------------------------
repweights <- sps_repweights(weights(sample))

var <- attr(repweights, "tau")^2 *
  mean((colSums(survey$sales * repweights) - ht)^2)

sqrt(var) / ht


## ----variance2----------------------------------------------------------------
sps_var <- function(y, w) {
  y <- y[w > 1]
  w <- w[w > 1]
  n <- length(y)
  total <- sum(y * w)
  n / (n - 1) * sum((1 - 1 / w) * (w * y - total / n)^2)
}

var <- with(
  survey,
  mapply(sps_var, split(sales, region), split(weight, region))
)

sqrt(sum(var)) / ht


## ----prns---------------------------------------------------------------------
frame$prn <- runif(1000)

head(frame)


## ----prn samples--------------------------------------------------------------
pareto <- order_sampling(\(x) x / (1 - x))

sample <- with(frame, sps(revenue, allocation, region, prn))

parsample <- with(frame, pareto(revenue, allocation, region, (prn - 0.5) %% 1))

length(intersect(sample, parsample)) / 100


## ----prn simualtion-----------------------------------------------------------
replicate(1000, {
  s <- with(frame, pareto(revenue, allocation, region))
  length(intersect(sample, s)) / 100
}) |>
  summary()


## ----top up-------------------------------------------------------------------
sample <- with(frame, sps(revenue, allocation, region, prn))

sample_tu <- with(frame, sps(revenue, allocation + c(10, 0, 0), region, prn))

all(sample %in% sample_tu)


## ----critical-----------------------------------------------------------------
Map(\(x) head(becomes_ta(x)), split(frame$revenue, frame$region))


## ----no_tu--------------------------------------------------------------------
set.seed(13026)
x <- rlnorm(10)
u <- runif(10)

becomes_ta(x)

sample <- sps(x, 4, prn = u)

sample %in% sps(x, 5, prn = u)


## ----yes_tu-------------------------------------------------------------------
sample %in% sps(x, 6, prn = u)


## -----------------------------------------------------------------------------
s <- sps_iterator(x, prn = u)
for (i in 1:5) {
  print(s())
}

sps(x, 6, prn = u)


## -----------------------------------------------------------------------------
set.seed(10052)
u <- runif(10)
pareto(x, 2, prn = u) %in% pareto(x, 3, prn = u)

set.seed(10063)
u <- runif(10)
successive <- order_sampling(\(x) log(1 - x))
successive(x, 2, prn = u) %in% successive(x, 3, prn = u)


## ----ht bias------------------------------------------------------------------
sampling_distribution <- replicate(1000, {
  sample <- with(frame, sps(revenue, allocation, region))
  sum(sales[sample] * weights(sample))
})

summary(sampling_distribution / sum(sales) - 1)


## ----tille, fig.width=8, fig.height=5.33--------------------------------------
#| fig.alt: >
#|   Empirical distribution of inclusion probabilities under sequential Poisson
#|   sampling is approximately Guassian.
set.seed(123456)
n <- 5e3
frame1 <- subset(frame, region == 1)

pi_est <- tabulate(
  replicate(n, sps(frame1$revenue, allocation[1])),
  nbins = nrow(frame1)
)

pi <- inclusion_prob(frame1$revenue, allocation[1])

dist <- (pi_est / n - pi) / sqrt(pi * (1 - pi) / n)

plot(
  density(dist, na.rm = TRUE),
  ylim = c(0, 0.5),
  xlim = c(-4, 4),
  ylab = "",
  xlab = "",
  main = "Empirical distribution of inclusion probabilities"
)
lines(seq(-4, 4, 0.1), dnorm(seq(-4, 4, 0.1)), lty = "dashed")
legend("topright", c("empirical", "theoretical"), lty = c("solid", "dashed"))

