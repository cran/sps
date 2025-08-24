## -----------------------------------------------------------------------------
pi <- function(x, n) {
  n * (x / sum(x))
}

# Population with 13 units.
x <- c(1:8, 9.5, 10, 20, 20, 30)

alpha <- 0.15

# Units 11, 12, and 13 have an inclusion probability
# greater than 1 - alpha.
which(pi(x, 8) >= 1 - alpha)

# Now units 9 and 10 have an inclusion probability
# greater than 1 - alpha.
which(pi(x[1:10], 5) >= 1 - alpha)

# After two rounds of removal all inclusion probabilities
# are less than 1 - alpha.
any(pi(x[1:8], 3) >= 1 - alpha)


## -----------------------------------------------------------------------------
pi(x[1:9], 4)[9] >= 1 - alpha


## ----fig.width=8, fig.height=5.33---------------------------------------------
#| fig.alt: >
#|   Diagram showing how to find units that belong in the take-all stratum.
p <- function(x, n) {
  ord <- order(x, decreasing = TRUE)
  s <- seq_len(n)
  possible_ta <- rev(ord[s])
  x_ta <- x[possible_ta] # ties are in reverse
  definite_ts <- ord[seq.int(n + 1, length.out = length(x) - n)]

  x_ta * s / (sum(x[definite_ts]) + cumsum(x_ta))
}

plot(
  1:4,
  p(x, 8)[1:4],
  xlab = "",
  ylab = "p",
  xlim = c(1, 8),
  ylim = c(0, 2),
  pch = 20
)
points(5:8, p(x, 8)[5:8], pch = 19)
abline(1 - alpha, 0, lty = 2)
legend(1, 2, c("take-some", "take-all"), pch = c(20, 19))


## ----fig.width=8, fig.height=5.33---------------------------------------------
#| fig.alt: >
#|   Diagram showing when units first enter the take-all stratum.
plot(
  2:5,
  p(x, 8)[1:4],
  xlab = "",
  ylab = "p",
  xlim = c(1, 9),
  ylim = c(0, 2.5),
  pch = 20
)
points(6:9, p(x, 8)[5:8], pch = 19)
points(1:3, p(x, 9)[1:3], pch = 20, col = "red")
points(4:9, p(x, 9)[4:9], pch = 19, col = "red")
abline(1 - alpha, 0, lty = 2)
legend(1, 2.5, c("take-some", "take-all"), pch = c(20, 19))
legend(1, 2, c("n = 8", "n = 9"), pch = 20, col = c("black", "red"))

