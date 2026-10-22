get_kl_divergence <- function(p, q) {
  # What information is lost when using q to approximate p?
  eps <- 1e-12
  p <- p + eps
  q <- q + eps
  sum(p * log(p / q, base = 2))
}

get_kl_divergences_mtx <- function(P, Q) {
  # What information is lost when using Q to approximate P?
  eps <- 1e-12
  P <- P + eps
  Q <- Q + eps
  rowSums(P * log(P / Q, base = 2))
}
