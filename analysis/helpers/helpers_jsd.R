get_jsd_mtx <- function(P, Q) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  M <- (P + Q) / 2
  kl_to_m <- function(A) {
    r <- A * log2(A / M) # use base 2 for range [0,1]
    r[A == 0] <- 0 # convention 0 * log 0 = 0
    rowSums(r)
  }
  0.5 * kl_to_m(P) + 0.5 * kl_to_m(Q)
}
