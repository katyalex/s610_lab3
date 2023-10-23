
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}
# Create a function to perform element-wise matrix multiplication
elementwise_matrix_mult <- function(A, B) {
  return(sapply(seq_len(ncol(A)), function(i) A[, i] %*% B[i, , drop = FALSE]))
}
compute_f_hat = function(z, x, y, omega) {
  Wz = diag(make_weight_matrix(z, x, omega))
    X = make_predictor_matrix(x)
    f_hat <- c(1, z) %*% solve(t(X) %*% sweep(x = X, MARGIN = 1, STATS = Wz, FUN = "*")) %*% t(X) %*% (Wz * y)
    return(f_hat)
}



make_weight_matrix <- function(z, x, omega) {
  r <- abs(x - z)/omega
  w <- ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0) 
  return(diag(w))
}

make_predictor_matrix <- function(x) {
  X <- cbind(1,x)
  return(X)
}

make_predictor_matrix(1:4)
