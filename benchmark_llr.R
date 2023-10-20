
source("llr_functions.R")
source("test_llr.R")

# install.packages("bench")
library(bench)


microbenchmark::microbenchmark(
  llr(x, y, z, omega = 1),
  make_weight_matrix(0.2,x,omega = 2),
  make_predictor_matrix(rnorm(5)))
