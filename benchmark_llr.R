setwd("~/Documents/GitHub/s610_lab3")
source("llr_functions.R")
# install.packages("bench")
library(microbenchmark)

microbenchmark(llr)
