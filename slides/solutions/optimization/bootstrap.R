# Code under MIT License by Michael Mayer and Lukas Widmer.
# See file LICENSE.
# Original code at https://github.com/luwidmer/fastR-website/blob/master/materials/2023-12-06%20BBS%20Next%20Gen/code/fastR-example-code/optimizebootstrap.R. # nolint
# Adapted and extended by Daniel Sabanes Bove, March 2024.
# https://inferential.bio

library(tidyverse)
library(data.table)

# Setup ----

n_patients <- 1000
bootstrap_n <- 5000
bootstrap_size <- 100

set.seed(1337)
population <- data.table(
    patient_id = 1:n_patients,
    dummy_measurement = runif(n_patients),
    analysis_flag = rbinom(n_patients, 1, 0.8) > 0
)

# Original ----

impl_1 <- function(population) {
    result <- NULL
    for (i in seq_len(bootstrap_n)) {
        bootstrap_data_rows <- sample(
            x = seq_len(nrow(population)),
            size = bootstrap_size,
            replace = TRUE
        )
        current_bootstrap <- population[bootstrap_data_rows, ]
        analysis_pop <- filter(current_bootstrap, analysis_flag)
        current_result <- tibble(
            bootstrap_index = i,
            computed_output = median(analysis_pop$dummy_measurement)
        )
        result <- bind_rows(result, current_result)
    }
    result
}

# Tibble at the end ----

impl_2 <- function(population) {
  bootstrap_index <- seq_len(bootstrap_n)
  median_vals <- numeric(bootstrap_n)
  for (i in bootstrap_index) {
    bootstrap_data_rows <- sample(
      x = seq_len(nrow(population)),
      size = bootstrap_size,
      replace = TRUE
    )
    current_bootstrap <- population[bootstrap_data_rows, ]
    analysis_pop <- filter(current_bootstrap, analysis_flag)
    median_vals[i] <- median(analysis_pop$dummy_measurement)
  }
  tibble(
    bootstrap_index = bootstrap_index,
    computed_output = median_vals
  )
}

# Efficient subsetting ----

impl_3 <- function(population) {
  bootstrap_index <- seq_len(bootstrap_n)
  median_vals <- numeric(bootstrap_n)
  for (i in bootstrap_index) {
    bootstrap_data_inds <- sample(
      x = seq_len(nrow(population)),
      size = bootstrap_size,
      replace = TRUE
    )
    bootstrap_flags <- population$analysis_flag[bootstrap_data_inds]
    bootstrap_vals <- population$dummy_measurement[bootstrap_data_inds]
    analysis_pop_vals <- bootstrap_vals[bootstrap_flags] 
    
    median_vals[i] <- median(analysis_pop_vals)
  }
  tibble(
    bootstrap_index = bootstrap_index,
    computed_output = median_vals
  )
}

# boot with parallelization ----

library(boot)

impl_4 <- function(population) {
  # From ?boot :
  # A function which when applied to data returns a
  # vector containing the statistic(s) of interest.
  # The first argument passed will always be the original data.
  # The second will be a vector of indices [...] 
  # which define the bootstrap sample.
  boot_fun <- function(data, inds, bootstrap_size) {
    inds_to_use <- head(inds, bootstrap_size)
    bootstrap_flags <- data$analysis_flag[inds_to_use]
    bootstrap_vals <- data$dummy_measurement[inds_to_use]
    analysis_pop_vals <- bootstrap_vals[bootstrap_flags] 
    median(analysis_pop_vals)
  }
  tmp <- boot(
    data = population,
    statistic = boot_fun,
    R = bootstrap_n,
    # Only when we want to use parallel computation, we have to
    # pass this argument as well:
    bootstrap_size = bootstrap_size,
    parallel = "snow",
    ncpus = parallel::detectCores() - 1L
  )
  tibble(
    bootstrap_index = seq_along(tmp$t),
    computed_output = as.numeric(tmp$t)
  )
}

# Rcpp ----

library(Rcpp)

# Have a look at the introduction:
vignette("Rcpp-introduction")

# Just to check that we can use Rcpp correctly:
evalCpp("2 + 2")

# We want to write something like this:
#
# cppFunction("
# bool isOddCpp(int num = 10) {
# bool result = (num % 2 == 1);
# return result;
# }")

# Let's do it, but in the separate file 
# boot_rcpp.cpp
# Very helpful is:
vignette("Rcpp-quickref")
# Also I found:
# https://teuder.github.io/rcpp4everyone_en/
# Generally writing the C++ function will take much longer than
# writing the R functions above; I needed at least 30 mins to write it.
# Be prepared to search online for functions etc.

# Afterwards we can source it like this:
Rcpp::sourceCpp("boot_rcpp.cpp") 

# We can then call like this:
median_vals <- boot_rcpp(population, 1:100, 50)

# We use this now in a small R wrapper:
impl_5 <- function(population) {
  bootstrap_index <- seq_len(bootstrap_n)
  median_vals <- boot_rcpp(
    population, 
    bootstrap_index, 
    bootstrap_size
  )
  tibble(
    bootstrap_index = bootstrap_index,
    computed_output = median_vals
  )
}




