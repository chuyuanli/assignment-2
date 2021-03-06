## Exo 1
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- median(d_1[[var]]) - median(d_2[[var]])
  return(result)
}


randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}


permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    permutation <- randomize(d, var)
    permutation_statistics[i] <- statistic(permutation, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}


p_value_mc_right <- function(simulated, observed) {
  return(mean(simulated >= observed))
}

p_value_mc_left <- function(simulated, observed) {
  return(mean(simulated <= observed))
}


## Exo 3
permutation_t_test <- function(d, value, grouping_var, group1, group2, N_SAMPLES=9999){
  statistics <- rep(NA, N_SAMPLES)
  for (i in 1:N_SAMPLES){
    d[[grouping_var]] <- sample(d[[grouping_var]])
    # run t-test for permutation groups
    statistics[i] <- t.test(dplyr::filter(d, get(grouping_var) == group1)$value, dplyr::filter(d, get(grouping_var) == group2)$value)$statistic
  } 
  return (statistics)
}





