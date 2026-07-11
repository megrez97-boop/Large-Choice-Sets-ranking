# installing from GitHub
install.packages("devtools") # if required
devtools::install_github("EllaKaye/BradleyTerryScalable", build_vignettes = TRUE)
library(BradleyTerryScalable)

# for small stimulation
data(citations)
# note: transfor the data to the form which this package can deal with
citations_btdata <- btdata(citations)
# note: checking connectivity
summary(citations_btdata)

#-----result-----
# Number of items: 4 
# Density of wins matrix: 1 
# Fully-connected: TRUE 
#----------------

#note: since it is fully connected ,we use 1 in bffit function
citations_fit <- btfit(citations_btdata, 1)

#note: checking the info for the model
summary(citations_fit)

#-----result-----
# $call (note: R code used to execute the model fitting)
# btfit(btdata = citations_btdata, a = 1)

# $item_summary (data frame with fit results for each item)
# A tibble: 4 × 3
# component    item         estimate
# <chr>        <chr>           <dbl>
#  1 full_dataset JRSS-B          1.06 
#  2 full_dataset Biometrika      0.790
#  3 full_dataset JASA            0.310
#  4 full_dataset Comm Statist   -2.16 
#
# note: estimate is estimated strength parameter on the log scale. 
# note: Higher value = higher strength/rank

# $component_summary (note:data frame summarizing the status of the fitted components)
# A tibble: 1 × 4
# component    num_items iters converged
# <chr>            <int> <int> <lgl>    
#  1 full_dataset         4     2 TRUE
#
# note: num_items is count of items in that component
# note: iters = iterations convergence
#----------------

# now we do a simulation using the data we got， 100 times and it is independent
citations_sim <- simulate(citations_fit, nsim = 100, seed = 1)
# some result from simulation
citations_sim[1:2]





#=====================================================================================
#=====================================================================================
# for large model
# important note: they dont have real data for tournament, they just build a data
# that looks like real tournament data
library(Matrix)
library(dplyr)
library(ggplot2)
set.seed(1989)
# setting no of player(item)
n_items <- 1000
# create a poisson distribution, that mean = 1 to simulate total number of 
# comparisons  between 1000 players
#
# note that we choose posisson with mean 1 as produces non-negative integer 
# counts (0, 1, 2, etc.), which is necessary for recording the number of matches 
# played, we also assume it is incomplete (sparse) tournament, where most players 
# do not compete against each other

Nvalues <- rpois(n = n_items * (n_items - 1) / 2, lambda = 1)

poisdis <- data.frame(Nvalues = Nvalues)

ggplot(poisdis, aes(x = Nvalues)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Distribution of Poisson(1) Samples",
       x = "Value", y = "Frequency")

# logic function for later use
notzero <- Nvalues > 0

# build a empty matrix
Nmatrix <- Matrix(nrow = n_items, ncol = n_items)
# Identify the element which is not 0, later we want to fill into the matrix
ij <- which(lower.tri(Nmatrix), arr.ind = TRUE)[notzero, ]
# fill in the matrix with the generated data
Nmatrix <- sparseMatrix(
  i = ij[, 1],
  j = ij[, 2],
  x = Nvalues[notzero],
  symmetric = TRUE,
  dims = c(n_items, n_items)
)



# now for the BT part
# generating "hidden ability value" for each player, assuming it is normally distributed
# divide by 4 to reduce the sd of generated data
pi_vec <- exp(rnorm(n_items) / 4)

#-----juz for understanding-----
# testrnorm <- rnorm(n_items) 
# exptestrnorm <- exp(testrnorm)
# exptestrnorm
#-------------------------------

# now we normalize pi_vec so its mean is 1,eliminate parameter redundancy caused
# by the Bradley-Terry model's scale invariance （thx gpt）
pi_vec <- pi_vec / mean(pi_vec)
# juz like the small data set, we do a simulation
# given the "hidden ability value" and "the number of time they played to each other"
big_matrix <- simulate_BT(pi_vec, Nmatrix, nsim = 1, seed = 1)[[1]]
# convert the resulting matrix into a btdata object(the form this package can deal with)
big_btdata <- btdata(big_matrix)

# check if it is fully connected
summary(big_btdata)
# fit the model using MLE (a=1)
the_model <- btfit(big_btdata, a = 1)
# extract the fitted ability parameters (pi)
pi_fitted <- the_model$pi$full_dataset




#plot
plot_df <- tibble(
  x = log(pi_vec[as.numeric(names(pi_fitted))]), 
  y = log(pi_fitted)
)

ggplot(plot_df, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_abline() + 
  xlab("true strength") +
  ylab("maximum likelihood estimate") +
  ggtitle("1000-player simulation from a Bradley-Terry model") +
  theme(plot.title = element_text(hjust = 0.5))