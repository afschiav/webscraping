#####README####
#This example is taken from http://modernstatisticalworkflow.blogspot.com/2017/11/bayesian-instrumental-variables-with.html:

#Below I'll give you code for two versions of the model. The first one has a boring prior on ?? but assumes multiple
#endogenous regressors. The second assumes a single endogenous regressor and uses the hierarchical prior.
#You could of course do both, but the sorts of situations I'm thinking about you having good prior information 
#is more when you have a single effect you care about, not many.

#Note that in this implementation we allow you to provide a flag for whether or not you'd like it to estimate;
#if you don't estimate, it'll generate fake data, This is an excellent way to check that the model code is correct.
#I'll show you how to do that below.



#####LIBRARIES####
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Industry elasticites")

library(rstan)
options(mc.cores = parallel::detectCores())


#####Fake Data Example #####

# Compile the model
compiled_model <- stan_model("classic_iv_hierarchical_prior.stan")
  

# Let's make some fake data

N <- 1000 # Number of observations
PX <- 5 # Number of exogenous variables
PZ <- 2 # Number of instruments
J <- 10 # Number of previous studies

# Previous study parameters, drawn from the prior
beta_hat <- rnorm(1, 0, 1)
sigma_beta <- truncnorm::rtruncnorm(1, a = 0)
beta_j <- rnorm(J, beta_hat, sigma_beta)
se_j <- truncnorm::rtruncnorm(J, a = 0)
b_j <- rnorm(J, beta_j, se_j)

# Exogenous variables (make them correlated with a random correlation matrix)
X_exog <- MASS::mvrnorm(N, rep(0, PX), cor(matrix(rnorm(PX*(PX+5)), PX+5, PX)))

# We have to feed it some dummy endogenous variables but these won't make a difference
X_endog <- rnorm(N)

# Some fake instruments
Z <- MASS::mvrnorm(N, rep(0, PZ), cor(matrix(rnorm(PZ*(PZ+5)), PZ+5, PZ)))

Y_outcome <- rnorm(N)

data_list <- list(N = N, PX = PX, PZ = PZ, J = J,
                  b_j = b_j, se_j = se_j, X_exog = X_exog, X_endog = X_endog, 
                  Z = Z, Y_outcome = Y_outcome, 
                  run_estimation = 0)

# Now let's get some fake draws
draws_from_model <- sampling(compiled_model, data = data_list, iter = 50, chains = 1)

#What we've done above is to first compiled the Stan code into machine code. Then we made some fake data
#for the "meta-analysis" we have conducted. Then we made fake values for X and Z (we had to make fake values
#for y and the endogenous regressor also because Stan needs us to give it a data list with values for everything 
#declared in the data block). Finally we asked Stan to "sample" from the model. Because we weren't evaluating the
#likelihood, this is equivalent to drawing from the prior, then drawing from the generative model, for 50 iterations.


#What we'll do now is get the fake data out of the fitted Stan object and run Stan on the fake data.


# Let's use the next to last non-warm-up draw as our fake data (draw 24)
y_sim <- extract(draws_from_model, pars = "y_sim")[[1]][24,]
x_endog <- extract(draws_from_model, pars = "x_endog")[[1]][24,]
true_beta <- extract(draws_from_model, pars = "beta_ours")[[1]][24]

# Now let's make a new data list
data_list_2 <- data_list
data_list_2$X_endog <- x_endog
data_list_2$Y_outcome <- y_sim
data_list_2$run_estimation <- 1

# Fit the model
fitted_model <- sampling(compiled_model, data = data_list_2, cores = 4, chains = 4, iter = 1000)

print(fitted_model, pars = c("beta_ours"))

print(paste0("And the true value of beta_ours is ", round(true_beta, 3)))

#Now let's stick the data together and estimate the same model using vanilla IV:

library(AER)

iv_data <- data.frame(Y_outcome = y_sim, X_endog = x_endog, X_exog, Z)

iv_fit <- ivreg(Y_outcome ~ X_endog + X1+X2+X3+X4+X5| X1.1 + X2.1+ X1+X2+X3+X4+X5, data = iv_data)
# That fit instantly! 

# ...but
summary(iv_fit)
