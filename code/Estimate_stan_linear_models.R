
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, rstan, shinystan, bayesplot, bridgesampling)
# Import data
#-----------------------------------------------

df <- read_csv("data/metadata2.csv")
df %>% View()
summary(df)

# Set seed
seed = "12345"

# Choose number of iterations for models
n_iter <- 10000
n_chains <- 4

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#source('EstimateDiscountRate/code/stan/stan_utility.R')
# Set data list
#--------------------------------------------------------------------

df <- df %>%
	#	filter(wlfresh == 1) %>%
	mutate(q0 = q0/1000,
		   q1 = q1/1000)

df <- df %>%
	mutate(q01 = (q1 + q0)/2,
		   q_percent = ifelse(q0 == 0, 100, q1 / q0 -1), 
		   lnwtp2 = lnwtp - log(q1- q0),
		   q_change = q1 - q0,
		   us = ifelse(canada == 1, 0, 1))

#scaling predictor variables except binary variables
#str(df)
#df_scaled <- df %>% mutate_at(c("lnyear", "wlfresh", "lninc","q0", "q1" ), ~(scale(.) %>% as.vector))
#df_scaled %>% View()

#Graph of lnacres vrs lnwtp
df %>%
	group_by(canada) %>%
	summarise(count = n(),
			  wtp = mean(exp(lnwtp)),
			  acre = mean(q1-q0)*1000,
			  wtp_acre = wtp/acre)

#Graph of lnwtp vrs lnacres for full data
df %>%
	mutate(lnacres = log((q1-q0)*1000)) %>%
	ggplot(aes(x=lnacres, y = lnwtp, label=canada))+
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0)
summary(df)

#Graph of lnwtp vrs lnacres for freshwater data
df %>%
	mutate(lnacres = log((q1-q0)*1000)) %>%
	filter(wlfresh == 1) %>%
	ggplot(aes(x=lnacres, y = lnwtp, label=canada))+
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0)
summary(df)
#Bayesia models
source("code/rstan_data.R")

init <- list(gamma = 0.08,
			 #	 beta = c(-.5, 0, .2, -0.4, -0.7, 3.1, -2.2, 1.6, -.3, 1.1, -0.02, 1.5),
			 sigma = .5)

init <- list(init = init,
			 init = init,
			 init = init,
			 init = init)

# Linear model (M3c from Moeltner paper)
ma_linear <- stan("code/linearMA_bridgesampling.stan", 
				  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"), init = init,
				  data=data_stan_whole, iter=n_iter, chains=n_chains)#, seed = seed)
ma_linear_freshwl <- stan("code/linearMA_bridgesampling.stan", 
						  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
						  data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)
ma_linear_freshwl_can <- stan("code/linearMA_bridgesampling.stan", 
							  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							  data=data_stan_freshwl_can, iter=n_iter, chains=n_chains)#, seed = seed)

save(ma_linear, file="output/ma_linear.RData")
save(ma_linear_freshwl, file="output/ma_linear_freshwl.RData")
save(ma_linear_freshwl_can, file="output/ma_linear_freshwl_can.RData")

## nonLinear model (M1c from Moeltner paper)
ma_nonlinear <- stan("code/nonlinearMA_bridgesampling.stan", 
					 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
					 data=data_stan_whole, iter=n_iter, chains=n_chains)#, seed = seed)
ma_nonlinear_freshwl <- stan("code/nonlinearMA_bridgesampling.stan", 
							 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							 data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)
ma_nonlinear_freshwl_can <- stan("code/nonlinearMA_bridgesampling.stan", 
								 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
								 data=data_stan_freshwl_can, iter=n_iter, chains=n_chains)#, seed = seed)

save(ma_nonlinear, file="output/ma_nonlinear.RData")
save(ma_nonlinear_freshwl, file="output/ma_nonlinear_freshwl.RData")
save(ma_nonlinear_freshwl_can, file="output/ma_nonlinear_freshwl_can.RData")

#Posterior Diagnostics
#Model comparison: Bayes factor
library(bridgesampling)
# compute (log) marginal likelihoods ###
set.seed(1)
bridge_lin_whole <- bridge_sampler(ma_linear)
bridge_lin_freshwl <- bridge_sampler(ma_linear_freshwl)
bridge_lin_freshwl_can <- bridge_sampler(ma_linear_freshwl_can)
bridge_nonlin_whole <- bridge_sampler(ma_nonlinear)
bridge_nonlin_freshwl <- bridge_sampler(ma_nonlinear_freshwl)
bridge_nonlin_freshwl_can <- bridge_sampler(ma_nonlinear_freshwl_can)
#compute log marginal likelihood
print(bridge_lin_whole)
print(bridge_lin_freshwl)
print(bridge_lin_freshwl_can)
print(bridge_nonlin_whole)
print(bridge_nonlin_freshwl)
print(bridge_nonlin_freshwl_can)

### compute Bayes factor ###
bf(bridge_lin_whole, bridge_nonlin_whole) #BF :1.12
bf(bridge_lin_whole, bridge_lin_freshwl) #BF : 0.00
#..Relative Mean Square Errors
summary(bridge_lin_freshwl)
summary(bridge_lin_freshwl_can)
summary(bridge_lin_whole)
summary(bridge_nonlin_whole)
summary(bridge_nonlin_freshwl)
summary(bridge_nonlin_freshwl_can)


#model comparison: Loo
library(loo)
#m <- apply(ext_fit$y_rep, 2, mean)----for obtaining predicted values
#Preparing estimated models for LOO Cross Validation checks: https://mc-stan.org/loo/articles/loo2-example.html

loo_whole_lin <- loo(ma_linear, save_psis = TRUE)
loo_wh_fresh_lin <- loo(ma_linear_freshwl, save_psis = TRUE)
loo_wh_fresh_lin_can <- loo(ma_linear_freshwl_can, save_psis = TRUE)
loo_whole_nonlin <- loo(ma_nonlinear, save_psis = TRUE)
loo_wh_fresh_nonlin <- loo(ma_nonlinear_freshwl, save_psis = TRUE)
loo_wh_fresh_nonlin_can <- loo(ma_nonlinear_freshwl_can, save_psis = TRUE)

#Comparing the models on expected log predictive density
model_com_lin_nonlin <- loo_compare(loo_whole_lin, loo_whole_nonlin) #linear model preferred based on low predictive error based on Loo CV
print(model_com_lin_nonlin, simplify = FALSE)
#Plotting Pareto k diagnostics for linear model
plot(loo_whole_lin)
plot(loo_wh_fresh_lin)
plot(loo_whole_nonlin)
plot(loo_wh_fresh_lin_can) 

#extracting predicted dependent values for the models for nonlinear models
fit_wh_fresh_lin <- extract(ma_linear_freshwl)
y_rep_wh_fresh_lin <- fit_wh_fresh_lin$y_rep

fit_fresh_can_lin <- extract(ma_linear_freshwl_can)
y_rep_fresh_can_lin <- fit_fresh_can_lin$y_rep

#Marginal posterior predictive checks_linear
whole_fresh_lin <- ppc_loo_pit_overlay(
	y = df_freshwl$lnwtp,
	yrep = y_rep_wh_fresh_lin,
	lw = weights(loo_wh_fresh_lin$psis_object)
)

#loo_wh_fresh_nonlin_can <- loo(ma_nonlinear_freshwl_can, save_psis = TRUE)

whole_fresh_lin_can <- ppc_loo_pit_overlay(
	y = df_canada_fresh$lnwtp,
	yrep = y_rep_fresh_can_lin,
	lw = weights(loo_wh_fresh_lin_can$psis_object)
)

#summary of results--Linear
#library(devtools)
#devtools::install_github("strengejacke/sjstats")
#library("sjstats")
#install.packages("tidy_stan")
#library("tidy_stan")

results_wholelin <- MCMCsummary(ma_linear, params = c("beta","gamma","sigma"), round = 2)
results_freshwholelin <- MCMCsummary(ma_linear_freshwl, params = c("beta","gamma","sigma"),round = 2)
results_ma_linear_freshwl_can <- MCMCsummary(ma_linear_freshwl_can, params = c("beta","gamma","sigma"), round = 2)

write.csv(results_wholelin, "output/results_wholelin.csv")
write.csv(results_freshwholelin, "output/results_freshwholelin.csv")
write.csv(results_ma_linear_freshwl_can, "output/results_ma_linear_freshwl_can.csv")

#summary of results--NonLinear
results_whole_onlin <- MCMCsummary(ma_nonlinear, params = c("beta","gamma","sigma"), round = 2)
results_freshwhole_nonlin <- MCMCsummary(ma_nonlinear_freshwl, params = c("beta","gamma","sigma"),round = 2)
results_ma_nonlinear_freshwl_can <- MCMCsummary(ma_nonlinear_freshwl_can, params = c("beta","gamma","sigma"), round = 2)

write.csv(results_whole_onlin, "output/results_whole_nonlin.csv")
write.csv(results_freshwhole_nonlin, "output/results_freshwhole_nonlin.csv")
write.csv(results_ma_nonlinear_freshwl_can, "output/results_ma_nonlinear_freshwl_can.csv")

#...Probability parameter > 0
load("output/ma_linear_freshwl.RData")
load("output/ma_linear_freshwl_can.RData")

mean(as.matrix(ma_linear_freshwl)[, "beta[1]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[2]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[3]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[4]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[5]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[6]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[7]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[8]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[9]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[10]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[11]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[12]"] > 0)


mean(as.matrix(ma_linear_freshwl_can)[, "beta[1]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[2]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[3]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[4]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[5]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[6]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[7]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[8]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[9]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[10]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[11]"] > 0)
mean(as.matrix(ma_linear_freshwl_can)[, "beta[12]"] > 0)

install.packages("BayesPostEst")
library("BayesPostEst")

for (i in 1:14){
	print(mean(as.matrix(ma_linear_freshwl)[, "beta[1]"]) )
	#> 0))
	#	print(i)
	i <- i + 1
}
#other post estimation diagnostics
library(bayesplot)
#mcmc_intervals(as.matrix(ma_linear_freshwl), regex_pars = "beta|gamma
# sigman")
#mcmc_areas(as.matrix(ma_linear_freshwl), regex_pars = "beta|gamma|sigma")

#diagnostic Plots
#Autocorrelation
stan_ac(ma_linear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]', 'beta[11]'))
stan_ac(ma_linear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
									   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
									   'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_trace(ma_linear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										  'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_trace(ma_linear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]', 'beta[11]'))
stan_plot(ma_linear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))
stan_plot(ma_linear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_dens(ma_linear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_dens(ma_linear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_linear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_linear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_rhat(ma_linear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_rhat(ma_linear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))

###P..........................Benefit Transfer
#a) BT Transfer Error
fit_linear_fresh <- extract(ma_linear_freshwl)
fit_linear_fresh_can <- extract(ma_linear_freshwl_can)

y_prep_linfresh <- apply(fit_linear_fresh$y_rep, 2, mean) # extract the predicted y at their means : Linear freshwater model
y_prep_linfresh_can <- apply(fit_linear_fresh_can$y_rep, 2, mean) # extract the predicted y at their means : Linear freshwater model

linfreshwater_TE <- data.frame(cbind(y_prep_linfresh, df_freshwl$lnwtp))
linfreshwater_TE <- linfreshwater_TE %>%
	mutate(wtp_y = exp(V2) - 1,
		   wtp_ypred = exp(y_prep_linfresh) - 1,
		   TE = (abs(wtp_y - wtp_ypred)/wtp_y)*100) 

linfreshwater_TE_noINF <- linfreshwater_TE[!is.infinite(rowSums(linfreshwater_TE)),]
hist(linfreshwater_TE_noINF$TE)
median(linfreshwater_TE_noINF$TE)
mean(linfreshwater_TE_noINF$TE)
min(linfreshwater_TE_noINF$TE)
max(linfreshwater_TE_noINF$TE)
sd(linfreshwater_TE_noINF$TE)


linfreshwater_can_TE <- data.frame(cbind(y_prep_linfresh_can, df_canada_fresh$lnwtp))
linfreshwater_can_TE <- linfreshwater_can_TE %>%
	mutate(wtp_y = exp(V2) - 1,
		   wtp_ypred = exp(y_prep_linfresh_can) - 1,
		   TE = (abs(wtp_y - wtp_ypred)/wtp_y)*100) 

linfreshwater_can_TE_noINF <- linfreshwater_can_TE[!is.infinite(rowSums(linfreshwater_can_TE)),]
hist(linfreshwater_can_TE_noINF$TE)
median(linfreshwater_can_TE_noINF$TE)
mean(linfreshwater_can_TE_noINF$TE)
min(linfreshwater_can_TE_noINF$TE)
max(linfreshwater_can_TE_noINF$TE)
sd(linfreshwater_can_TE_noINF$TE)


write.csv(y_prep_linfresh,"data/y_prep_linfresh.csv")
write.csv(y_prep_linfresh_can,"data/y_prep_linfresh_can.csv")
write.csv(linfreshwater_TE,"data/linfreshwater_TE.csv")
write.csv(linfreshwater_can_TE,"data/linfreshwater_can_TE.csv")

#b) Predictions
#1) Saskachewan - PHJV Landscapes

ma_linear_freshwl_sask <- stan("code/linearMA_bridgesampling_pred.stan", 
							   pars = "y_rep", init = init,
							   data=data_stan_freshwl_sask, iter=n_iter, chains=4)#, seed = seed)
predictions_sask_freshwater <- extract(ma_linear_freshwl_sask)

ma_linear_freshwl_can_sask <- stan("code/linearMA_bridgesampling_pred.stan", 
								   pars =  "y_rep", init = init,
								   data=data_stan_freshwl_can_sask, iter=n_iter, chains=n_chains)#, seed = seed)
predictions_sask_freshwater_can <- extract(ma_linear_freshwl_can_sask)