
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
	ggplot(aes(x=lnacres, y = lnwtp, label=canada)) +
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0) + theme_bw() +
	labs(x = "Log(Acres)", y = "log(WTP)")


summary(df)

#Graph of lnwtp vrs lnacres for freshwater data
df %>%
	mutate(lnacres = log((q1-q0)*1000)) %>%
	filter(wlfresh == 1) %>%
	ggplot(aes(x=lnacres, y = lnwtp, label=canada))+
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0) + bw()
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
ma_linear <- stan("code/linearMA_bridgesampling_us.stan", 
				  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"), #init = init,
				  data=data_stan_whole, iter=n_iter, chains=n_chains)#, seed = seed)
ma_linear_freshwl <- stan("code/linearMA_bridgesampling.stan", 
						  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
						  data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)
ma_linear_freshwl_can <- stan("code/linearMA_bridgesampling.stan", 
							  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							  data=data_stan_freshwl_can, iter=n_iter, chains=n_chains)#, seed = seed)
ma_linear_freshwl_us <- stan("code/linearMA_bridgesampling.stan", 
							  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							  data=data_stan_freshwl_us, iter=n_iter, chains=n_chains)#, seed = seed)

save(ma_linear, file="output/ma_linear.RData")
save(ma_linear_freshwl, file="output/ma_linear_freshwl.RData")
save(ma_linear_freshwl_can, file="output/ma_linear_freshwl_can.RData")
save(ma_linear_freshwl_us, file="output/ma_linear_freshwl_us.RData")

## nonLinear model (M1c from Moeltner paper)
ma_nonlinear <- stan("code/nonlinearMA_bridgesampling.stan", 
					 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
					 data=data_stan_whole, iter=n_iter, chains=n_chains)#, seed = seed)
ma_nonlinear_freshwl <- stan("code/nonlinearMA_bridgesampling.stan", 
							 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							 data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)
print(ma_nonlinear_freshwl_can)
ma_nonlinear_freshwl_can <- stan("code/nonlinearMA_bridgesampling_can.stan", 
								 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"), #init = init,
								 data=data_stan_freshwl_can, iter=100, chains=1)#, seed = seed)

ma_nonlinear_freshwl_us <- stan("code/nonlinearMA_bridgesampling.stan", 
								 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
								 data=data_stan_freshwl_us, iter=n_iter, chains=n_chains)#, seed = seed)

save(ma_nonlinear, file="output/ma_nonlinear.RData")
save(ma_nonlinear_freshwl, file="output/ma_nonlinear_freshwl.RData")
save(ma_nonlinear_freshwl_can, file="output/ma_nonlinear_freshwl_can.RData")
save(ma_nonlinear_freshwl_us, file="output/ma_nonlinear_freshwl_us.RData")


#...................................Posterior Diagnostics....................
#A. Model comparison: Bayes factor
library(bridgesampling)
# compute (log) marginal likelihoods ###
set.seed(1)
bridge_lin_whole <- bridge_sampler(ma_linear) #LogML: -166.11
bridge_lin_freshwl <- bridge_sampler(ma_linear_freshwl)#LogML: -111.01
bridge_lin_freshwl_can <- bridge_sampler(ma_linear_freshwl_can)
bridge_lin_freshwl_us <- bridge_sampler(ma_linear_freshwl_us) #LogML: -59.86

bridge_nonlin_whole <- bridge_sampler(ma_nonlinear)
bridge_nonlin_freshwl <- bridge_sampler(ma_nonlinear_freshwl)
bridge_nonlin_freshwl_can <- bridge_sampler(ma_nonlinear_freshwl_can)
bridge_nonlin_freshwl_us <- bridge_sampler(ma_nonlinear_freshwl_us)

#compute log marginal likelihood
print(bridge_nonlin_whole)
print(bridge_nonlin_freshwl)
print(bridge_nonlin_freshwl_can)
print(bridge_nonlin_freshwl_us)

### compute Bayes factor ###
bf(bridge_lin_whole, bridge_nonlin_whole) #BF :1.11
#..Relative Mean Square Errors
summary(bridge_lin_whole) #rmse : 6.2e-05
summary(bridge_nonlin_whole) #rmse 6.0e-05
summary(bridge_lin_freshwl) #rmse 5.79e-05
summary(bridge_nonlin_freshwl) #rmse 5.10e-05

summary(bridge_nonlin_freshwl)
summary(bridge_nonlin_freshwl_can)
summary(bridge_nonlin_freshwl_us)

# B. model comparison: Loo
library(loo)
#Preparing estimated models for LOO Cross Validation checks: https://mc-stan.org/loo/articles/loo2-example.html

loo_whole_lin <- loo(ma_linear, save_psis = TRUE)
loo_wh_fresh_lin <- loo(ma_linear_freshwl, save_psis = TRUE)

loo_whole_nonlin <- loo(ma_nonlinear, save_psis = TRUE)
loo_wh_fresh_nonlin <- loo(ma_nonlinear_freshwl, save_psis = TRUE)


#Comparing the models on expected log predictive density
model_com_lin_nonlin <- loo_compare(loo_whole_lin, loo_whole_nonlin) #nonlinear model preferred based on low predictive error based on Loo CV

print(model_com_lin_nonlin, simplify = FALSE)

#............Marginal posterior predictive checks
#1) Extracting predicted dependent values for the models for nonlinear models
fit_wh_fresh_nonlin <- extract(ma_nonlinear_freshwl)
y_rep_wh_fresh_nonlin <- fit_wh_fresh_nonlin$y_rep

fit_fresh_us_nonlin <- extract(ma_nonlinear_freshwl_us)
y_rep_fresh_us_nonlin <- fit_fresh_us_nonlin$y_rep

fit_fresh_can_nonlin <- extract(ma_nonlinear_freshwl_can)
y_rep_fresh_can_nonlin <- fit_fresh_can_nonlin$y_rep

#2. Marginal posterior predictive

whole_fresh_nonlin <- ppc_loo_pit_overlay(
	y = df_freshwl$lnwtp,
	yrep = y_rep_wh_fresh_nonlin,
	lw = weights(loo_wh_fresh_nonlin$psis_object)
)


whole_fresh_nonlin_us <- ppc_loo_pit_overlay(
	y = df_us_fresh$lnwtp,
	yrep = y_rep_fresh_us_nonlin,
	lw = weights(loo_wh_fresh_nonlin_us$psis_object)
)
#summary of results--Linear
#library(devtools)
#devtools::install_github("strengejacke/sjstats")
#library("sjstats")
#install.packages("tidy_stan")
#library("tidy_stan")
library("MCMCvis")
results_wholelin <- MCMCsummary(ma_linear, params = c("beta","gamma","sigma"), round = 2)
results_freshwholelin <- MCMCsummary(ma_linear_freshwl, params = c("beta","gamma","sigma"),round = 2)
results_wholenonlin <- MCMCsummary(ma_nonlinear, params = c("beta","gamma","sigma"), round = 2)
results_freshwholenonlin <- MCMCsummary(ma_nonlinear_freshwl, params = c("beta","gamma","sigma"),round = 2)
results_ma_nonlinear_freshwl_can <- MCMCsummary(ma_nonlinear_freshwl_can, params = c("beta","gamma","sigma"), round = 2)
results_ma_nonlinear_freshwl_us <- MCMCsummary(ma_nonlinear_freshwl_us, params = c("beta","gamma","sigma"), round = 2)


write.csv(results_wholelin, "output/results_wholelin.csv")
write.csv(results_freshwholelin, "output/results_freshwholelin.csv")
write.csv(results_wholenonlin, "output/results_wholenonlin.csv")
write.csv(results_freshwholenonlin, "output/results_freshwholenonlin.csv")
write.csv(results_ma_nonlinear_freshwl_can, "output/results_ma_nonlinear_freshwl_can.csv")
write.csv(results_ma_nonlinear_freshwl_us, "output/results_ma_nonlinear_freshwl_cus.csv")

#...Probability parameter > 0
load("output/ma_linear_freshwl.RData")
load("output/ma_linear_freshwl_can.RData")

mean(as.matrix(ma_nonlinear_freshwl)[, "beta[1]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[2]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[3]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[4]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[5]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[6]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[7]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[8]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[9]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[10]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[11]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl)[, "beta[12]"] > 0)


mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[1]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[2]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[3]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[4]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[5]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[6]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[7]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[8]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[9]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[10]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_can)[, "beta[11]"] > 0)

mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[1]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[2]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[3]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[4]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[5]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[6]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[7]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[8]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[9]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[10]"] > 0)
mean(as.matrix(ma_nonlinear_freshwl_us)[, "beta[11]"] > 0)

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
stan_ac(ma_nonlinear_freshwl_us, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]', 'beta[11]'))
stan_ac(ma_nonlinear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]', 'beta[11]'))
stan_ac(ma_nonlinear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
									   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
									   'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_trace(ma_nonlinear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										  'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_trace(ma_nonlinear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]', 'beta[11]'))
stan_trace(ma_nonlinear_freshwl_us, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]', 'beta[11]'))
tan_dens(ma_nonlinear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_dens(ma_nonlinear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))
stan_dens(ma_nonlinear_freshwl_us, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_nonlinear_freshwl_us, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_nonlinear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_nonlinear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_rhat(ma_nonlinear_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_rhat(ma_nonlinear_freshwl_can, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))
stan_rhat(ma_nonlinear_freshwl_us, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]', 'beta[11]'))
###P..........................Benefit Transfer

 #a) BT Transfer Error
fit_nonlinear_fresh <- extract(ma_nonlinear_freshwl)
fit_nonlinear_fresh_us <- extract(ma_nonlinear_freshwl_us)
fit_nonlinear_fresh_can <- extract(ma_nonlinear_freshwl_can)

y_prep_nonlinfresh <- apply(fit_nonlinear_fresh$y_rep, 2, mean) # extract the predicted y : Linear freshwater model
y_prep_nonlinfresh_us <- apply(fit_nonlinear_fresh_us$y_rep, 2, mean) # extract the predicted y: Linear freshwater model
y_prep_nonlinfresh_can <- apply(fit_nonlinear_fresh_can$y_rep, 2, mean) # extract the predicted y: Linear freshwater model

#nonlinear freshwater Meta-function
nonlinfreshwater_TE <- data.frame(y_prep_nonlinfresh, df_canada_fresh$lnwtp)

nonlinfreshwater_TE <- nonlinfreshwater_TE %>%
	mutate(wtp_y = exp(df_canada_fresh.lnwtp) - 1,
		   wtp_ypred = exp(y_prep_nonlinfresh) - 1,
		   TE = (abs(wtp_y - wtp_ypred)/wtp_y)*100)

nonlinfreshwater_TE %>% 
	ggplot(aes(x=df_canada_fresh.lnwtp, y = y_prep_nonlinfresh)) +
	geom_point() +
	theme_bw() +
	geom_line(aes(x=df_canada_fresh.lnwtp, y = df_canada_fresh.lnwtp)) + 
	labs(x = "Log(WTP)", y = "log(Predicted WTP)")

hist(linfreshwater_TE$TE)
median(linfreshwater_TE$TE)
mean(linfreshwater_TE$TE)

write_csv(nonlinfreshwater_TE, "data/nonlinfreshwater_TE.csv")

#nonlinear Freshwater US Meta Function
nonlinfreshwater_us_TE <- data.frame(cbind(y_prep_nonlinfresh_us, df_canada_fresh$lnwtp))
nonlinfreshwater_us_TE <- nonlinfreshwater_us_TE %>%
	mutate(wtp_y = exp(V2) - 1,
		   wtp_ypred = exp(y_prep_nonlinfresh_us) - 1,
		   TE = (abs(wtp_y - wtp_ypred)/wtp_y)*100) 

nonlinfreshwater_us_TE %>% 
	ggplot(aes(x=V2 , y = y_prep_nonlinfresh)) +
	geom_point() +
	theme_bw() +
	geom_line(aes(x=V2, y = V2)) +
    labs(x = "Log(WTP)", y = "log(Predicted WTP)")

hist(linfreshwater_us_TE$TE)
median(linfreshwater_us_TE$TE)
mean(linfreshwater_us_TE$TE)

write_csv(nonlinfreshwater_us_TE, "data/nonlinfreshwater_us_TE.csv")


# NonLinear Freshwater Canada Meta Function
nonlinfreshwater_can_TE <- data.frame(cbind(y_prep_nonlinfresh_can, df_canada_fresh$lnwtp))
nonlinfreshwater_can_TE <- nonlinfreshwater_can_TE %>%
	mutate(wtp_y = exp(V2) - 1,
		   wtp_ypred = exp(y_prep_nonlinfresh_can) - 1,
		   TE = (abs(wtp_y - wtp_ypred)/wtp_y)*100) 

nonlinfreshwater_can_TE %>% 
	ggplot(aes(x=V2 , y = y_prep_nonlinfresh_can)) +
	geom_point() +
	theme_bw() +
	geom_line(aes(x=V2, y = V2)) +
	labs(x = "Log(WTP)", y = "log(Predicted WTP)")

hist(nonlinfreshwater_can_TE$TE)
median(nonlinfreshwater_can_TE$TE)
sd(nonlinfreshwater_can_TE$TE)

write_csv(nonlinfreshwater_can_TE, "data/nonlinfreshwater_can_TE.csv")

#b) Predictions
#1) Saskachewan - PHJV Landscapes
ma_nonlinear_freshwl_can_sk <-  stan("code/nonlinearMA_bridgesampling_sk.stan", 
										 pars = c( "y_rep"), init = init,
										 data=data_stan_freshwl_can_sask, iter=n_iter, chains=4)#, seed = seed) the gamma since it is approximately 0 and causing division by 0 problem in the predictions
print(ma_nonlinear_freshwl_can_sk)

fit_nonlinear_fresh_can_sk <- extract(ma_nonlinear_freshwl_can_sk)
y_prep_nonlinfresh_sk <- apply(fit_nonlinear_fresh_can_sk$y_rep, 2, mean) # extract the predicted y : Linear freshwater model

#x_saskq1q0 <- read_csv("data/phjv_sask_q0q1.csv")
#x_saskq1q0 <- x_saskq1q0 %>% select(-starts_with("X")) %>% na.omit()

nonlinfreshwater_can_sk <- data.frame(y_prep_nonlinfresh_sk)
nonlinfreshwater_can_sk <- nonlinfreshwater_can_sk %>%
	mutate(wtp_ypred = exp(y_prep_nonlinfresh_sk) - 1) %>%
	cbind.data.frame(x_saskq1q0)

write_csv(nonlinfreshwater_can_sk, "data/sask_phjv_predictions.csv")
sask_phjv_predictions <- read_csv("data/sask_phjv_predictions.csv")

nonlinfreshwater_can_sk %>% 
	ggplot(aes(x= log(q1) , y = wtp_ypred)) +
	geom_point() +
	theme_bw() +
	labs(x = "Estimated Restoration Wetland Acres", y = "WTP (CAN$ 2017)")
max(nonlinfreshwater_can_sk$wtp_ypred)

sask_phjv_predictions %>%
	ggplot(aes(x= PHJV, y = wtp_ypred)) +
	geom_bar(stat="identity") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "PHJV Landscape", y = "WTP (CAN$ 2017)")

hist(nlinfreshwater_can_TE$TE)
median(linfreshwater_can_TE$TE)
sd(linfreshwater_can_TE$TE)

write_csv(linfreshwater_can_TE, "data/linfreshwater_can_TE.csv")



#mean(as.numeric(unlist((y_pred_sak["y_rep.1"]))))
sask_predictions <- read_csv("data/fresh_can_sask_predictions.csv")

sask_predictions %>% mutate(lnacres = log(q1)) %>%
	ggplot(aes(x= q1, y = wtp, label = PHJV )) +
	geom_point() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	theme_bw() +
	geom_text(check_overlap = TRUE, hjust=0.5, vjust=0, size = 0.1) +
	labs(x = "Estimated Restoration Wetland Acres", y = "WTP (CAN$ 2017)")

sask_predictions %>%
	ggplot(aes(x= PHJV, y = wtp)) +
	geom_bar(stat="identity") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "PHJV Landscape", y = "WTP (CAN$ 2017)")

