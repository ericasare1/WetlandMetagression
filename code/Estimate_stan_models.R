
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
df %>%
	mutate(lnacres = log((q1-q0)*1000)) %>%
	ggplot(aes(x=lnacres, y = lnwtp, label=canada))+
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0)

summary(df)

#Creating different datafranes
df_whole <- df
df_freshwl <- df %>% filter(wlfresh ==1)
df_canada_fresh <- df_freshwl %>% filter(canada ==1)
df_us_fresh <- df_freshwl %>% filter(canada ==0)
df_canada <- df %>% filter(canada ==1)
df_us <- df %>% filter(canada ==0)


#Linear Regression (Frequentist) estimations
lm1 <- lm(lnwtp2 ~ q01 + lnyear + lninc + us + 
		  	local + 
		  	prov + reg + cult + 
		  	forest + 
		  	volunt + lumpsum, data  = df)
summary(lm1)

lm1 <- lm(lnwtp ~ q01 + lnyear + lninc + #sagulf + nmw + 
		  	local + prov + reg + cult + forest + 
		  	volunt + lumpsum + 
		  	canada, data  = df)
summary(lm1)

lm1 <- lm(lnwtp ~ q_percent + lnyear + lninc + #sagulf + nmw + 
		  	local + prov + reg + cult + forest + canada, data  = df)
summary(lm1)

lm1 <- lm(lnwtp ~ q_change + lnyear + lninc + #sagulf + nmw + 
		  	local + prov + reg + cult + 
		  	volunt + lumpsum + 
		  	forest + canada, data  = df)
summary(lm1)


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
ma_linear <- stan("code/linearMA.stan", 
					 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
					 data=data_stan_whole, iter=n_iter, chains=n_chains)#, seed = seed)

ma_linear_freshwl <- stan("code/linearMA.stan", 
				  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
				  data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)

ma_linear_freshwl_can <- stan("code/linearMA.stan", 
						  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
						  data=data_stan_freshwl_can, iter=n_iter, chains=n_chains)#, seed = seed)

ma_linear_whole_can <- stan("code/linearMA.stan", 
							  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							  data=data_stan_wholel_can, iter=n_iter, chains=n_chains)#, seed = seed)

# nonLinear model (M1c from Moeltner paper)
ma_nonlinear <- stan("code/nonlinearMA.stan", 
				  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
				  data=data_stan_whole, iter=n_iter, chains=n_chains)#, seed = seed)

ma_nonlinear_freshwl <- stan("code/nonlinearMA.stan", 
						  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
						  data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)

ma_nonlinear_freshwl_can <- stan("code/nonlinearMA.stan", 
							  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							  data=data_stan_freshwl_can, iter=n_iter, chains=n_chains)#, seed = seed)

ma_nonlinear_whole_can <- stan("code/nonlinearMA.stan", 
							pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							data=data_stan_wholel_can, iter=n_iter, chains=n_chains)#, seed = seed)

#summary of results--Linear
#library(devtools)
#devtools::install_github("strengejacke/sjstats")
#library("sjstats")
#install.packages("tidy_stan")
#library("tidy_stan")
library("MCMCvis")
library("broom")
results_wholelin <- MCMCsummary(ma_linear, params = c("beta","gamma","sigma"), round = 2)
results_freshwholelin <- MCMCsummary(ma_linear_freshwl, params = c("beta","gamma","sigma"),round = 2)
MCMCsummary(ma_linear_freshwl_can, round = 2)
MCMCsummary(ma_linear_whole_can, round = 2)

write.csv(results_wholelin, "output/results_wholelin.csv")
write.csv(results_freshwholelin, "output/results_freshwholelin.csv")

#summary of results--NonLinear
results_whole_nonlin <- MCMCsummary(ma_nonlinear, params = c("beta","gamma","sigma"), round = 2)
results_freshwhole_nonlin <- MCMCsummary(ma_nonlinear_freshwl, params = c("beta","gamma","sigma"),round = 2)
MCMCsummary(ma_nonlinear_freshwl_can, round = 2)
MCMCsummary(ma_nonlinear_whole_can, round = 2)

write.csv(results_whole_nonlin, "output/results_whole_nonlin.csv")
write.csv(results_freshwhole_nonlin, "output/results_freshwhole_nonlin.csv")

library(loo)
#m <- apply(ext_fit$y_rep, 2, mean)----for obtaining predicted values
#Preparing estimated models for LOO Cross Validation checks: https://mc-stan.org/loo/articles/loo2-example.html
 
loo_whole_lin <- loo(ma_linear, save_psis = TRUE)
loo_wh_fresh_lin <- loo(ma_linear_freshwl, save_psis = TRUE)
loo_fresh_can_lin <- loo(ma_linear_freshwl_can, save_psis = TRUE)
loo_wh_can_lin <- loo(ma_linear_whole_can, save_psis = TRUE)

loo_whole_nonlin <- loo(ma_nonlinear, save_psis = TRUE)
loo_wh_fresh_nonlin <- loo(ma_nonlinear_freshwl, save_psis = TRUE)
loo_fresh_can_nonlin <- loo(ma_nonlinear_freshwl_can, save_psis = TRUE)
loo_wh_can_nonlin <- loo(ma_nonlinear_whole_can, save_psis = TRUE)

#Comparing the models on expected log predictive density
model_com_lin_nonlin <- loo_compare(loo_whole_lin,loo_whole_nonlin)
model_com_freshlin_freshnonlin <- loo_compare(loo_wh_fresh_lin,loo_wh_fresh_nonlin)

#linear model preferred based on low predictive error based on Loo CV

#extracting predicted dependent values for the models for linear models

fit_wh_fresh_lin <- extract(ma_linear_freshwl)
y_rep_wh_fresh_lin <- fit_wh_fresh_lin$y_rep

fit_fresh_can_lin <- extract(ma_linear_freshwl_can)
y_rep_fresh_can_lin <- fit_fresh_can_lin$y_rep

#Plotting Pareto k diagnostics for linear model
plot(loo_whole_lin)
plot(loo_wh_fresh_lin)

#Marginal posterior predictive checks_linear
#Dependent Variable in linear model for posterior check
#y = lwtp - log(q1- q0)
df1 <- df %>% mutate(y = lnwtp - log(q1-q0))
df_freshwl1 <- df1 %>% filter(wlfresh ==1)
df_canada_fresh1 <- df_freshwl1 %>% filter(canada ==1)
df_canada1 <- df1 %>% filter(canada ==1)

whole_lin <- ppc_loo_pit_overlay(
	y = df1$y, #change the dep variable
	yrep = y_rep_wh_lin,
	lw = weights(loo_whole_lin$psis_object)
)

whole_fresh_lin <- ppc_loo_pit_overlay(
	y = df_freshwl1$y,
	yrep = y_rep_wh_fresh_lin,
	lw = weights(loo_wh_fresh_lin$psis_object)
)

#other post estimation diagnostics
library(bayesplot)
mcmc_intervals(as.matrix(ma_linear_freshwl), regex_pars = "beta|sigma")
mcmc_areas(as.matrix(ma_linear_freshwl), regex_pars = "beta|sigma")

MCMCtrace(ma_linear, 
		  params = c('beta\\[1\\]', 'beta\\[2\\]', 'beta\\[3\\]'),
		  ISB = FALSE,
		  pdf = FALSE,
		  Rhat = TRUE,
		  n.eff = TRUE)

MCMCplot(ma_nonlinear, 
		 params = 'beta', 
		 # rank = TRUE,
		 xlab = 'ESTIMATE',
		 guide_lines = TRUE)

MCMCplot(MCMC_data, 
		 params = 'beta', 
		 xlab = 'ESTIMATE',
		 main = 'MCMCvis plot',
		 #labels = c('First param', 'Second param', 'Third param', 
		 #'Fourth param', 'Fifth param', 'Sixth param'), 
		 #col = c('red', 'blue', 'green', 'purple', 'orange', 'black'),
		 guide_lines = TRUE,
		 sz_labels = 1.5,
		 sz_med = 2,
		 sz_thick = 7,
		 sz_thin = 3,
		 sz_ax = 4,
		 sz_main_txt = 2)

#Autocorrelation
stan_ac(ma_linear)


shinystan::launch_shinystan(ma_linear)




save(ma_linear, file="output/ma_linear.RData")
save(ma_linear_freshwl, file="output/ma_linear_freshwl.RData")



library("broom.mixed")
library("broom")
library("coda")
load("output/ma_linear_freshwl.RData")

final_results_wholefresh <- tidyMCMC(ma_linear_freshwl, conf.int = TRUE, conf.method = "HPDinterval",
								   pars = c( "beta", "gamma", "sigma"))

#Bayesian p-values that are somewhat analogous to the frequentist p-values for investigating the hypothesis that a parameter is equal to zero.: share of posterior density to the right of zero

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
mean(as.matrix(ma_linear_freshwl)[, "beta[13]"] > 0)
mean(as.matrix(ma_linear_freshwl)[, "beta[14]"] > 0)

install.packages("BayesPostEst")
library("BayesPostEst")
	
for (i in 1:14){
		print(mean(as.matrix(ma_linear_freshwl)[, "beta[1]"]) )
			  #> 0))
	#	print(i)
		i <- i + 1
}
		
	