
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}
# Load Packages
p_load(tidyverse, rstan, shinystan, bayesplot, bridgesampling, loo)

# Import data
#-----------------------------------------------
df <- read_csv("data/fina_data_10_20.csv")
df %>% View()
summary(df)

df <- df %>%
	filter(wlfresh == 1) %>%
	mutate(q0 = q0/1000,
		   q1 = q1/1000)

df <- df %>%
	mutate(q01 = (q1 + q0)/2,
		   q_percent = ifelse(q0 == 0, 100, q1 / q0 -1), 
		   lnwtp2 = lnwtp - log(q1- q0),
		   q_change = q1 - q0,
		   us = ifelse(canada == 1, 0, 1))

#Summary statistics for key variables for US and Canada 
df %>%
	group_by(canada) %>%
	summarise(count = n(),
			  wtp = mean(exp(lnwtp)-1),
			  acre = mean(q1-q0)*1000,
			  wtp_acre = wtp/acre)

#Graph of lnacres vrs lnwtp for freshwater wetlands
lnacres_lnwtp <- df %>%
	mutate(lnacres = log((q1-q0)*1000)) %>%
	ggplot(aes(x=lnacres, y = lnwtp, label=canada)) +
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0) + theme_bw() +
	labs(x = "Log(Acres)", y = "log(WTP)")

ggsave("output/graphs/logacres_vrs_logwtp_freshwater.png")

#Bayesian models
#a)....Checking for multicollinearity with Variance Inflation Factors
library(car)
lm1 <- lm(lnwtp ~ q01 + lnyear + lninc + us + 
		  	local + 
		  	prov + reg + cult + 
		  	forest + #q0 + q1 +
		  	volunt + lumpsum + ce + nrev, data  = df)
car::vif(lm1) 
#b) ...Outliers...
boxplot(df$lnwtp)

#...............Setting up Bayesian Model estimation environment
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
				  data=data_stan_freshwl, iter=n_iter, chains=n_chains)
ma_linear_us_freshwl <- stan("code/linearMA_bridgesampling.stan", 
						  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
						  data=data_stan_us_freshwl, iter=n_iter, chains=n_chains)
ma_linear_freshwl_can <- stan("code/linearMA_bridgesampling.stan", 
							  pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							  data=data_stan_can_freshwl, iter=n_iter, chains=n_chains)

save(ma_linear, file="output/results/ma_linear.RData")
save(ma_linear_us_freshwl, file="output/results/ma_linear_us_freshwl.RData")
save(ma_linear_freshwl_can, file="output/results/ma_linear_freshwl_can.RData")

## nonLinear model (M1c from Moeltner paper)
ma_nonlinear <- stan("code/nonlinearMA_bridgesampling.stan", 
					 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
					 data=data_stan_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)
ma_nonlinear_us_freshwl <- stan("code/nonlinearMA_bridgesampling.stan", 
							 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
							 data=data_stan_us_freshwl, iter=n_iter, chains=n_chains)#, seed = seed)
ma_nonlinear_can_freshwl <- stan("code/nonlinearMA_bridgesampling.stan", 
								pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),init = init,
								data=data_stan_can_freshwl, iter=n_iter, chains=n_chains)

save(ma_nonlinear, file="output/results/ma_nonlinear.RData")
save(ma_nonlinear_us_freshwl, file="output/results/ma_nonlinear_us_freshwl.RData")
save(ma_nonlinear_can_freshwl, file="output/results/ma_nonlinear_can_freshwl.RData")

#...................................Posterior Diagnostics....................
#A. Model Comparison: Bayes Factor, Marginal loglikelihood and RMSE
#library(bridgesampling)
set.seed(1)
bridge_lin_whole <- bridge_sampler(ma_linear) 
bridge_lin_us_freshwl <- bridge_sampler(ma_linear_us_freshwl)
bridge_lin_freshwl_can <- bridge_sampler(ma_linear_freshwl_can)

bridge_nonlin_whole <- bridge_sampler(ma_nonlinear)
bridge_nonlin_us_freshwl <- bridge_sampler(ma_nonlinear_us_freshwl)
bridge_nonlin_freshwl_can <- bridge_sampler(ma_nonlinear_can_freshwl)

#compute Bayes factor
bf(bridge_lin_whole, bridge_nonlin_whole) 

#Marginal loglikelihood
print(bridge_lin_whole)
print(bridge_lin_us_freshwl)
print(bridge_lin_freshwl_can)

print(bridge_nonlin_whole)
print(bridge_nonlin_us_freshwl)
print(bridge_nonlin_freshwl_can)

#Relative Mean Square Errors
summary(bridge_lin_whole)
summary(bridge_lin_us_freshwl)
summary(bridge_lin_freshwl_can)

summary(bridge_nonlin_whole)
summary(bridge_nonlin_us_freshwl)
summary(bridge_nonlin_freshwl_can)

# B. Model comparison: Loo cross validation
#library(loo)
#Preparing estimated models for LOO Cross Validation checks: https://mc-stan.org/loo/articles/loo2-example.html

loo_whole_lin <- loo(ma_linear, save_psis = TRUE)
loo_wh_us_fresh_lin <- loo(ma_linear_us_freshwl, save_psis = TRUE)
loo_wh_can_fresh_lin <- loo(ma_linear_freshwl_can, save_psis = TRUE)

loo_whole_nonlin <- loo(ma_nonlinear, save_psis = TRUE)
loo_wh_us_fresh_nonlin <- loo(ma_nonlinear_us_freshwl, save_psis = TRUE)
loo_wh_can_fresh_nonlin <- loo(ma_nonlinear_can_freshwl, save_psis = TRUE)

#Comparing the models on expected log predictive density
model_com_lin_nonlin <- loo_compare(loo_whole_lin, loo_whole_nonlin) 
print(model_com_lin_nonlin, simplify = FALSE) 

#Summary of rEstimated Results
library("MCMCvis")
results_freshlin <- MCMCsummary(ma_linear, params = c("beta","gamma","sigma"), round = 2)
results_us_freshlin <- MCMCsummary(ma_linear_us_freshwl, params = c("beta","gamma","sigma"),round = 2)
results_can_freshlin <- MCMCsummary(ma_linear_freshwl_can, params = c("beta","gamma","sigma"), round = 2)
results_freshnonlin <- MCMCsummary(ma_nonlinear, params = c("beta","gamma","sigma"), round = 2)
results_us_freshnonlin <- MCMCsummary(ma_nonlinear_us_freshwl, params = c("beta","gamma","sigma"),round = 2)
results_can_freshnonlin <- MCMCsummary(ma_nonlinear_can_freshwl, params = c("beta","gamma","sigma"), round = 2)

write.csv(results_freshlin, "output/tables/results_freshlin_10_20.csv")
write.csv(results_us_freshlin, "output/tables/results_us_freshlin_10_20.csv")
write.csv(results_can_freshlin, "output/tables/results_can_freshlin_10_20.csv")
write.csv(results_freshnonlin, "output/tables/results_freshnonlin_10_20.csv")
write.csv(results_us_freshnonlin, "output/tables/results_us_freshnonlin_10_20.csv")
write.csv(results_can_freshnonlin, "output/tables/results_can_freshnonlin_10_20.csv")

#...Probability parameter > 0
load("output/results/ma_linear_freshwl.RData")
load("output/results/ma_linear_freshwl_can.RData")

#Whole Freshwater Data (US and Canada)
mean(as.matrix(ma_nonlinear)[, "beta[1]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[2]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[3]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[4]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[5]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[6]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[7]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[8]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[9]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[10]"] > 0)
mean(as.matrix(ma_nonlinear)[, "beta[11]"] > 0)

#US only model
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[1]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[2]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[3]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[4]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[5]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[6]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[7]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[8]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[9]"] > 0)
mean(as.matrix(ma_nonlinear_us_freshwl)[, "beta[10]"] > 0)

#Canada Only Model
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[1]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[2]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[3]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[4]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[5]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[6]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[7]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[8]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[9]"] > 0)
mean(as.matrix(ma_nonlinear_can_freshwl)[, "beta[10]"] > 0)

#Post estimation diagnostics
#library(bayesplot)

#Autocorrelation 
ma_nonlinear_autocor <- stan_ac(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]', 'beta[11]'))
ma_nonlinear_us_autocor <- stan_ac(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]'))
ma_nonlinear_cab_autocor <- stan_ac(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
									   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
									   'beta[9]', 'beta[10]'))
#Trace
stan_trace(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										  'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_trace(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]'))
stan_trace(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]'))
# Effective Sample Size
stan_ess(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]'))
stan_ess(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										'beta[9]', 'beta[10]', 'beta[11]'))
#Rhat
stan_rhat(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]'))
stan_rhat(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]'))
stan_rhat(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]'))
### Transfer Error; TE_MA = Meta-function Transfer Error, TE_BT = Benefit Transfer Error (predicted wtp = mean of original wtp)

fit_nonlinear_fresh <- extract(ma_nonlinear)
fit_nonlinear_fresh_us <- extract(ma_nonlinear_us_freshwl)
fit_nonlinear_fresh_can <- extract(ma_nonlinear_can_freshwl)

#extracting the predicted y (y_rep at the mean of posterior distribution)
y_prep_nonlinfresh <- apply(fit_nonlinear_fresh$y_rep, 2, mean) 
y_prep_nonlinfresh_us <- apply(fit_nonlinear_fresh_us$y_rep, 2, mean) 
y_prep_nonlinfresh_can <- apply(fit_nonlinear_fresh_can$y_rep, 2, mean) 

# TE - Nonlinear 
nonlinfreshwater_TE <- data.frame(y_prep_nonlinfresh, df_can_freshwl$lnwtp)
	
all_g <- nonlinfreshwater_TE %>%
	ggplot(aes(x=df_can_freshwl.lnwtp, y = y_prep_nonlinfresh)) +
	geom_point() +
	geom_line(aes(x=df_can_freshwl.lnwtp, y = df_can_freshwl.lnwtp)) + 
	theme_bw() +
	labs(x = "Log(WTP)", y = "log(Predicted WTP)")
ggsave("output/graphs/Nonlin_lnwtppred_vrs_lnwtp.png")

TransferErrors_nonlin <- nonlinfreshwater_TE %>%	
		mutate(wtp_y = exp(df_can_freshwl.lnwtp) - 1,
		   wtp_ypred = exp(y_prep_nonlinfresh) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))

#Distribution of TE_MA and TE_BT
boxplot(TransferErrors_nonlin$TE_MA)
boxplot(TransferErrors_nonlin$TE_BT)

#Choice median to find central locations of TE because of extreme outliears 
SummaryTE_nonlin <- TransferErrors_nonlin %>% 
	select(TE_MA, TE_BT) %>% 
	summarise_all(list("median", "sd"))

write_csv(TransferErrors_nonlin, "output/tables/TransferErrors_nonlin_10_20.csv")


#nonlinear Freshwater US Meta Function
nonlinfreshwater_us_TE <- data.frame(cbind(y_prep_nonlinfresh_us, df_can_freshwl$lnwtp)) %>% rename(lnwtp_y = V2, lnwtp_pred = y_prep_nonlinfresh_us)

us_g <- nonlinfreshwater_us_TE %>% 
	ggplot(aes(x= lnwtp_y, y = lnwtp_pred)) +
	geom_point() +
	geom_line(aes(x=lnwtp_y, y = lnwtp_y)) + 
	theme_bw() +
	labs(x = "Log(WTP)", y = "log(Predicted WTP)")
ggsave("output/graphs/Us_Nonlin_lnwtppred_vrs_lnwtp.png")


TransferErrors_nonlin_us <- nonlinfreshwater_us_TE %>%
	mutate(wtp_y = exp(lnwtp_y) - 1,
		   wtp_ypred = exp(lnwtp_pred) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))

#Distribution of TE_MA and TE_BT
boxplot(TransferErrors_nonlin_us$TE_MA)
boxplot(TransferErrors_nonlin_us$TE_BT)

#Choice median to find central locations of TE because of extreme outliears 
SummaryTE_nonlin_us <- TransferErrors_nonlin_us %>% 
	select(TE_MA, TE_BT) %>% 
	summarise_all(list("median", "sd"))

write_csv(TransferErrors_nonlin_us, "output/tables/TransferErrors_nonlin_us_10_20.csv")

# NonLinear Freshwater Canada Meta Function
nonlinfreshwater_can_TE <- data.frame(cbind(y_prep_nonlinfresh_can, df_can_freshwl$lnwtp)) %>% rename(lnwtp_y = V2, lnwtp_pred = y_prep_nonlinfresh_can)

can_g <- nonlinfreshwater_can_TE %>% 
	ggplot(aes(x= lnwtp_y, y = lnwtp_pred)) +
	geom_point() +
	geom_line(aes(x=lnwtp_y, y = lnwtp_y)) + 
	theme_bw() +
	labs(x = "Log(WTP)", y = "log(Predicted WTP)")
ggsave("output/graphs/Can_Nonlin_lnwtppred_vrs_lnwtp.png")


TransferErrors_nonlin_can <- nonlinfreshwater_can_TE %>%
	mutate(wtp_y = exp(lnwtp_y) - 1,
		   wtp_ypred = exp(lnwtp_pred) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))

#Distribution of TE_MA and TE_BT
boxplot(TransferErrors_nonlin_can$TE_MA)
boxplot(TransferErrors_nonlin_can$TE_BT)

#Choice median to find central locations of TE because of extreme outliears 
SummaryTE_nonlin_can <- TransferErrors_nonlin_can %>% 
	select(TE_MA, TE_BT) %>% 
	summarise_all(list("median", "sd"))

write_csv(TransferErrors_nonlin_can, "output/tables/TransferErrors_nonlin_can_10_20.csv")

#gridExtra::grid.arrange(all_g, us_g, can_g, nrow = 2)
#b) Policy Application
#1) Saskachewan - PHJV Landscapes
phjv_cocations <- read_csv("data/phjv_locations.csv")

ma_nonlinear_freshwl_can_sk <-  stan("code/nonlinearMA_bridgesampling_sk.stan", 
										 pars = c( "y_rep"), init = init,
										 data=data_stan_can_freshwl_sask, iter=n_iter, chains=4)
print(ma_nonlinear_freshwl_can_sk)

#extracting estimated parameters
fit_nonlinear_fresh_can_sk <- extract(ma_nonlinear_freshwl_can_sk)
y_prep_nonlinfresh_sk <- apply(fit_nonlinear_fresh_can_sk$y_rep, 2, mean) #extracting predicted lnwtp at means

nonlinfreshwater_can_sk <- data.frame(y_prep_nonlinfresh_sk, phjv_cocations, x_saskq1q0) %>% 
	rename (lnwtp_pred = y_prep_nonlinfresh_sk) %>%
	mutate(wtp_ypred = exp(lnwtp_pred) - 1,
		   lnacres = log((q1-q0)*1000)) 

write_csv(nonlinfreshwater_can_sk, "output/tables/sask_phjv_wtp_predictions_ca_10_20.csv")

nonlinfreshwater_can_sk %>% 
	ggplot(aes(x= log(q1) , y = wtp_ypred)) +
	geom_point() +
	theme_bw() +
	labs(x = "Estimated Restoration Wetland Acres", y = "WTP (CAN$ 2020)")
ggsave("output/graphs/Restorationacres_vrs_wtp_pred.png")

nonlinfreshwater_can_sk %>%
	ggplot(aes(x= PHJV, y = wtp_ypred)) +
	geom_bar(stat="identity") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "PHJV Landscape", y = "WTP (CAN$ 2020)")
ggsave("output/graphs/wtppred_across_phvjlocations.png")

median(nonlinfreshwater_can_sk$wtp_ypred)
