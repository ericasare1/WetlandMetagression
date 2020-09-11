
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
whole_lin <- ppc_loo_pit_overlay(
	y = data_stan_whole$lwtp,
	yrep = y_rep_wh_lin,
	lw = weights(loo_whole_lin$psis_object)
)

whole_fresh_lin <- ppc_loo_pit_overlay(
	y = data_stan_freshwl$lwtp,
	yrep = y_rep_wh_fresh_lin,
	lw = weights(loo_wh_fresh_lin$psis_object)
)

#linear
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

fresh_canada_lin <- ppc_loo_pit_overlay(
	y = df_canada_fresh1$y,
	yrep = y_rep_fresh_can_lin,
	lw = weights(loo_fresh_can_lin$psis_object)
)

whole_can_lin <- ppc_loo_pit_overlay(
	y = df_canada1$y,
	yrep = y_rep_wh_can_lin,
	lw = weights(loo_fresh_can_lin$psis_object)
)


#other post estimation diagnostics
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

#MCMC diagnostics
stan_trace(ma_linear)
#Autocorrelation
stan_ac(ma_linear)
#Raftery Diagnostics
library(LaplacesDemon)
raftery.diag(ma_linear)
#Rhat
stan_rhat(ma_linear)
#effective SS
stan_ess(ma_linear)

#Predictions
ext_fit <- extract(ma_linear)

# Accuracy
apply(ext_fit$y_rep, 2, median)
## [1] 0.75

loo(ma_linear)

beta_post <- ext_fit$beta
min(beta_post)
gamma_post <- ext_fit$gamma
sigma_post <- ext_fit$sigma

x <- data.frame(data_stan$x)
q <- data.frame(cbind(data_stan$q0, data_stan$q1))
q <- q %>% mutate(q01 = 0.5*(X1 + X2))
# Function for simulating y based on new x
gen_quant_r <- function(x, q01) {
	beta_x = as.matrix(sample(beta_post, size = length(x)))
	y_pred = as.matrix(x) %*% beta_x + as.matrix(q$q01) %*% as.matrix(sample(gamma_post, size = length(1))) 
	+ as.matrix(sample(sigma_post, size = 58))
	return(y_pred)
}
gen_quant_r(x,q$q01)

#pred_gen
pred <- stan(file = "pred_generated.stan",
			 data = list(x = x, N = nrow(df_scaled),
			 			n_col = ncol(x),
			 			N_samples = length(beta_post),
			 			gamma = gamma_post,
			 			beta = beta_post,
			 			sigma = sigma_post),
			 chains = 1, iter = 100,
			 algorithm = "Fixed_param")

# Extract and format output
ext_pred <- extract(pred)
out_mat <- matrix(NA, nrow = dim(ext_pred$y_test)[2],
				  ncol = dim(ext_pred$y_test)[3])
for(i in 1:dim(ext_pred$y_test)[2]) {
	for(j in 1:dim(ext_pred$y_test)[3]) {
		out_mat[i, j] <- mean(ext_pred$y_test[, i, j])
	}
}


ext_ract = extract(ma_linear)
#summary of results
print(ma_linear, digits_summary = 3)
#MCMC diagnostics
stan_trace(ma_linear)
#Autocorrelation
stan_ac(ma_linear)
#Raftery Diagnostics
library(LaplacesDemon)
raftery.diag(ma_linear)
#Rhat
stan_rhat(ma_linear)
#effective SS
stan_ess(ma_linear)

y_rep <- as.matrix(ma_linear, pars = "y_rep")

##Model Validation
library("tidyverse")
mcmc = as.data.frame(ma_linear) %>% dplyr:::select(starts_with("beta"),
														 sigma) %>% as.matrix

newdata = df_scaled
str(newdata)
Xmat = model.matrix(newdata)




print(ma_linear, digits_summary = 3)

print(get_elapsed_time(ma_linear))

posterior <- as.array(ma_linear)
colnames(data_stan$x)
apply(posterior, c(3), function(x){mean(x>0)})

str(rstan::extract(ma_linear))

shinystan::launch_shinystan(ma_linear)


plot_data <- extract(ma_linear)

save(ma_linear, file="output/ma_linear.RData")


# Nonlinear model (M1c from Moeltner paper)
ma_nonlinear <- stan("code/nonlinearMA.stan", 
					pars = c("beta", "sigma", "gamma"),init = init,
					data=data_stan, iter=n_iter, chains=n_chains)#, seed = seed)

 print(ma_nonlinear, digits_summary = 3)

print(get_elapsed_time(ma_nonlinear))

posterior <- as.array(ma_nonlinear)

apply(posterior, c(3), function(x){mean(x>0)})

bh_mcmc <- ma_nonlinear %>% 
	rstan::extract()



pp_rhat <- bh_mcmc[ 'y_new'] %>% 
	map_df(as_data_frame, .id = 'variable') %>% 
	gather(observation,value, -variable) %>% View()

ggplot() + 
	geom_density(data = pp_rhat, aes(log(value),fill = 'Posterior Predictive'), alpha = 0.5) + 
	geom_density(data = df$lwtp, aes(log(r), fill = 'Observed'), alpha = 0.5)




extract(ma_nonlinear)$y_new 



y_pred <- extract(ma_nonlinear, 'y_new')

y_pred <- unlist(y_new, use.names=FALSE)

save(ma_linear, file="output/ma_nonlinear.RData")





color_scheme_set("red")
mcmc_intervals(posterior, pars = c("beta[1]", "sigma", "gamma2"))

color_scheme_set("blue")
mcmc_trace(posterior)#, pars = c("wt", "sigma"))

mcmc_pairs(posterior, off_diag_args = list(size = 1.5))

mcmc_dens_overlay(posterior, pars = c("beta[1]", "sigma", "gamma2"))
