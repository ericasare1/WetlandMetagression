
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, rstan, shinystan, bayesplot, bridgesampling)

# Import data
#-----------------------------------------------

df <- read.csv("data/finalData_24_8.csv")
df %>% view()

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
		   q1 = q1/1000) %>% view()


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

n_ind <- n_distinct(df$studyid)

data_stan <- list(N=nrow(df),
				  S = n_ind,
				  lwtp=df$lnwtp,
#				  x=as.matrix(df[,6]),
				  x=cbind(rep(1,nrow(df)),as.matrix(df[, 4:6]),
				  		as.matrix(df[, 10:14]),
				  		as.matrix(df[, 17:18]), 
						as.matrix(df[, 23])), # add for canada dummy
				 q0 = df$q0,
				 q1 = df$q1)

data_stan$K <- ncol(data_stan$x)
ncol(df)

init <- list(gamma = 0.08,
		#	 beta = c(-.5, 0, .2, -0.4, -0.7, 3.1, -2.2, 1.6, -.3, 1.1, -0.02, 1.5),
			 sigma = .5)

init <- list(init = init,
			 init = init,
			 init = init,
			 init = init)

df <- df %>%
	mutate(q01 = (q1 + q0)/2,
		   q_percent = ifelse(q0 == 0, 100, q1 / q0 -1), 
		   lnwtp2 = lnwtp - log(q1- q0),
		   q_change = q1 - q0,
		   us = ifelse(canada == 1, 0, 1))

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

# Linear model (M3c from Moeltner paper)
ma_linear <- stan("code/linearMA.stan", 
					 pars = c("beta", "sigma", "gamma"),init = init,
					 data=data_stan, iter=n_iter, chains=n_chains)#, seed = seed)

print(ma_linear, digits_summary = 3)

print(get_elapsed_time(ma_linear))

posterior <- as.array(ma_linear)
colnames(data_stan$x)
apply(posterior, c(3), function(x){mean(x>0)})

save(ma_linear, file="output/ma_linear.RData")


# Nonlinear model (M1c from Moeltner paper)
ma_nonlinear <- stan("code/nonlinearMA.stan", 
					pars = c("beta", "sigma", "gamma"),init = init,
					data=data_stan, iter=n_iter, chains=n_chains)#, seed = seed)

print(ma_nonlinear, digits_summary = 3)

print(get_elapsed_time(ma_nonlinear))

posterior <- as.array(ma_nonlinear)

apply(posterior, c(3), function(x){mean(x>0)})

save(ma_linear, file="output/ma_nonlinear.RData")





color_scheme_set("red")
mcmc_intervals(posterior, pars = c("beta[1]", "sigma", "gamma2"))

color_scheme_set("blue")
mcmc_trace(posterior)#, pars = c("wt", "sigma"))

mcmc_pairs(posterior, off_diag_args = list(size = 1.5))

mcmc_dens_overlay(posterior, pars = c("beta[1]", "sigma", "gamma2"))
