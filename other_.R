rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, brms, shinystan, bayesplot, bridgesampling)
# Import data
#-----------------------------------------------

df <- read_csv("data/metadata2.csv")

df1 = cbind(df[, 3], df[, 13:16])

priors <- c(
	set_prior("normal(0,10)", nlpar = "b1"),
	set_prior("normal(0,10)", nlpar = "b2"),
	set_prior("normal(0,10)", nlpar = "gamma")
)

priors_ <- set_prior("normal(0,10)", nlpar = "gamma")

nlform <- bf(lnwtp ~ 1 + (log((exp(gamma * q1) - exp(gamma * q0)) / gamma)),
			 gamma~1, nl = TRUE)

fit <- brm(
	nlform,
	family = gaussian(),
	data = df1,
	prior = priors_, chain = 1)


beta ~ normal(0,10);
gamma ~ normal(0,10);
sigma ~ inv_gamma(0.5, 0.5);
lwtp ~ normal(v, sigma);
