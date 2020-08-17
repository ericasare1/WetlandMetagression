library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#Ex. This is an example in Section 5.5 of Gelman et al (2003), which studied coaching effects from eight schools. For simplicity, we call this example "eight schools."
