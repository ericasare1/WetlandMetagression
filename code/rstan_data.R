# Setting Rstan data

df_freshwl <- df %>% filter(wlfresh ==1)
df_canada_fresh <- df_freshwl %>% filter(canada ==1)
df_canada <- df %>% filter(canada ==1)

str(df)
#Whole dataset
n_ind <- n_distinct(df$studyid)
data_stan_whole <- list(N=nrow(df),
						S = n_ind,
						lwtp=df$lnwtp,
						#				  x=as.matrix(df[,6]),
						x=cbind(rep(1,nrow(df)),as.matrix(df[, 4:14]),
								as.matrix(df[, 17:18]), as.matrix(df[,22])), 
						q0 = df$q0,
						q1 = df$q1)

data_stan_whole$K <- ncol(data_stan_whole$x)
#only freshwetlands
n_ind_freshwl <- n_distinct(df_freshwl$studyid)
data_stan_freshwl <- list(N=nrow(df_freshwl),
						  S = n_ind_freshwl,
						  lwtp=df_freshwl$lnwtp,
						  #				  x=as.matrix(df[,6]),
						  x=cbind(rep(1,nrow(df_freshwl)),as.matrix(df_freshwl[, 5:7]),
						  		as.matrix(df_freshwl[, 10:14]), as.matrix(df_freshwl[, 17:18]), 
						  		as.matrix(df_freshwl[,22])), 
						  q0 = df_freshwl$q0,
						  q1 = df_freshwl$q1)

data_stan_freshwl$K <- ncol(data_stan_freshwl$x)
#only freshwetlands and Canada
n_ind_freshwl_can <- n_distinct(df_canada_fresh$studyid)
data_stan_freshwl_can <- list(N=nrow(df_canada_fresh),
							  S = n_ind_freshwl_can,
							  lwtp=df_canada_fresh$lnwtp,
							  #				  x=as.matrix(df[,6]),
							  x=cbind(rep(1,nrow(df_canada_fresh)),as.matrix(df_canada_fresh[, 5:7]),
							  		as.matrix(df_canada_fresh[, 10:14]), 
							  		as.matrix(df_canada_fresh[, 17:18])), 
							  q0 = df_canada_fresh$q0,
							  q1 = df_canada_fresh$q1)

data_stan_freshwl_can$K <- ncol(data_stan_freshwl_can$x)

###..................Nonlinear Model Prediction Saskatchewan data.................................
#only freshwetlands
x_sask <- read_csv("data/phjv_sask.csv")
x_saskq1q0 <- read_csv("data/phjv_sask_q0q1.csv")

n_ind_freshwl <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_sask <- list(N=nrow(df_freshwl),
						  S = n_ind_freshwl,
						  lwtp=df_freshwl$lnwtp,
						  #				  x=as.matrix(df[,6]),
						  x=cbind(rep(1,nrow(df_freshwl)),as.matrix(df_freshwl[, 5:7]),
						  		as.matrix(df_freshwl[, 10:14]), as.matrix(df_freshwl[, 17:18]), 
						  		as.matrix(df_freshwl[,22])),
						  q0 = x_saskq1q0$q0,
						  q1 = x_saskq1q0$q1)

data_stan_freshwl_sask$K <- ncol(data_stan_freshwl_sask$x)
data_stan_freshwl_sask$xnew <- x_sask
data_stan_freshwl_sask$Nnew <- nrow(x_sask)
#only freshwetlands and Canada
n_ind_freshwl_can <- n_distinct(df_canada_fresh$studyid)
data_stan_freshwl_can_sask <- list(N=nrow(df_canada_fresh),
							  S = n_ind_freshwl_can,
							  lwtp=df_canada_fresh$lnwtp,
							  #				  x=as.matrix(df[,6]),
							  x=cbind(rep(1,nrow(df_canada_fresh)),as.matrix(df_canada_fresh[, 5:7]),
							  		as.matrix(df_canada_fresh[, 10:14]), 
							  		as.matrix(df_canada_fresh[, 17:18])), 
							  q0 = x_saskq1q0$q0,
							  q1 = x_saskq1q0$q1)

data_stan_freshwl_can_sask$K <- ncol(data_stan_freshwl_can_sask$x)
data_stan_freshwl_sask$xnew <- x_sask
data_stan_freshwl_sask$Nnew <- nrow(x_sask)

###..................Nonlinear Model Prediction Prairie data.................................
#only freshwetlands
x_prairie <- read_csv("data/ducks_prairie.csv")
x_prairieq1q0 <- read_csv("data/duks_sask_q0q1.csv")
n_ind_freshwl <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_prairie <- list(N=nrow(df_freshwl),
							   S = n_ind_freshwl,
							   lwtp=df_freshwl$lnwtp,
							   #				  x=as.matrix(df[,6]),
							   x=cbind(rep(1,nrow(df_freshwl)),as.matrix(df_freshwl[, 5:7]),
							   		as.matrix(df_freshwl[, 10:14]), as.matrix(df_freshwl[, 17:18]), 
							   		as.matrix(df_freshwl[,22])),
							   q0 = x_prairieq1q0$q0,
							   q1 = x_prairieq1q0$q1)

data_stan_freshwl_prairie$K <- ncol(data_stan_freshwl_prairie$x)
data_stan_freshwl_prairie$xnew <- x_prairie
data_stan_freshwl_prairie$Nnew <- nrow(x_prairie)
#only freshwetlands and Canada
n_ind_freshwl_can <- n_distinct(df_canada_fresh$studyid)
data_stan_freshwl_can_prairie <- list(N=nrow(df_canada_fresh),
								   S = n_ind_freshwl_can,
								   lwtp=df_canada_fresh$lnwtp,
								   #				  x=as.matrix(df[,6]),
								   x=cbind(rep(1,nrow(df_canada_fresh)),as.matrix(df_canada_fresh[, 5:7]),
								   		as.matrix(df_canada_fresh[, 10:14]), 
								   		as.matrix(df_canada_fresh[, 17:18])), 
								   q0 = x_prairieq1q0$q0,
								   q1 = x_prairieq1q0$q1)

data_stan_freshwl_can_prairie$K <- ncol(data_stan_freshwl_can_prairie$x)
data_stan_freshwl_can_prairie$xnew <- x_prairie
data_stan_freshwl_can_prairie$Nnew <- nrow(x_prairie)

