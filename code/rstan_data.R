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

#only whole and Canada
n_ind_whole_can <- n_distinct(df_canada$studyid)
data_stan_wholel_can <- list(N=nrow(df_canada),
							 S = n_ind_whole_can,
							 lwtp=df_canada$lnwtp,
							 #				  x=as.matrix(df[,6]),
							 x=cbind(rep(1,nrow(df_canada)),as.matrix(df_canada[, 4:6]),
							 		as.matrix(df_canada[, 10:14]),
							 		as.matrix(df_canada[, 17:21])), 
							 q0 = df_canada$q0,
							 q1 = df_canada$q1)

data_stan_wholel_can$K <- ncol(data_stan_wholel_can$x)

###..................Prediction data.................................
#only freshwetlands
n_ind_freshwl <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_pred <- list(N=nrow(df_freshwl),
						  S = n_ind_freshwl,
						  lwtp=df_freshwl$lnwtp,
						  #				  x=as.matrix(df[,6]),
						  x=cbind(rep(1,nrow(df_freshwl)),as.matrix(df_freshwl[, 5:7]),
						  		as.matrix(df_freshwl[, 10:14]), as.matrix(df_freshwl[, 17:18]), 
						  		as.matrix(df_freshwl[,22])),
						  q0 = df_freshwl$q0,
						  q1 = df_freshwl$q1)

data_stan_freshwl_pred$K <- ncol(data_stan_freshwl_pred$x)
data_stan_freshwl_pred$xnew <- data_stan_freshwl_pred$x[1:2,]
data_stan_freshwl_pred$Nnew <- nrow(data_stan_freshwl_pred$xnew)
#only freshwetlands and Canada
n_ind_freshwl_can <- n_distinct(df_canada_fresh$studyid)
data_stan_freshwl_can_pred <- list(N=nrow(df_canada_fresh),
							  S = n_ind_freshwl_can,
							  lwtp=df_canada_fresh$lnwtp,
							  #				  x=as.matrix(df[,6]),
							  x=cbind(rep(1,nrow(df_canada_fresh)),as.matrix(df_canada_fresh[, 5:7]),
							  		as.matrix(df_canada_fresh[, 10:14]), 
							  		as.matrix(df_canada_fresh[, 17:18])), 
							  q0 = df_canada_fresh$q0,
							  q1 = df_canada_fresh$q1)

data_stan_freshwl_can_pred$K <- ncol(data_stan_freshwl_can_pred$x)
data_stan_freshwl_can_pred$xnew <- data_stan_freshwl_can_pred$x[1:2,]
data_stan_freshwl_can_pred$Nnew <- nrow(data_stan_freshwl_can_pred$xnew)

