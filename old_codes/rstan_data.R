# Setting Rstan data

df_freshwl <- df %>% filter(wlfresh ==1)
df_us_freshwl <- df_freshwl %>% filter(canada ==0)
df_can_freshwl <- df_freshwl %>% filter(canada ==1)
insample_pred <- data.frame(cbind(rep(1,nrow(as.matrix(df_can_freshwl[, 5:6]))), 
					   as.matrix(df_can_freshwl[, 5:6]),
					   as.matrix(df_can_freshwl[, 10:14]),
					   as.matrix(df_can_freshwl[, 17:18]), as.matrix(df_can_freshwl[,22])))

write.csv(df_freshwl, "data/df_freshwl.csv")
write.csv(df_us_freshwl, "data/df_us_fresh.csv")
write.csv(df_can_freshwl, "data/df_can_fresh.csv")
write.csv(insample_pred, "data/insample_pred.csv")

# Freshwater US and Canada Combined Dataset
n_ind <- n_distinct(df_freshwl$studyid)
data_stan_freshwl <- list(N=nrow(df_freshwl),
						S = n_ind,
						lwtp=df_freshwl$lnwtp,
						#				  x=as.matrix(df[,6]),
						x=cbind(rep(1,nrow(as.matrix(df_freshwl[, 5:6]))), 
								as.matrix(df_freshwl[, 5:6]),
								as.matrix(df_freshwl[, 10:14]),
								as.matrix(df_freshwl[, 17:18]), as.matrix(df_freshwl[,22])), 
						q0 = df_freshwl$q0,
						q1 = df_freshwl$q1,
						q0new = df_can_freshwl$q0,
						q1new = df_can_freshwl$q1)

data_stan_freshwl$K <- ncol(data_stan_freshwl$x)
data_stan_freshwl$xnew <- insample_pred
data_stan_freshwl$Nnew <- nrow(insample_pred)
# Freshwater US
n_ind_us_freshwl <- n_distinct(df_us_freshwl$studyid)
data_stan_us_freshwl <- list(N=nrow(df_us_freshwl),
						  S = n_ind_us_freshwl,
						  lwtp=df_us_freshwl$lnwtp,
						  #				  x=as.matrix(df[,6]),
						  x=cbind(rep(1,nrow(as.matrix(df_us_freshwl[, 5:6]))),
						  		as.matrix(df_us_freshwl[, 5:6]),
						  		as.matrix(df_us_freshwl[, 10:14]), 
						  		as.matrix(df_us_freshwl[, 17:18])),
						  q0 = df_us_freshwl$q0,
						  q1 = df_us_freshwl$q1,
						  q0new = df_can_freshwl$q0,
						  q1new = df_can_freshwl$q1)

data_stan_us_freshwl$K <- ncol(data_stan_us_freshwl$x)
data_stan_us_freshwl$xnew <- insample_pred[,1:10]
data_stan_us_freshwl$Nnew <- nrow(insample_pred)
# Freshwater canada
n_ind_can_freshwl <- n_distinct(df_can_freshwl$studyid)
data_stan_can_freshwl <- list(N=nrow(df_can_freshwl),
							 S = n_ind_can_freshwl,
							 lwtp=df_can_freshwl$lnwtp,
							 #				  x=as.matrix(df[,6]),
							 x=cbind(rep(1,nrow(as.matrix(df_can_freshwl[, 5:6]))),
							 		as.matrix(df_can_freshwl[, 5:6]),
							 		as.matrix(df_can_freshwl[, 10:14]), 
							 		as.matrix(df_can_freshwl[, 17:18])),
							 q0 = df_can_freshwl$q0,
							 q1 = df_can_freshwl$q1,
							 q0new = df_can_freshwl$q0,
							 q1new = df_can_freshwl$q1) 

data_stan_can_freshwl$K <- ncol(data_stan_can_freshwl$x)
data_stan_can_freshwl$xnew <- insample_pred[,1:10]
data_stan_can_freshwl$Nnew <- nrow(insample_pred)

###..Out of Sample Prediction ----PHJV Landscapes in Saskatchewan.
x_sask <- read_csv("data/phjv_sask.csv")  
x_saskq1q0 <- read_csv("data/phjv_sask_q0q1.csv")
x_sask_can <- read_csv("data/phjv_sask_can.csv")

n_ind_freshwl <- n_distinct(df_freshwl$studyid)
data_stan_can_freshwl_sask <- list(N=nrow(df_can_freshwl),
						  S = n_ind_freshwl,
						  lwtp=df_can_freshwl$lnwtp,
						  #				  x=as.matrix(df[,6]),
						  x=cbind(rep(1,nrow(as.matrix(df_can_freshwl[, 5:6]))),
						  		as.matrix(df_can_freshwl[, 5:6]),
						  		as.matrix(df_can_freshwl[, 10:14]), 
						  		as.matrix(df_can_freshwl[, 17:18])),
						  q0 = df_can_freshwl$q0,
						  q1 = df_can_freshwl$q1,
						  q0new = x_saskq1q0$q0,
						  q1new = x_saskq1q0$q1)

data_stan_can_freshwl_sask$K <- ncol(data_stan_can_freshwl_sask$x)
data_stan_can_freshwl_sask$xnew <- x_sask_can
data_stan_can_freshwl_sask$Nnew <- nrow(x_sask_can)
#only freshwetlands and Canada
n_ind_freshwl_can <- n_distinct(df_canada_fresh$studyid)
data_stan_freshwl_can_sask <- list(N=nrow(df_canada_fresh),
							  S = n_ind_freshwl_can,
							  lwtp=df_canada_fresh$lnwtp,
							  #				  x=as.matrix(df[,6]),
							  x=cbind(rep(1,nrow(df_canada_fresh)),as.matrix(df_canada_fresh[, 5:7]),
							  		as.matrix(df_canada_fresh[, 10:14]), 
							  		as.matrix(df_canada_fresh[, 17:18])), 
							  q0 = df_canada_fresh$q0,
							  q1 = df_canada_fresh$q1,							  
							  q0new = x_saskq1q0$q0,
							  q1new = x_saskq1q0$q1)

data_stan_freshwl_can_sask$K <- ncol(data_stan_freshwl_can_sask$x)
data_stan_freshwl_can_sask$xnew <- x_sask_can
data_stan_freshwl_can_sask$Nnew <- nrow(x_sask_can)
#only freshwetlands and us
n_ind_freshwl_us <- n_distinct(df_us_fresh$studyid)
data_stan_freshwl_us_sask <- list(N=nrow(df_us_fresh),
							 S = n_ind_freshwl_us,
							 lwtp=df_us_fresh$lnwtp,
							 #				  x=as.matrix(df[,6]),
							 x=cbind(rep(1,nrow(df_us_fresh)),as.matrix(df_us_fresh[, 5:7]),
							 		as.matrix(df_us_fresh[, 10:14]), 
							 		as.matrix(df_us_fresh[, 17:18])), 
							 q0 = df_us_fresh$q0,
							 q1 = df_us_fresh$q1,						  
							 q0new = x_saskq1q0$q0,
							 q1new = x_saskq1q0$q1)

data_stan_freshwl_us_sask$K <- ncol(data_stan_freshwl_us_sask$x)
data_stan_freshwl_us_sask$xnew <- x_sask_can
data_stan_freshwl_us_sask$Nnew <- nrow(x_sask_can)

###..................Nonlinear Model Prediction Prairie data.................................
#only freshwetlands
x_prairie <- read_csv("data/ducks_prairie.csv")
x_prairie_can <- read_csv("data/ducks_prairie_can.csv")
x_prairieq1q0 <- read_csv("data/duks_sask_q0q1.csv")

n_ind_freshwl <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_prairie <- list(N=nrow(df_freshwl),
							   S = n_ind_freshwl,
							   lwtp=df_freshwl$lnwtp,
							   #				  x=as.matrix(df[,6]),
							   x=cbind(rep(1,nrow(df_freshwl)),as.matrix(df_freshwl[, 5:7]),
							   		as.matrix(df_freshwl[, 10:14]), as.matrix(df_freshwl[, 17:18]), 
							   		as.matrix(df_freshwl[,22])),
							   q0 = df_freshwl$q0,
							   q1 = df_freshwl$q1,
							   q0new = x_prairieq1q0$q0,
							   q1new = x_prairieq1q0$q1)

data_stan_freshwl_prairie$K <- ncol(data_stan_freshwl_prairie$x)
data_stan_freshwl_prairie$xnew <- x_prairie
data_stan_freshwl_prairie$Nnew <- nrow(x_prairie)
dim(x_prairie)
#only freshwetlands and Canada
n_ind_freshwl_can <- n_distinct(df_canada_fresh$studyid)
data_stan_freshwl_can_prairie <- list(N=nrow(df_canada_fresh),
								   S = n_ind_freshwl_can,
								   lwtp=df_canada_fresh$lnwtp,
								   #				  x=as.matrix(df[,6]),
								   x=cbind(rep(1,nrow(df_canada_fresh)),as.matrix(df_canada_fresh[, 5:7]),
								   		as.matrix(df_canada_fresh[, 10:14]), 
								   		as.matrix(df_canada_fresh[, 17:18])), 
								   q0 = df_canada_fresh$q0,
								   q1 = df_canada_fresh$q1,
								   q0new = x_prairieq1q0$q0,
								   q1new = x_prairieq1q0$q1)

data_stan_freshwl_can_prairie$K <- ncol(data_stan_freshwl_can_prairie$x)
data_stan_freshwl_can_prairie$xnew <- x_prairie_can
data_stan_freshwl_can_prairie$Nnew <- nrow(x_prairie_can)


