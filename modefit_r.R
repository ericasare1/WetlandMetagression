#Stan is not necessary for estimating this simple model, but the example if useful for illustrating the three approaches to making predictions with Stan. The data generating process is:
#	y ∼ Bernoulli(π);
#π = inv_logit(α+β∗x)
#We’ll want to estimate α and β so that we can make predictions for unseen y based on new data x as it becomes available.

library(dplyr)
library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Create some fake data - logistic regression
set.seed(56)
N <- 500
alpha <- 1
beta <- 2
x <- rnorm(N)
prob <- 1/(1 + exp(-(alpha + beta*x)))
y <- rbinom(N, 1, prob)
# Distribution of y
table(y)

# Split into training and testing
N_train <- N*0.8
N_test <- N*0.2
train_ind <- sample(c(1:N), size = N_train, replace = FALSE)
x_train <- x[train_ind]
x_test <- x[-train_ind]
y_train <- y[train_ind]
y_test <- y[-train_ind]

#Fit-and-predict
#The fit-and-predict approach uses Stan’s generated quantities block to make predictions from x_test in the same program that we used to estimate the relationship between x_train and y_train. From the Stan manual (http://mc-stan.org/users/documentation/index.html):

#The generated quantities program block is rather different than the other blocks. Nothing in the generated quantities block affects the sampled parameter values. The block is executed only after a sample has been generated. Among the applications of posterior inference that can be coded in the generated quantities block are forward sampling to generate simulated data for model testing, generating predictions for new data, calculating posterior event probabilities, including multiple comparisons, sign tests, etc., calculating posterior expectations, transforming parameters for reporting, applying full Bayesian decision theory, calculating log likelihoods, deviances, etc. for model comparison. This Stan program simultaneously fits the logistic regression model based on the training data and generates predictions for y_test based on x_test.

# Recover parameters with stan
fit <- stan(file = "model_fit.stan",
			data = list(x_train, y_train, N_train,
						x_test, N_test),
			chains = 3, iter = 1000)
plot(fit, pars = c("alpha", "beta"))

# Accuracy
ext_fit <- extract(fit)
mean(apply(ext_fit$y_test, 2, median) == y_test)
## [1] 0.75

#Predict with R
#Another option is to extract the posterior distributions of the parameters and use them to recreate the model in R. The code below extracts the posterior distributions (alpha_post and beta_post) and uses them in a prediction function gen_quant_r. The function simulates the data generating process with samples from the parameters’ posterior distributions. This approach doesn’t generate full posterior distributions for each prediction, but it is fast and easy to implement.

# Extract posteriod distributions  ...Case 1
alpha_post <- ext_fit$alpha
beta_post <- ext_fit$beta
# Function for simulating y based on new x
gen_quant_r <- function(x) {
	lin_comb <- sample(alpha_post, size = length(x)) + x*sample(beta_post, size = length(x))
	prob <- 1/(1 + exp(-lin_comb))
	out <- rbinom(length(x), 1, prob)
	return(out)
}
# Run the function on x_test...
set.seed(56)
y_pred_r <- gen_quant_r(x_test)
# Accuracy
mean(y_pred_r == y_test)
## [1] 0.75 

#The accuracy of this predictive approach is similar to the accuracy of the fit-and-predict approach.

#Predict with Stan.....Case 2
#The third approach is to write another Stan program to make predictions without refitting the old model. The parameter estimates from the original program become the data for the prediction program. The code below shows how this program might look. Note that the parameters and model blocks are empty because we are not estimating parameter distributions from a model.

data {
	int N;
	int N_samples;
	vector[N] x_test;
	vector[N_samples] alpha;
	vector[N_samples] beta;
}
parameters 
}
model {
}
generated quantities {
	matrix[N_samples, N] y_test;
	for(n in 1:N) {
		for(i in 1:N_samples) {
			y_test[i, n] = bernoulli_rng(inv_logit(alpha[i] + beta[i]*x_test[n]));
		}  
	}
}


#When we run this program, we have to set the algorithm to fixed_param so Stan knows that it’s not estimating parameters. We have to go through a bit of effort to extract the distributions of the generated quantities. But once we do, we’ll have full posterior distributions of our predictions with similar accuracy to the two approaches outlined above.

pred <- stan(file = "model_pred.stan",
			 data = list(x_test = x_test, N = N_test,
			 			N_samples = length(alpha_post),
			 			alpha = alpha_post,
			 			beta = beta_post),
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
# Accuracy
(apply(out_mat, 2, median) %>% round(0) == y_test) %>% mean()
## [1] 0.75


#Conclusion
#Stan is a powerful language for Bayesian inference with a robust mechanism for out-of-sample prediction in its generated quantities block. However, it is not easy to ‘re-use’ Stan models on new data as it becomes available, particularly if the data is streaming in and predictions must be made regularly. This post has outlined a few options for making live predictions with Stan. Each has strengths and weaknesses; which is most appropriate depends on the use-case.