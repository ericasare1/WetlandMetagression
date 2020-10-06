colnames(df)
#Linear Regression (Frequentist) estimations
library(car)
lm1 <- lm(lnwtp ~ q01 + lnyear + lninc + us + 
		  	local + 
		  	prov + reg + cult + 
		  	forest + #q0 + q1 +
		  	volunt + lumpsum + ce + nrev, data  = df)
car::vif(lm1) 

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


