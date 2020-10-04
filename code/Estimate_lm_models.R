
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
library(car)
car::vif(lm1) 

summary(lm1)

lm1 <- lm(lnwtp ~ q_percent + lnyear + lninc + #sagulf + nmw + 
		  	local + prov + reg + cult + forest + canada, data  = df)
summary(lm1)

lm1 <- lm(lnwtp ~ q_change + lnyear + lninc + #sagulf + nmw + 
		  	local + prov + reg + cult + 
		  	volunt + lumpsum + 
		  	forest + canada, data  = df)
summary(lm1)


