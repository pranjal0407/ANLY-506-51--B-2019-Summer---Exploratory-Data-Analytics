# Checking Assumptions
sim_1 = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x, y)
}

sim_2 = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x, y)
}

sim_3 = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}

#Fitted versus Residuals Plot
# simulate observations from this model
set.seed(42)
sim_data_1 = sim_1()
head(sim_data_1)

#fit the model and add the fitted line to a scatterplot
plot(y ~ x, data = sim_data_1, col = "grey", pch = 20,
     main = "Data from Model 1")
fit_1 = lm(y ~ x, data = sim_data_1)
abline(fit_1, col = "darkorange", lwd = 3)

#residuals versus predicted plot
plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

#example of non-constant variance
set.seed(42)
sim_data_2 = sim_2()
fit_2 = lm(y ~ x, data = sim_data_2)
plot(y ~ x, data = sim_data_2, col = "grey", pch = 20,
     main = "Data from Model 2")
abline(fit_2, col = "darkorange", lwd = 3)

#In case of MLR, a fitted vs residual plot is necessary
plot(fitted(fit_2), resid(fit_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd = 2)

#below is an example where Y is not linear combination of predictors
set.seed(42)
sim_data_3 = sim_3()
fit_3 = lm(y ~ x, data = sim_data_3)
plot(y ~ x, data = sim_data_3, col = "grey", pch = 20,
     main = "Data from Model 3")
abline(fit_3, col = "darkorange", lwd = 3)

#plotting model
plot(fitted(fit_3), resid(fit_3), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd = 2)

#Breusch-Pagan Test
#Installing lmtest package and using library lmtest
install.packages("lmtest")
library(lmtest)

#3 models
bptest(fit_1)
bptest(fit_2)
bptest(fit_3)

#Histograms
#Histogram is the most obvious to assess normality assumption
par(mfrow = c(1, 3))
hist(resid(fit_1),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, fit_1",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_2),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, fit_2",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_3),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, fit_3",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)

#normal quantile-quantile plot or Q-Q plot
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

#Function to create a Q-Q plot
qq_plot = function(e) {
  
  n = length(e)
  normal_quantiles = qnorm(((1:n - 0.5) / n))
  # normal_quantiles = qnorm(((1:n) / (n + 1)))
  
  # plot theoretical verus observed quantiles
  plot(normal_quantiles, sort(e),
       xlab = c("Theoretical Quantiles"),
       ylab = c("Sample Quantiles"),
       col = "darkgrey")
  title("Normal Q-Q Plot")
  
  # calculate line through the first and third quartiles
  slope     = (quantile(e, 0.75) - quantile(e, 0.25)) / (qnorm(0.75) - qnorm(0.25))
  intercept = quantile(e, 0.25) - slope * qnorm(0.25)
  
  # add to existing plot
  abline(intercept, slope, lty = 2, lwd = 2, col = "dodgerblue")
}

# verify that it is essentially equivalent to using qqnorm() and qqline()
set.seed(420)
x = rnorm(100, mean = 0 , sd = 1)
par(mfrow = c(1, 2))
qqnorm(x, col = "darkgrey")
qqline(x, lty = 2, lwd = 2, col = "dodgerblue")
qq_plot(x)

# simulate data from a normal distribution with different sample sizes
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rnorm(10))
qq_plot(rnorm(25))
qq_plot(rnorm(100))

# simulate data from a  
# t distribution with a small degrees of freedom, for different sample sizes
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rt(10, df = 4))
qq_plot(rt(25, df = 4))
qq_plot(rt(100, df = 4))

#simulate data from an exponential distribution
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rexp(10))
qq_plot(rexp(25))
qq_plot(rexp(100))

#creating Q-Q plot for each fit_1, fit_2 and fit_3 to assess normality of errors
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

qqnorm(resid(fit_2), main = "Normal Q-Q Plot, fit_2", col = "darkgrey")
qqline(resid(fit_2), col = "dodgerblue", lwd = 2)

qqnorm(resid(fit_3), main = "Normal Q-Q Plot, fit_3", col = "darkgrey")
qqline(resid(fit_3), col = "dodgerblue", lwd = 2)

# Shapiro-Wilk Test - Used for formal testing
set.seed(42)
shapiro.test(rnorm(25))
shapiro.test(rexp(25))

# running shapiro.test() for fit_1, fit_2 and fit_3
shapiro.test(resid(fit_1))
shapiro.test(resid(fit_2))
shapiro.test(resid(fit_3))

#Unusual Observations
#Create lm models
par(mfrow = c(1, 3))
set.seed(42)
ex_data  = data.frame(x = 1:10,
                      y = 10:1 + rnorm(n = 10))
ex_model = lm(y ~ x, data = ex_data)

# low leverage, large residual, small influence
point_1 = c(5.4, 11)
ex_data_1 = rbind(ex_data, point_1)
model_1 = lm(y ~ x, data = ex_data_1)
plot(y ~ x, data = ex_data_1, cex = 2, pch = 20, col = "grey",
     main = "Low Leverage, Large Residual, Small Influence")
points(x = point_1[1], y = point_1[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_1, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, small residual, small influence
point_2 = c(18, -5.7)
ex_data_2 = rbind(ex_data, point_2)
model_2 = lm(y ~ x, data = ex_data_2)
plot(y ~ x, data = ex_data_2, cex = 2, pch = 20, col = "grey",
     main = "High Leverage, Small Residual, Small Influence")
points(x = point_2[1], y = point_2[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_2, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, large residual, large influence
point_3 = c(14, 5.1)
ex_data_3 = rbind(ex_data, point_3)
model_3 = lm(y ~ x, data = ex_data_3)
plot(y ~ x, data = ex_data_3, cex = 2, pch = 20, col = "grey", ylim = c(-3, 12),
     main = "High Leverage, Large Residual, Large Influence")
points(x = point_3[1], y = point_3[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_3, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

#slope of the regression
coef(ex_model)[2]
coef(model_1)[2]
coef(model_2)[2]
coef(model_3)[2]

#Leverage
#Find leverage. Creating df lev_ex
lev_ex = data.frame(
  x1 = c(0, 11, 11, 7, 4, 10, 5, 8),
  x2 = c(1, 5, 4, 3, 1, 4, 4, 2),
  y  = c(11, 15, 13, 14, 0, 19, 16, 8))

plot(x2 ~ x1, data = lev_ex, cex = 2)
points(7, 3, pch = 20, col = "red", cex = 2)

#create the X matrix, then calculate H as defined, and extract the diagonal elements
X = cbind(rep(1, 8), lev_ex$x1, lev_ex$x2)
H = X %*% solve(t(X) %*% X) %*% t(X)
diag(H)

#sum of the diagonal elements is 3
sum(diag(H))

#use the hatvalues() function, which returns the leverages
lev_fit = lm(y ~ ., data = lev_ex)
hatvalues(lev_fit)
coef(lev_fit)

#modify the y value of the point with the highest leverage and check coefficients
which.max(hatvalues(lev_fit))
lev_ex[which.max(hatvalues(lev_fit)),]

#create a copy of the data, and modify this point to have a y value of 20
lev_ex_1 = lev_ex
lev_ex_1$y[1] = 20
lm(y ~ ., data = lev_ex_1)

#modify the y value of the point with the lowest leverage and check coefficients
which.min(hatvalues(lev_fit))
lev_ex[which.min(hatvalues(lev_fit)),]

#create a copy of the data, and modify this point to have a y value of 20
lev_ex_2 = lev_ex
lev_ex_2$y[4] = 20
lm(y ~ ., data = lev_ex_2)

#Mean
mean(lev_ex$x1)
mean(lev_ex$x2)
lev_ex[4,]

#calculate the leverages for each for the three plots each woth an added point
hatvalues(model_1)
hatvalues(model_2)
hatvalues(model_3)

#Checing large hat values
hatvalues(model_1) > 2 * mean(hatvalues(model_1))
hatvalues(model_2) > 2 * mean(hatvalues(model_2))
hatvalues(model_3) > 2 * mean(hatvalues(model_3))

#Outliers
#Standardized residuals can be obtained in R by using rstandard() 
#where we would normally use resid()
resid(model_1)
rstandard(model_1)
rstandard(model_1)[abs(rstandard(model_1)) > 2]
resid(model_2)
rstandard(model_2)
rstandard(model_2)[abs(rstandard(model_2)) > 2]
resid(model_3)
rstandard(model_3)
rstandard(model_3)[abs(rstandard(model_3)) > 2]

#Influence using cooks distance
cooks.distance(model_1)[11] > 4 / length(cooks.distance(model_1))
cooks.distance(model_2)[11] > 4 / length(cooks.distance(model_2))
cooks.distance(model_3)[11] > 4 / length(cooks.distance(model_3))


