# Import the Twitch Data Set
lm.1 <- lm(log(twitch$Watch.time.Minutes)~.-Channel-Watch.time.Minutes.-Partnered, data = twitch)
summary(lm.1)

# Stepwise backwards
lm.2 <- step(lm.1, direction = "backward")
summary(lm.2)
AIC(lm.2)
BIC(lm.2)

# Collinearity
library(car)
vif(lm.2)

# Get residual plots
par(mfrow=c(2,2))
plot(lm.2)

# Compare paired plots
attributes_to_exclude <- c("Channel", "Partnered", "Mature", "Language")
attributes_to_include <- setdiff(names(twitch), attributes_to_exclude)
twitch.pairs <- twitch[, attributes_to_include]
twitch.pairs$Watch.time.Minutes. = log(twitch.pairs$Watch.time.Minutes.)
pairs(twitch.pairs)

# Training/Testing Sets
twitch.new = twitch[, attributes_to_include]
twitch.new$Watch.time.Minutes. = log(twitch.new$Watch.time.Minutes.)
MSE = rep(0,10)
for (i in 1:10) {
  set.seed(i)
  train = sample(1:nrow(twitch.new), 0.8*nrow(twitch.new))
  test = twitch.new[-train,]
  twitch.lm = lm(twitch.new$Watch.time.Minutes ~., data=twitch.new,
                subset=train)
  yhat = predict(twitch.lm, newdata=test)
  MSE[i] = mean((yhat - twitch.new$Watch.time.Minutes) * (yhat - twitch.new$Watch.time.Minutes))
}
MSE
mean(MSE)

# Polynomial Regression
lm.poly <- lm(log(twitch$Watch.time.Minutes)~poly(Stream.time.minutes. + 
                                                    Peak.viewers + Average.viewers + Followers + Followers.gained + 
                                                    Views.gained, 4), data = twitch)
summary(lm.poly)
