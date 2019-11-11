setwd("C:/Ankit/Y2S1/DATA7202 Statistical Methods for Data Science/Assignment/Assignment_2")
raw_data = read.csv("Data/OnlineNewsPopularity.csv")

#remove non predictive columns
data = raw_data[c(3:61)]

#change shares to numeric
data[, c(59)] <- sapply(data[, c(59)], as.numeric)


#change categorical data to factor
data$data_channel_is_lifestyle <- as.factor(data$data_channel_is_lifestyle)
data$data_channel_is_entertainment <- as.factor(data$data_channel_is_entertainment)
data$data_channel_is_bus <- as.factor(data$data_channel_is_bus)
data$data_channel_is_socmed <- as.factor(data$data_channel_is_socmed)
data$data_channel_is_tech <- as.factor(data$data_channel_is_tech)
data$data_channel_is_world <- as.factor(data$data_channel_is_world)
data$weekday_is_monday <- as.factor(data$weekday_is_monday)
data$weekday_is_tuesday <- as.factor(data$weekday_is_tuesday)
data$weekday_is_wednesday <-as.factor(data$weekday_is_wednesday)
data$weekday_is_thursday <- as.factor(data$weekday_is_thursday)
data$weekday_is_friday <- as.factor(data$weekday_is_friday)
data$weekday_is_saturday <- as.factor(data$weekday_is_saturday)
data$weekday_is_sunday <- as.factor(data$weekday_is_sunday)
data$is_weekend <- as.factor(data$is_weekend)



#Produce test set and train set.
#Train
#Seed for reproducible random number generator
set.seed(5291)
#Take 29500 samples in the Training set (Approximately 75%)
row_train_check = sample(floor(nrow(data)),size = 29500,replace = FALSE)
data_train_check = data[row_train_check, ]

#Test 
data_not_train_check = data[which(!(1:nrow(data))%in%row_train_check),]
row_test_check = sample(floor(nrow(data_not_train_check)),replace = FALSE)
data_test_check = data_not_train_check[row_test_check, ]



#install.packages("fitdistrplus")
library(fitdistrplus)
#install.packages("logspline")
library(logspline)

png(filename=paste("./Plot/1.1_Cullen&FreyGraph",".png"), width = 640, height = 480)
descdist(data_train_check$shares, discrete = FALSE)
dev.off()

fit.norm = fitdist(data_train_check$shares, "norm")
png(filename=paste("./Plot/1.2_DistributionPlot_Normal",".png"), width = 640, height = 480)
plot(fit.norm)
dev.off()
fit.norm$aic

fit.lnorm = fitdist(data_train_check$shares, "lnorm")
png(filename=paste("./Plot/1.3_DistributionPlot_LogNormal",".png"), width = 640, height = 480)
plot(fit.lnorm)
dev.off()
fit.lnorm$aic

# fit.poisson = fitdist(data_train_check$shares, "pois")
# png(filename=paste("./Plot/1.4_DistributionPlot_Poisson",".png"), width = 640, height = 480)
# plot(fit.poisson)
# dev.off()
# fit.poisson$aic

data_lm = lm(shares~.,  data = data_train_check)
png(filename=paste("./Plot/2.1_Multiple_Regression_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_lm)
dev.off()

data_shares_pred_old = predict.lm(data_lm,data_test_check)

png(filename=paste("./Plot/2.2_Multiple_Regression_b",".png"), width = 640, height = 480)
hist(data_shares_pred_old)
dev.off()


preds_ac_old = data.frame(cbind(actual= data_test_check$shares,predicted
                                = data_shares_pred_old))
preds_ac_old$predicted[which(preds_ac_old$predicted<0)] = 0
min_max_accuracy_old = mean(apply(preds_ac_old, 1, min) /
                              apply(preds_ac_old, 1, max))

#install.packages("dvmisc")
#library("dvmisc")
#get_mse(data_lm, var.estimate = TRUE)

library("rcompanion")
png(filename=paste("./Plot/2.3_Multiple_Regression_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_lm), breaks=50)
dev.off()

sum_data_lm = summary(data_lm)

#install.packages("Metrics")
library("Metrics")

mse(data_shares_pred_old, data_test_check$shares)
rmse(data_shares_pred_old, data_test_check$shares)

#Gaussian Family Identity Link

data_glm_gaussian_identity = glm(shares~.,family = gaussian(link = identity), data =
                     data_train_check)



png(filename=paste("./Plot/3.1_gaussian_identity_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_gaussian_identity)
dev.off()

data_shares_pred_new_gaussian =(predict.glm(data_glm_gaussian_identity,data_test_check)) 

png(filename=paste("./Plot/3.2_gaussian_identity_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new_gaussian)
dev.off()

preds_ac_new_guassian = data.frame(cbind(actual=
                                  data_test_check$shares,predicted = data_shares_pred_new_gaussian))
preds_ac_new_guassian$predicted[which(preds_ac_new_guassian$predicted<0)] = 0
min_max_accuracy_gaussian = mean(apply(preds_ac_new_guassian, 1, min) /
                              apply(preds_ac_new_guassian, 1, max))

png(filename=paste("./Plot/3.3_gaussian_identity_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_gaussian_identity), breaks=50)
dev.off()


sum_data_glm_gaussian_identity = summary(data_glm_gaussian_identity)

mse(data_shares_pred_new_gaussian, data_test_check$shares)
rmse(data_shares_pred_new_gaussian, data_test_check$shares)


#Gamma Family, Log Link

data_glm_gamma_log = glm(shares~.,family = Gamma(link = log), data = data_train_check)


png(filename=paste("./Plot/4.1_gamma_log_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_gamma_log)
dev.off()

data_shares_pred_new_gamma_log =exp(predict.glm(data_glm_gamma_log,data_test_check))

png(filename=paste("./Plot/4.2_gamma_log_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new_gamma_log)
dev.off()

preds_ac_new_gamma_log = data.frame(cbind(actual=
                                           data_test_check$shares,predicted = data_shares_pred_new_gamma_log))
preds_ac_new_gamma_log$predicted[which(preds_ac_new_gamma_log$predicted<0)] = 0
min_max_accuracy_gamma_log = mean(apply(preds_ac_new_gamma_log, 1, min) /
                              apply(preds_ac_new_gamma_log, 1, max))

png(filename=paste("./Plot/4.3_gamma_log_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_gamma_log), breaks=50)
dev.off()


sum_data_glm_gamma_log = summary(data_glm_gamma_log)

mse(data_shares_pred_new_gamma_log, data_test_check$shares)
rmse(data_shares_pred_new_gamma_log, data_test_check$shares)


#Poisson Family, Log Link

data_glm_poisson_log = glm(shares~.,family = poisson(link = log), data =
                                 data_train_check)

png(filename=paste("./Plot/5.1_poisson_log_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_poisson_log)
dev.off()

data_shares_pred_new_poisson_log =exp(predict.glm(data_glm_poisson_log,data_test_check))

png(filename=paste("./Plot/5.2_poisson_log_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new_poisson_log)
dev.off()

preds_ac_new_poisson_log = data.frame(cbind(actual=
                                            data_test_check$shares,predicted = data_shares_pred_new_poisson_log))
preds_ac_new_poisson_log$predicted[which(preds_ac_new_poisson_log$predicted<0)] = 0
min_max_accuracy_poisson_log = mean(apply(preds_ac_new_poisson_log, 1, min) /
                              apply(preds_ac_new_poisson_log, 1, max))

png(filename=paste("./Plot/5.3_poisson_log_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_poisson_log), breaks=50)
dev.off()

sum_data_glm_poisson_log = summary(data_glm_poisson_log)

mse(data_shares_pred_new_poisson_log, data_test_check$shares)
rmse(data_shares_pred_new_poisson_log, data_test_check$shares)

#Poisson Family, Sqrt Link

data_glm_poisson_sqrt = glm(shares~.,family = poisson(link = sqrt), data =
                                  data_train_check)

png(filename=paste("./Plot/6.1_poisson_sqrt_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_poisson_sqrt)
dev.off()

data_shares_pred_new_poisson_sqrt =(predict.glm(data_glm_poisson_sqrt,data_test_check))^2

png(filename=paste("./Plot/6.2_poisson_sqrt_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new_poisson_sqrt)
dev.off()

preds_ac_new_poisson_sqrt = data.frame(cbind(actual=
                                              data_test_check$shares,predicted = data_shares_pred_new_poisson_sqrt))
preds_ac_new_poisson_sqrt$predicted[which(preds_ac_new_poisson_sqrt$predicted<0)] = 0
min_max_accuracy_poisson_sqrt = mean(apply(preds_ac_new_poisson_sqrt, 1, min) /
                                      apply(preds_ac_new_poisson_sqrt, 1, max))

png(filename=paste("./Plot/6.3_poisson_sqrt_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_poisson_sqrt), breaks=50)
dev.off()

sum_data_glm_poisson_sqrt = summary(data_glm_poisson_sqrt)

mse(data_shares_pred_new_poisson_sqrt, data_test_check$shares)
rmse(data_shares_pred_new_poisson_sqrt, data_test_check$shares)

#Quasi Family, Identity link

data_glm_quasi_identity = glm(shares~.,family = quasi(link = identity), data =
                               data_train_check)

png(filename=paste("./Plot/7.1_quasi_identity_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_quasi_identity)
dev.off()

data_shares_pred_new_quasi_identity =(predict.glm(data_glm_quasi_identity,data_test_check))

png(filename=paste("./Plot/7.2_quasi_identity_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new_quasi_identity)
dev.off()

preds_ac_new_quasi_identity = data.frame(cbind(actual=
                                               data_test_check$shares,predicted = data_shares_pred_new_quasi_identity))
preds_ac_new_quasi_identity$predicted[which(preds_ac_new_quasi_identity$predicted<0)] = 0
min_max_accuracy_quasi_identity = mean(apply(preds_ac_new_quasi_identity, 1, min) /
                                       apply(preds_ac_new_quasi_identity, 1, max))

png(filename=paste("./Plot/7.3_quasi_identity_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_quasi_identity), breaks=50)
dev.off()

sum_data_glm_quasi_identity = summary(data_glm_quasi_identity)

mse(data_shares_pred_new_quasi_identity, data_test_check$shares)
rmse(data_shares_pred_new_quasi_identity, data_test_check$shares)
