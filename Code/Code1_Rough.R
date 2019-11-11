setwd("C:/Ankit/Y2S1/DATA7202 Statistical Methods for Data Science/Assignment/Assignment_2")
raw_data = read.csv("Data/OnlineNewsPopularity.csv")

#remove non predictive columns
data = raw_data[c(3:61)]

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

#Removing variables with very high correlation
data_processed = data[,(!names(data) %in% c('n_non_stop_unique_tokens','n_non_stop_words','kw_max_min','self_reference_max_shares','LDA_02','self_reference_min_shares','kw_max_avg
                                              '))]

#We remove which cause multicollinearity

data_processed = data_processed[,(!names(data_processed) %in% c('weekday_is_sunday','is_weekend'))]
data_processed = data_processed[,(!names(data_processed) %in% c('rate_positive_words'))]
data_processed = data_processed[,(!names(data_processed) %in% c('kw_avg_avg'))]
data_processed = data_processed[,(!names(data_processed) %in% c('rate_negative_words'))]
data_processed = data_processed[,(!names(data_processed) %in% c('avg_negative_polarity'))]
data_processed = data_processed[,(!names(data_processed) %in% c('data_channel_is_world'))]
data_processed = data_processed[,(!names(data_processed) %in% c('global_sentiment_polarity'))]
data_processed = data_processed[,(!names(data_processed) %in% c('LDA_04'))]
data_processed = data_processed[,(!names(data_processed) %in% c('kw_max_max'))]
data_processed = data_processed[,(!names(data_processed) %in% c('avg_positive_polarity'))]
data_processed = data_processed[,(!names(data_processed) %in% c('LDA_00'))]
data_processed = data_processed[,(!names(data_processed) %in% c('weekday_is_wednesday'))]

library(MASS)
#install.packages("dplyr")
library(dplyr)
box = boxcox(shares ~ ., data=data_processed)

cox = data.frame(box$x, box$y)
cox2 = cox[with(cox, order(-cox$box.y)),]
#cox2[1,]

lambda = cox2[1, "box.x"]
data_transformed = data_processed%>%
  mutate(cox_shares = ((shares ^ lambda - 1)/lambda),
         log_n_tokens_content = log10(n_tokens_content + 1),
         log_n_unique_tokens = log10(n_unique_tokens + 1),
         log_num_hrefs = log10(num_hrefs + 1),
         log_num_self_hrefs = log10(num_self_hrefs + 1),
         log_num_imgs = log10(num_imgs + 1),
         log_num_videos = log10(num_videos + 1),
         log_average_token_length = log10(average_token_length + 1)
  )

data_transformed = data_transformed[,!(names(data_transformed) %in% c("shares","n_tokens_content", "n_unique_tokens", "num_hrefs",  "num_self_hrefs", "num_imgs", "num_videos", "average_token_length"))]

#Produce test set and train set.
#Train
#Seed for reproducible random number generator
set.seed(5291)
#Take 29500 samples in the Training set (Approximately 75%)
row_train = sample(floor(nrow(data_transformed)),size = 29500,replace = FALSE)
data_train = data_transformed[row_train, ]

#Test 
data_not_train = data_transformed[which(!(1:nrow(data_transformed))%in%row_train),]
row_test = sample(floor(nrow(data_not_train)),replace = FALSE)
data_test = data_not_train[row_test, ]

#install.packages("fitdistrplus")
library(fitdistrplus)
#install.packages("logspline")
library(logspline)

png(filename=paste("./Plot/1.1_Cullen&FreyGraph",".png"), width = 640, height = 480)
descdist(data_train$cox_shares, discrete = FALSE)
dev.off()

fit.norm = fitdist(data_train$cox_shares, "norm")
png(filename=paste("./Plot/1.2_DistributionPlot",".png"), width = 640, height = 480)
plot(fit.norm)
dev.off()
fit.norm$aic

data_glm_old = glm(cox_shares~.,family = gaussian, data = data_train)

#data_glm_old = lm(cox_shares~., data = data_train)
png(filename=paste("./Plot/1.3_Multiple_Regression_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_old)
dev.off()

data_shares_pred_old = predict.lm(data_glm_old,data_test)
png(filename=paste("./Plot/1.4_Multiple_Regression_b",".png"), width = 640, height = 480)
hist(data_shares_pred_old)
dev.off()
preds_ac_old = data.frame(cbind(actual= data_test$cox_shares,predicted
                                = data_shares_pred_old))
preds_ac_old$predicted[which(preds_ac_old$predicted<0)] = 0
min_max_accuracy_old = mean(apply(preds_ac_old, 1, min) /
                              apply(preds_ac_old, 1, max))

# png(filename=paste("./Plot/1.5_Multiple_Regression_c",".png"), width = 640, height = 480)
# hist(data_train$cox_shares)
# dev.off()

#install.packages("rcompanion")
library(rcompanion)
png(filename=paste("./Plot/1.5_Multiple_Regression_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_old), breaks=50)
dev.off()

data_glm_new = glm(cox_shares~., family = quasi, data =
                     data_train)

png(filename=paste("./Plot/1.6_Gaussian_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_new)
dev.off()

data_shares_pred_new =exp(predict.glm(data_glm_new,data_test))

png(filename=paste("./Plot/1.7_Gaussian_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new)
dev.off()

preds_ac_new = data.frame(cbind(actual=
                                  data_test$cox_shares,predicted = data_shares_pred_new))
preds_ac_new$predicted[which(preds_ac_new$predicted<0)] = 0
min_max_accuracy_new = mean(apply(preds_ac_new, 1, min) /
                              apply(preds_ac_new, 1, max))

png(filename=paste("./Plot/1.7_Gaussian_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_new), breaks=50)
dev.off()

sum_old = summary(data_glm_old)
sum_new = summary(data_glm_new)

#------- TEST-------#
#------- TEST-------#
#------- TEST-------#
#------- TEST-------#

data[, c(59)] <- sapply(data[, c(57)], as.numeric)

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

png(filename=paste("./Plot/2.1_Cullen&FreyGraph",".png"), width = 640, height = 480)
descdist(data_train_check$shares, discrete = FALSE)
dev.off()

fit.norm = fitdist(data_train_check$shares, "norm")
png(filename=paste("./Plot/2.2_DistributionPlot",".png"), width = 640, height = 480)
plot(fit.norm)
dev.off()
fit.norm$aic

data_glm_old = lm(shares~., data = data_train_check)
png(filename=paste("./Plot/2.3_Multiple_Regression_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_old)
dev.off()

data_shares_pred_old = predict.lm(data_glm_old,data_test_check)

png(filename=paste("./Plot/2.4_Multiple_Regression_b",".png"), width = 640, height = 480)
hist(data_shares_pred_old)
dev.off()

preds_ac_old = data.frame(cbind(actual= data_test_check$shares,predicted
                                = data_shares_pred_old))
preds_ac_old$predicted[which(preds_ac_old$predicted<0)] = 0
min_max_accuracy_old = mean(apply(preds_ac_old, 1, min) /
                              apply(preds_ac_old, 1, max))

# png(filename=paste("./Plot/1.5_Multiple_Regression_c",".png"), width = 640, height = 480)
# hist(data_train_check$cox_shares)
# dev.off()

library("rcompanion")
png(filename=paste("./Plot/2.5_Multiple_Regression_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_old), breaks=50)
dev.off()

data_glm_new = glm(shares~.,family = gaussian, data =
                     data_train_check)

png(filename=paste("./Plot/2.6_Gaussian_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_new)
dev.off()

data_shares_pred_new =exp(predict.glm(data_glm_new,data_test_check))

png(filename=paste("./Plot/2.7_Gaussian_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new)
dev.off()

preds_ac_new = data.frame(cbind(actual=
                                  data_test_check$shares,predicted = data_shares_pred_new))
preds_ac_new$predicted[which(preds_ac_new$predicted<0)] = 0
min_max_accuracy_new = mean(apply(preds_ac_new, 1, min) /
                              apply(preds_ac_new, 1, max))

png(filename=paste("./Plot/2.7_Gaussian_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_new), breaks=50)
dev.off()

sum_old = summary(data_glm_old)
sum_new = summary(data_glm_new)

#Create logistic data
#install.packages("dplyr")
library(dplyr)
logistic_data <- data %>%
  mutate(popular_article = if_else(shares>1400,1,0))
logistic_data <- logistic_data[,!(names(logistic_data) %in% c("shares"))]

#Train
#Seed for reproducible random number generator
set.seed(5291)
#Take 29500 samples in the Training set (Approximately 75%)
row_train_check_log = sample(floor(nrow(logistic_data)),size = 29500,replace = FALSE)
data_train_check_log = logistic_data[row_train_check_log, ]

#Test 
data_not_train_check_log = logistic_data[which(!(1:nrow(logistic_data))%in%row_train_check_log),]
row_test_check_log = sample(floor(nrow(data_not_train_check_log)),replace = FALSE)
data_test_check_log = data_not_train_check_log[row_test_check_log, ]

data_glm_new_log_a = glm(popular_article~.,family = binomial, data =
                           data_train_check_log)

png(filename=paste("./Plot/1.8_Binomial_a",".png"), width = 640, height = 480)
layout(matrix(c(1,2,3,4),2,2))
plot(data_glm_new_log_a)
dev.off()

data_shares_pred_new_log_a =exp(predict.glm(data_glm_new_log_a,data_test_check_log))

png(filename=paste("./Plot/1.9_Binomial_b",".png"), width = 640, height = 480)
hist(data_shares_pred_new_log_a)
dev.off()

preds_ac_new_log_a = data.frame(cbind(actual=
                                        data_test_check_log$popular_article,predicted = data_shares_pred_new_log_a))
preds_ac_new_log_a$predicted[which(preds_ac_new_log_a$predicted<0)] = 0
min_max_accuracy_new_log_a = mean(apply(preds_ac_new_log_a, 1, min) /
                                    apply(preds_ac_new_log_a, 1, max))

png(filename=paste("./Plot/1.10_Binomial_c",".png"), width = 640, height = 480)
plotNormalHistogram(residuals(data_glm_new_log_a), breaks=50)
dev.off()

#Gamma Family, Inverse Link didn't work

data_glm_gamma_inverse = glm(shares~.,family = Gamma(link = inverse), data = data_train_check)

#Gamma Family, Identity Link didn't work 

data_glm_gamma_inverse = glm(shares~.,family = Gamma(link = identity), data = data_train_check)

#Gaussian Family, Inverse Link didn't work

data_glm_gaussian_identity = glm(shares~.,family = gaussian(link = inverse), data =
                                   data_train_check)

#Gaussian Family, Log Link didn't work

data_glm_gaussian_identity = glm(shares~.,family = gaussian(link = log), data =
                                   data_train_check)

#Binomial Family, Logit Link didn't work

data_glm_binomial_logit = glm(shares~.,family = binomial(link = logit), data =
                                data_train_check)

#Binomial Family, Probit Link didn't work

data_glm_binomial_probit = glm(shares~.,family = binomial(link = probit), data =
                                 data_train_check)

#Binomial Family, Cauchit Link didn't work

data_glm_binomial_probit = glm(shares~.,family = binomial(link = cauchit), data =
                                 data_train_check)

#Binomial Family, Log Link didn't work

data_glm_binomial_log = glm(shares~.,family = binomial(link = log), data =
                              data_train_check)

#Binomial Family, clogLog Link didn't work

data_glm_binomial_log = glm(shares~.,family = binomial(link = cloglog), data =
                              data_train_check)

#Poisson Family, Identity Link didn't work

data_glm_poisson_identity = glm(shares~.,family = poisson(link = identity), data =
                                  data_train_check)

#Inverse Gaussian Family, 1/mu^2 Link didn't work

data_glm_inverse_guassian_mu = glm(shares~.,family = inverse.gaussian(link = 1/mu^2), data =
                                     data_train_check)

#Inverse Gaussian Family, inverse Link didn't work

data_glm_inverse_guassian_inverse = glm(shares~.,family = inverse.gaussian(link = inverse), data =
                                          data_train_check)

#Inverse Gaussian Family, identity Link didn't work

data_glm_inverse_guassian_identity = glm(shares~.,family = inverse.gaussian(link = identity), data =
                                           data_train_check)

#Inverse Gaussian Family, log didn't work

data_glm_inverse_guassian_log = glm(shares~.,family = inverse.gaussian(link = log), data =
                                      data_train_check)

#Quasi Family, logit didn't work

data_glm_quasi_logit = glm(shares~.,family = quasi(link = logit), data =
                             data_train_check)

#Quasi Family, probit didn't work

data_glm_quasi_probit = glm(shares~.,family = quasi(link = probit), data =
                              data_train_check)

#Quasi Family, probit didn't work

data_glm_quasi_probit = glm(shares~.,family = quasi(link = probit), data =
                              data_train_check)

#Quasi Family, cloglog didn't work

data_glm_quasi_cloglog = glm(shares~.,family = quasi(link = cloglog), data =
                               data_train_check)

#Quasi Family, inverse didn't work

data_glm_quasi_identity = glm(shares~.,family = quasi(link = inverse), data =
                                data_train_check)

#Quasi Family, log didn't work

data_glm_quasi_identity = glm(shares~.,family = quasi(link = log), data =
                                data_train_check)

#Quasi Family, 1/mu^2 didn't work

data_glm_quasi_mu = glm(shares~.,family = quasi(link = 1/mu^2), data =
                          data_train_check)

#Quasi Family, sqrt didn't work

data_glm_quasi_sqrt = glm(shares~.,family = quasi(link = sqrt), data =
                            data_train_check)
