library(dplyr) 
library(tidyverse)
library(ggplot2)
library(corrplot)
library('Hmisc')
library(readxl)

# Q.1. Import data
toy_sales_data <- read_excel("Desktop/toy_sales_data.xlsx", 
                             sheet = "data")

planned_spend <- read_excel("Desktop/toy_sales_data.xlsx", 
                            sheet = "planned_spend")


toy_sales_data = as.data.frame(toy_sales_data)
# Q.2. Create a plot of sales, TV investment and Digital investment in the y axis with time in the x axis
ggplot(data = toy_sales_data, mapping = aes(x = month))+
  geom_line(mapping = aes(y = sales/100000,color = "sales")) +
  geom_point(mapping = aes(y = sales/100000,color = "sales")) +
  geom_line(mapping = aes(y = tv_spend/100000,color = "tv_spend")) + 
  geom_point(mapping = aes(y = tv_spend/100000,color = "tv_spend")) + 
  geom_line(mapping = aes(y = digital_spend/100000, color = "digital_spend"))+
  geom_point(mapping = aes(y = digital_spend/100000,color = "digital_spend")) + 
  xlab("Month")+
  ylab("Money (Thoundsand dollar)")+
  ggtitle("Toy Sales & Marketing Spending from Jan 2016 to Jan 2018", subtitle = "Created by: Bach Lam")

# Q.3. Report the correlations among sales, TV and Digital investment

# obtain the required data
df_q2 <- toy_sales_data %>% 
  subset(select = c(sales, tv_spend, digital_spend))
# normalize the dataframe & convert the df to a matrix.  
df_q2 <- scale(df_q2)

# rcorr from Hmisc package allows to separately access the correlations and  
# compute Pearson's (or spearman's corr) with or observations and the p-value. 
corr<-rcorr(df_q2) 
# Access the correlation matrix. 
corr_r<-as.matrix(corr[[1]])

corr_r[,1]# subset the correlation of "a" (=var1 ) with the rest if you want.
# get the p-values
pval<-as.matrix(corr[[3]])
# plot all pairs
corrplot(corr_r,method="number",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.1,tl.srt=45)
# plot pairs with significance cutoff defined by "p.mat"
corrplot(corr_r,p.mat = pval,sig.level=0.05,insig = "blank",method="number",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.1,tl.srt=45)

#Q.4. Regression model
# Fit the linear regression model on all available features
fit.sales <- lm(sales~. ,data = toy_sales_data)
 
# Review the obtained result
summary(fit.sales)
# Q.5. Contribution of TV spend on sales 

# Idea: contribution (%) = (mean sum of squares of each feature / total mean sum of squares) * 100
  # acquire the mean sum of squares of each features
  # acquire the total mean sum of squares
  # put the result into dataframe for manipulating the data
    # need to remove the row_6 as it contains the mean sum of square of the residuals
    # calculate the total sum of squares
    # create a new column containing the percentage of contribution

# Check the anova table for calculating the contribution of each variables on sales
anova_table  <- anova(fit.sales)
ss_each <- anova_table$`Mean Sq`
features <- c("month","tv_spend","digital_spend","trend","xmas","Residuals")
df_ss <- data.frame(features,ss_each) 
df_ss <- df_ss[-6,]
totalmeansumofsq_sales <- sum(anova(fit.sales)[,3])

df_ss$contribution <- round((df_ss$ss_each / totalmeansumofsq_sales)*100)
Total_sales_values <- colSums(toy_sales_data['sales'])
df_ss$contributionindollar <- (df_ss$contribution/100)*Total_sales_values

# Q.6. Return on Investment for TV Spend
TV_spend_RoI <- ((df_ss[2,4] - colSums(toy_sales_data['tv_spend']))/colSums(toy_sales_data['tv_spend']))*100

# Q.7. Predict the expected sales value for 1Q2018

planned_spend

plot(toy_sales_data$sales,fit.sales$fitted.values)
planned_spend$trend <- c(25,26,27)
planned_spend$xmas <- c(0,0,0)
pred_sales <- predict(fit.sales, newdata = planned_spend)
pred_sales

# Q.8. Additional data that could improve the model
# 1. Add more observations
library(pwr) # package for doing the power analysis (determine the sample size)
# pwr.f2.test: function for generalized linear model

# medium effect size
cohen.ES(test ="f2", size = c("medium")) # effect.size: 0.15

# large effect size
cohen.ES(test ="f2", size = c("large")) # effect.size: 0.35

# Considering 3 variables having statistical significant relationship with the sales performance
  # including digital spend, tv spend, and xmas

# normal case's sample size (medium effect size): 77 = round (v) + number of variables = round(72.70583) + 4
pwr.f2.test(u = 3, f2=0.15, sig.level = 0.05, power = 0.80)

# good case's sample size (large effect size): 35 = round(v) + number of variables = round(31.3129) + 4
pwr.f2.test(u = 3, f2=0.35, sig.level = 0.05, power = 0.80)

# Prepare training and testing datasets

set.seed(245)
df.train <- head(toy_sales_data,21)
df.test <- tail(toy_sales_data,3)

fit.naive.lm <- lm(sales ~.,data = df.train)

pred.naive.lm <- predict(fit.naive.lm, newdata = df.test)
y.test.naive.lm <- df.test$sales
class(y.test.naive.lm)
library(caret)
# rmse: 2977470
# mae: 2617622
rmse.naive.lm <- RMSE(pred.naive.lm,y.test.naive.lm)
mae.naive.lm <- MAE(pred.naive.lm,y.test.naive.lm)

# 2. Treat the outliners

# divide graph area in 3 columns
par(mfrow=c(1, 3))  

# Use boxplot to review the outliners: 1 outliner in the digital spending (spending $1944086.38 on March 2017)

# box plot for 'sales'
boxplot(toy_sales_data$sales, main="Sales", sub=paste("Outlier values: ", boxplot.stats(toy_sales_data$sales)$out))  

# box plot for 'tv_spend'
boxplot(toy_sales_data$tv_spend, main="tv_spend", sub=paste("Outlier values: ", boxplot.stats(toy_sales_data$tv_spend)$out))  

# box plot for 'digital_spend'
boxplot(toy_sales_data$digital_spend, main="Digital spend", sub=paste("Outlier values: ", boxplot.stats(toy_sales_data$digital_spend)$out))  


library(e1071)  # for skewness function

# density plot for 'sales'
plot(density(toy_sales_data$sales), main="Density Plot: Sales", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(toy_sales_data$sales), 2)))  
# add the collor for visualization purpose 
polygon(density(toy_sales_data$sales), col="red")

# density plot for 'tv_spend'
plot(density(toy_sales_data$tv_spend), main="Density Plot: Tv Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(toy_sales_data$tv_spend), 2)))  
# add the collor for visualization purpose 
polygon(density(toy_sales_data$tv_spend), col="blue")

# density plot for 'digital_spend'
plot(density(toy_sales_data$digital_spend), main="Density Plot: Digital Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(toy_sales_data$digital_spend), 2)))  # density plot for 'dist'
# add the collor for visualization purpose 
polygon(density(toy_sales_data$digital_spend), col="black")

# New plots

# box plot for 'sales'
boxplot(toy_sales_data$sales, main="Sales", sub=paste("Outlier values: ", boxplot.stats(toy_sales_data$sales)$out))  

# box plot for 'tv_spend'
boxplot(log(toy_sales_data$tv_spend), main="Transformed Tv_spend", sub=paste("Outlier values: ", boxplot.stats(log(toy_sales_data$tv_spend))$out))  

# box plot for 'digital_spend'
boxplot(log(toy_sales_data$digital_spend), main="Transformed Digital spend", sub=paste("Outlier values: ", boxplot.stats(log(toy_sales_data$digital_spend))$out))  


# density plot for 'sales'
plot(density(toy_sales_data$sales), main="Density Plot: Sales", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(toy_sales_data$sales), 2)))  
# add the collor for visualization purpose 
polygon(density(toy_sales_data$sales), col="red")

# density plot for 'tv_spend'
plot(density(log(toy_sales_data$tv_spend)), main="Density Plot: Transformed Tv Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(log(toy_sales_data$tv_spend)), 2)))  
# add the collor for visualization purpose 
polygon(density(log(toy_sales_data$tv_spend)), col="blue")

# density plot for 'digital_spend'
plot(density(log(toy_sales_data$digital_spend)), main="Density Plot: Transformed Digital Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(log(toy_sales_data$digital_spend)), 2)))  # density plot for 'dist'
# add the collor for visualization purpose 
polygon(density(log(toy_sales_data$digital_spend)), col="black")

# New lm model on the transformed variables

fit.naive.lm_new <- lm(sales ~ month +log(digital_spend)+ log(tv_spend)+ trend+ xmas,data = df.train)

pred.naive.lm_new <- predict(fit.naive.lm_new, newdata = df.test)

summary(fit.naive.lm_new)
y.test.naive.lm <- df.test$sales

rmse.naive.lm_new <- RMSE(pred.naive.lm_new,y.test.naive.lm)

# new rmse: 2502289
# 2359298
print(rmse.naive.lm_new)

mae.naive.lm_new <- MAE(pred.naive.lm_new,y.test.naive.lm)

# new mae: 2031825
# 1.60e6
print(mae.naive.lm_new)

# The rmse and mae have been improved

library(fpp3)

# ensuring that the "month" variable is recorded in the correct form 
toy_sales_data$month <- as.Date(toy_sales_data$month)

# transform the dataset into time series dataset by using the tsibble

df_toy_new <- toy_sales_data %>%
  mutate(Month = yearmonth(month)) %>%
  select(-c(month)) %>%
  as_tsibble(index = Month) 

ts_train_toy <- head(df_toy_new,21)
ts_test_toy <- tail(df_toy_new,3)

# par(mfrow=c(1, 1))
# dev.off() # reseting the system a bit for avoiding the crash on visualization 

# review the seasonality on sales
df_toy_new %>%
  gg_season(sales, labels = "both")

# review the seasonality on tv_spend
df_toy_new %>%
  gg_season(tv_spend, labels = "both")

# review the seasonality on tv_spend
df_toy_new %>%
  gg_season(digital_spend, labels = "both")

# review the correlation
df_toy_new %>%
  GGally::ggpairs(columns = 1:5)

fit_consMR <- ts_train_toy %>%
  model(tslm_naive = TSLM(sales ~ tv_spend + digital_spend +trend + xmas),
        tslm_transformed = TSLM(sales ~ log(tv_spend) + log(digital_spend) +trend + xmas),
        tslm_transform_trend_season = TSLM(sales ~ log(tv_spend) + log(digital_spend) + trend() +season() +xmas),
        tslm_transform_trend_season_noxmas = TSLM(sales ~ log(tv_spend) + log(digital_spend) + trend() +season()),
        tslm_transform_trend_trend_xmas = TSLM(sales ~ log(tv_spend) + log(digital_spend) + trend() + xmas))


print(report(fit_consMR))

fc_ts <- forecast(fit_consMR, new_data = ts_test_toy)
autoplot(fc_ts)

accuracy(fc_ts, ts_test_toy)

report(fit_consMR)
fit_consMR %>%
  gg_tsresiduals()
fc_ts %>%
  autoplot(ts_train_toy)+
  autolayer(ts_test_toy, sales, colour = "black")+
  labs(y = "Sales in dollar",
       title = "Applying TSLM method to forecast",
       subtitle = "Create by: Bach Lam")+
  guides(colour = guide_legend(title = "Forecast"))
  

fit_consMR_transform_trend_season <- ts_train_toy %>%
  model(tslm_transform_trend_season = TSLM(sales ~ log(tv_spend) + log(digital_spend) + trend() +season() +xmas))
report(fit_consMR_transform_trend_season)

fit_consMR_transform_trend_season_noxmas <- ts_train_toy %>%
  model(tslm_transform_trend_season_noxmas = TSLM(sales ~ log(tv_spend) + log(digital_spend) + trend() +season()))
report(fit_consMR_transform_trend_season_noxmas)

fit_consMR_transform_trend_season_noxmas %>%
  gg_tsresiduals()

report(fit_consMR_transform_trend_season_noxmas)
# null hypothesis: no serial correlation up to lag 12
  # p-value: 0.0000312 
  # cannot conclude the aimed result, considering to apply the dynamic regression
augment(fit_consMR_transform_trend_season_noxmas) %>%
  features(.innov, ljung_box, lag = 12, dof = 5)

# check the mean of innovation residuals
a <- augment(fit_consMR_transform_trend_season_noxmas)
# mean: 8.870686e-11
mean(a$.resid)

df_toy_new %>%
  left_join(residuals(fit_consMR_transform_trend_season_noxmas), by = "Month") %>%
  pivot_longer(sales:xmas,
               names_to = "regressor", values_to = "x") %>%
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "",
       title = "Scatterplots of residuals vs predictors",
       subtitle = "Model: fit_consMR_transform_trend_season_noxmas")

augment(fit_consMR_transform_trend_season_noxmas) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")+
  labs(title = "Scatterplots of residuals vs fitted values",
      subtitle = "Model: fit_consMR_transform_trend_season_noxmas")

# Try to optimize the performance by using lagged predictors 

fit_toy <- ts_train_toy %>%
  mutate(sales = c(NA,NA,NA,sales[4:21])) %>%
  # Estimate models
  model(
    lag0 = ARIMA(sales ~ pdq(d = 0) + 
                   log(tv_spend) + log(digital_spend) + trend() +season()),
    lag1 = ARIMA(sales ~ pdq(d = 0) +
                   log(tv_spend) + log(digital_spend) + trend() +season() + 
                   lag(log(digital_spend))+ lag(log(digital_spend))),
    lag2 = ARIMA(sales ~ pdq(d = 0) +
                   log(tv_spend) + log(digital_spend) + trend() +season() +
                   lag(log(digital_spend))+ lag(log(digital_spend)) +
                   lag(log(digital_spend),2)+ lag(log(digital_spend),2)),
    lag3 = ARIMA(sales ~ pdq(d = 0) +
                   log(tv_spend) + log(digital_spend) + trend() +season() +
                   lag(log(digital_spend))+ lag(log(digital_spend)) +
                   lag(log(digital_spend),2)+ lag(log(digital_spend),2)+
                   lag(log(digital_spend),3)+ lag(log(digital_spend),3))
  )


# the lag2 model shows the lowest AIC
glance(fit_toy)

# Train the model
fit_best_toy <- ts_train_toy %>%
  model(lag2 = ARIMA(sales ~ pdq(d = 0) +
                       log(tv_spend) + log(digital_spend) + trend() +season() +
                       lag(log(digital_spend))+ lag(log(digital_spend)) +
                       lag(log(digital_spend),2)+ lag(log(digital_spend),2)))

report(fit_best_toy)

# fit the model on the test dataset
fit.best.toy <- forecast(fit_best_toy, new_data = ts_test_toy)

# the accuracy is slower than the previous model
accuracy(fit.best.toy, ts_test_toy)

# plot the new model
fit_best_toy %>%
  forecast(ts_test_toy) %>%
  autoplot(df_toy_new) +
  labs(
    y = "Dollar",
    title = "Forecast sales of toy",
    subtitle = "Model: fit_best_toy"
  )

# plot the previous model
fit_consMR_transform_trend_season_noxmas %>%
  forecast(ts_test_toy) %>%
  autoplot(df_toy_new) +
  labs(
    y = "Dollar",
    title = "Forecast sales of toy",
    subtitle = "Model: fit_consMR_transform_trend_season_noxmas"
  )
