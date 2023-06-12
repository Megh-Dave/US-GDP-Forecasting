library(forecast)

# Set working directory for locating files.
setwd("/~/Documents/Time Series")

# Create Data Frame
project.data <- read.csv("~/Documents/Time Series/GDP_new.csv")

## Create time series data set.
gdp.ts <- ts(project.data$GDP, 
               start = c(2003, 1), end = c(2022, 4), freq = 4)
gdp.ts
head(gdp.ts)
tail(gdp.ts)
nrow(project.data)

#Create a data plot using historical data
plot(gdp.ts, 
          xlab = "Time", ylab = "GDP Values ", xaxt = "n",
          ylim = c(10000,27000), main = "Quarterly Values of US GDP.", 
          col = "blue", bty = "l", lwd = 2)
     axis(1, at = seq(2003, 2022, 1), labels = format(seq(2003, 2022, 1)))
     
#Using Acf() function to identify possible time series components
autocor <- Acf(gdp.ts,lag.max = 12,main="Autocorrelation for US GDP")

library(zoo)
# partition the data
nValid <-16
nTrain <- length(gdp.ts) - nValid
train.ts <- window(gdp.ts,start=c(2003,1),end=c(2003,nTrain))
valid.ts <- window(gdp.ts,start=c(2003,nTrain+1),
                   end=c(2003,nTrain+nValid))
train.ts
valid.ts
# Create trailing MA with window widths (number of periods) of k = 3, 8, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_2 <- rollmean(train.ts, k = 2, align = "right")
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_5<- rollmean(train.ts, k = 5, align = "right")
ma.trailing_8<- rollmean(train.ts, k = 8, align = "right")
ma.trailing_10<- rollmean(train.ts, k = 10, align = "right")
ma.trailing_12<- rollmean(train.ts, k = 12, align = "right")

#Create forecast for the validation data for the window widths of k = 3,8,12.
ma.trail_2.pred <- forecast(ma.trailing_2, h = nValid, level = 0)
ma.trail_2.pred
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred
ma.trail_5.pred <- forecast(ma.trailing_5, h = nValid, level = 0)
ma.trail_5.pred
ma.trail_8.pred <- forecast(ma.trailing_8, h = nValid, level = 0)
ma.trail_8.pred
ma.trail_10.pred <- forecast(ma.trailing_10, h = nValid, level = 0)
ma.trail_10.pred
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred
#Use accuracy() function to identify common measures.
#Use round() function round to accuracy measures to three decimal digits.
round(accuracy(ma.trail_2.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_5.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_8.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_10.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)

#Fit a regression model with linear trend and seasonality 
#or training partition.
trend.seas <- tslm(train.ts ~ trend+season)
summary(trend.seas)
#Use forecast() to predict monthly sales in the validation period
trend.seas.pred<-forecast(trend.seas,h=nValid,level=0)
trend.seas.pred
# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res
#Apply trailing MA for residuals with window width k=4 
#for training partition
ma.trail.res <- rollmean(trend.seas.res, k=5,align='right')
ma.trail.res
ma.trail.res.pred <- forecast(ma.trail.res,h=nValid,level=0)
ma.trail.res.pred

# Plot original data and regression forecast for training and 
# validation partitions.
plot(gdp.ts, 
     xlab = "Time", ylab = "GDP values", ylim = c(10000,30000), 
     bty = "l", xlim = c(2003, 2024), xaxt = "n",
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(2003, 2024, 1), labels = format(seq(2003, 2024, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(2004,24000, legend = c("US GDP data", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2018, 2018), c(0, 30000))
lines(c(2022, 2022), c(0, 30000))
text(2010, 28000, "Training")
text(2020, 28000, "Validation")
text(2023.2, 28000, "Future")
arrows(2003.1, 27000, 2017.9, 27000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2018.1, 27000, 2021.9, 27000, code = 3, length = 0.1,   lwd = 1, angle = 30)
arrows(2022.1, 27000, 2024.9, 27000, code = 3, length = 0.1,lwd = 1, angle = 30)

#Q 3--c.
# Develop two-level forecast for validation period by combining regression 
# forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level
# Create a table for validation period: validation data,regression,
#forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Validation Data", "Regression Forecast", 
                     "MA Forecast", "Combined Forecast")
valid.df
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail.res.pred$mean, valid.ts), 3)
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

# Fit a regression model with linear trend and 
# seasonality for entire data set.
tot.trend.seas <- tslm(gdp.ts ~ trend  + season)
summary(tot.trend.seas)
# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 8, level = 0)
tot.trend.seas.pred
# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res
# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res
# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 8, level = 0)
tot.ma.trail.res.pred
# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level
# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(tot.trend.seas.pred$fitted, gdp.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, gdp.ts), 3)
round(accuracy(tot.ma.trail.res.pred$fitted, gdp.ts), 3)

ses.orig <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.orig

# Use forecast() function to make predictions using this SES model 
# validation period (nValid). 
# Show predictions in tabular format.
ses.orig.pred <- forecast(ses.orig, h = nValid, level = 0)
ses.orig.pred


## HOLT'S EXPONENTIAL SMOOTHING WITH PARTITIONED DATA.

# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN <- ets(train.ts, model = "AAN", alpha = 0.1, beta = 0.1)
h.AAN

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
h.AAN.pred <- forecast(h.AAN, h = nValid, level = 0)
h.AAN.pred

# Holt's model with optimal smoothing parameters.
# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN.opt <- ets(train.ts, model = "AAN")
h.AAN.opt

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
h.AAN.opt.pred <- forecast(h.AAN.opt, h = nValid, level = 0)
h.AAN.opt.pred

# Plot Holt's model predictions with optimal smoothing parameters.
plot(h.AAN.opt.pred$mean, 
     xlab = "Time", ylab = "GDP (in 000s)", ylim = c(11000, 27000), 
     bty = "l", xlim = c(2003, 2025.25), xaxt = "n",
     main = "Holt's Additive Model with Optimal Smoothing Parameters", 
     col = "blue", lty = 2, lwd = 2, ) 
axis(1, at = seq(2003, 2025, 1), labels = format(seq(2003, 2025, 1)))
lines(h.AAN.opt.pred$fitted, col = "blue", lwd = 2)
lines(gdp.ts)
legend(2004,25000, legend = c("GDP", 
                              "Holt's Additive Model for Training Partition",
                              "Holt's Additive Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 27000))
lines(c(2023, 2023), c(0, 27000))
text(2011, 18000, "Training")
text(2020.5, 18000, "Validation")
text(2024.2, 18000, "Future")
arrows(2003, 20000, 2018.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 20000, 2022.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 20000, 2024.3, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA. 
## OPTIMAL PARAMETERS FOR ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "AAA", i.e., additive error(A), 
# additive trend (A), & additive seasonality (A). 
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred

# Plot HW predictions for HW additive model (AAA) optimal smoothing parameters.
plot(hw.AAA.pred$mean, 
     xlab = "Time", ylab = "GDP (in 000s)", ylim = c(11000, 27000), 
     bty = "l", xlim = c(2003, 2025.25), xaxt = "n",
     main = "Holt-Winter's Additive Model with Optimal Smoothing Parameters", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2003, 2025, 1), labels = format(seq(2003, 2025, 1)))
lines(hw.AAA.pred$fitted, col = "blue", lwd = 2)
lines(gdp.ts)
legend(2004,25000, 
       legend = c("GDP", 
                  "Holt-Winter's Additive Model for Training Partition",
                  "Holt-Winter's Additive Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 27000))
lines(c(2023, 2023), c(0, 27000))
text(2011, 18000, "Training")
text(2020.5, 18000, "Validation")
text(2024.2, 18000, "Future")
arrows(2003, 20000, 2018.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 20000, 2022.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 20000, 2024.3, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # Model appears to be (M, Ad, N), with alpha = 0.9999, beta = 0.4895
# and phi = 0.9556.

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "GDP (in 000s)", ylim = c(11000, 27000), 
     bty = "l", xlim = c(2003, 2025.25), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2003, 2025, 1), labels = format(seq(2003, 2025, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(gdp.ts)
legend(2004,25000, 
       legend = c("GDP", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 27000))
lines(c(2023, 2023), c(0, 27000))
text(2011, 18000, "Training")
text(2020.5, 18000, "Validation")
text(2024.2, 18000, "Future")
arrows(2003, 20000, 2018.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 20000, 2022.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 20000, 2024.3, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## COMPARE ACCURACY OF THREE MODELS: SES WITH ALPHA = 0.2, HW
## ADDITIVE MODEL WITH OPTIMAL PARAMETERS, AND HW MODEL WITH
## AUTOMATED SELECTION OF MODEL OPTIONS.
round(accuracy(ses.orig.pred$mean, valid.ts), 3)
round(accuracy(hw.AAA.pred$mean, valid.ts), 3)
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 8 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full GDP data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(gdp.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, A, N), with alpha = 0.8486, and beta = 0.0447

# Use forecast() function to make predictions using this HW model for
# 8 quarters into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 8 , level = 0)
HW.ZZZ.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "GDP (in 000s)", ylim = c(11000, 30000), 
     bty = "l", xlim = c(2003, 2025.25), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2003, 2025, 1), labels = format(seq(2003, 2025, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(gdp.ts)
legend(2003,25000, 
       legend = c("GDP", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(0, 30000))
text(2013, 25000, "Data Set")
text(2024.2, 25000, "Future")
arrows(2003, 22000, 2022.9, 22000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 22000, 2025.3, 22000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use ets() function with alternative model = "AAA". 
# Use forecast() function to make predictions using this HW model for
# 8 quarter into the future.
HW.AAA <- ets(gdp.ts, model = "AAA")
HW.AAA # Model appears to be (A, A, A), with alpha = 0.8857, 
# beta = 0.0596, and gamma = 1e-04.

# Use forecast() function to make predictions using this HW model for
# 8 quarters into the future.
HW.AAA.pred <- forecast(HW.AAA, h = 8 , level = 0)
HW.AAA.pred

# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, gdp.ts), 3)
round(accuracy(HW.AAA.pred$fitted, gdp.ts), 3)
round(accuracy((naive(gdp.ts))$fitted, gdp.ts), 3)
round(accuracy((snaive(gdp.ts))$fitted, gdp.ts), 3)


## (1) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
train.lin <- tslm(train.ts ~ trend)

# See summary of quadratic trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

## (2) QUADRATIC TREND MODEL.
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

## (3) SEASONALITY MODEL.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

## (4) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

## (5) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
  forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred


##  
# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)



##  
# FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY, 
# WITH QUADRATIC TREND,
# WITH LINEAR TREND AND SEASONALITY FOR ENTIRE DATASET. 
# FORECASTDATA, AND MEASURE ACCURACY.

## (1) QUADRATIC TREND AND SEASONALITY MODEL
# Use tslm() function to create quadratic trend and seasonality model.
quad.trend.season <- tslm(gdp.ts ~ trend + I(trend^2)+ season)

# See summary of linear trend equation and associated parameters.
summary(quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
quad.trend.season.pred <- forecast(quad.trend.season, h = 8, level = 0)
quad.trend.season.pred

## (2) QUADRATIC TREND MODEL.
# Use tslm() function to create quadratic (polynomial) trend model.
quad.trend <- tslm(gdp.ts ~ trend + I(trend^2))

# See summary of quadratic trend equation and associated parameters.
summary(quad.trend)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
quad.trend.pred <- forecast(quad.trend, h = 8, level = 0)
quad.trend.pred

## (3) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonality model.
lin.trend.season <- tslm(gdp.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 8, level = 0)
lin.trend.season.pred



##  
## COMPARE ACCURACY MEASURES OF REGRESSION FORECAST 
## WITH QUANDRATIC TREND AND SEASONALITY,  LINEAR TREND AND
## SEASONALITY, QUADRATIC TREND FOR ENTIRE DATA SET WITH ACCURACY MEASURES 
## OF NAIVE FORECAST AND SEASONAL NAIVE 
## FORECAST FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures
# for naive model, seasonal naive, and regression model 
# with quadratic trend and seasonality.
round(accuracy(quad.trend.season.pred$fitted, gdp.ts),3)
round(accuracy(quad.trend.pred$fitted, gdp.ts),3)
round(accuracy(lin.trend.season.pred$fitted, gdp.ts),3)
round(accuracy((naive(gdp.ts))$fitted, gdp.ts), 3)
round(accuracy((snaive(gdp.ts))$fitted, gdp.ts), 3)

## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Accuracy measure for Auto ARIMA model.

round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

# Auto ARIMA forecast for 12 future periods.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "GDP (in 000s)", 
     ylim = c(11000, 27000), xaxt = "n", 
     bty = "l", xlim = c(2003, 2025.25), 
     main = "Auto ARIMA Model", lwd = 2, lty = 5) 
axis(1, at = seq(2003, 2025, 1), labels = format(seq(2003, 2025, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2003,25000, legend = c("US GDP Time Series", 
                              "Auto ARIMA Forecast for Training Period",
                              "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2019, 2019), c(0, 27000))
lines(c(2023, 2023), c(0, 27000))
text(2011, 18000, "Training")
text(2020.5, 18000, "Validation")
text(2024.2, 18000, "Future")
arrows(2003, 20000, 2018.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 20000, 2022.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 20000, 2024.3, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(gdp.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


# Plot historical data, predictions for historical data, and seasonal 

plot(gdp.ts, 
     xlab = "Time", ylab = "GDP (in 000s)", 
     ylim = c(11000, 30000), xaxt = "n", 
     bty = "l", xlim = c(2003, 2025.25), 
     main = "Auto ARIMA Model for Entire Set", lwd = 2,lty = 5) 
axis(1, at = seq(2003, 2025, 1), labels = format(seq(2003, 2025, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "black", lwd = 2, lty = 1)
legend(2003,25000, legend = c("US GDP Time Series", 
                              "Auto ARIMA Forecast for Entire Data Set"),
       # "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2023, 2023), c(0, 30000))
text(2011, 18000, "Data Set")
text(2024.2, 18000, "Future")
arrows(2003, 20000, 2018.9, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 20000, 2024.3, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:

round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, GDP.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, gdp.ts), 3)
round(accuracy(auto.arima.pred$fitted, gdp.ts), 3)
round(accuracy(quad.trend.season.pred$fitted, gdp.ts),3)
round(accuracy((naive(gdp.ts))$fitted, gdp.ts), 3)
round(accuracy((snaive(gdp.ts))$fitted, gdp.ts), 3)
round(accuracy(res.ar1.pred$fitted, gdp.ts), 3)

