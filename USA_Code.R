
library(readxl)
library(ggplot2)
library(dbplyr)
library(tseries)
library(readr)
library(fUnitRoots)
library(gridExtra)
library(urca)
library(vars)
library(zoo)

USA_GDP <- read_excel("USA GDP.xls")
colnames(USA_GDP) <- c("Date", "GDP")
USA_CPI <- read_excel("USA CPI.xls")
colnames(USA_CPI) <- c("Date", "CPI")
Female_Unemp_USA <- read_excel("Female Unemp US.xls")
colnames(Female_Unemp_USA) <- c("Date", "Female_Unemp_USA")
Male_Unemp_USA <- read_excel("Male Unemp US.xls")
colnames(Male_Unemp_USA) <- c("Date", "Male_Unemp_USA")


# USA GDP
ggplot(USA_GDP, aes(x = Date, y = GDP)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "USA GDP Over Time")

adf_test_result <- adf.test(USA_GDP$GDP)
print(adf_test_result)


# USA CPI

# Differencing to remove trend
USA_CPI$CPI <- c(NA, diff(USA_CPI$CPI))
mean_unemp_diff2 <- mean(USA_CPI$CPI, na.rm = TRUE)

ggplot(USA_CPI, aes(x = Date, y = CPI)) +
  geom_line() +
  geom_hline(yintercept = mean_unemp_diff2, color = "red", linetype = "dashed") +
  labs(title = "USA CPI Over Time (Differenced)")

CPI <- na.omit(USA_CPI$CPI)
adf_test_result2 <- adf.test(CPI)
print(adf_test_result2)


# Female_Unemp_USA

# Differencing to remove trend
Female_Unemp_USA$Female_Unemp_USA <- c(NA, diff(Female_Unemp_USA$Female_Unemp_USA))
mean_unemp_diff <- mean(Female_Unemp_USA$Female_Unemp_USA, na.rm = TRUE)
Female_Unemp_USA$Female_Unemp_USA[is.na(Female_Unemp_USA$Female_Unemp_USA)] <- mean_unemp_diff

ggplot(Female_Unemp_USA, aes(x = Date, y = Female_Unemp_USA)) +
  geom_line() +
  geom_hline(yintercept = mean_unemp_diff, color = "red", linetype = "dashed") +
  labs(title = "USA Female Unemployment Over Time (Differenced)")

adf_test_result <- adf.test(Female_Unemp_USA$Female_Unemp_USA)
print(adf_test_result)


# Male_Unemp_USA

# Differencing to remove trend
Male_Unemp_USA$Male_Unemp_USA<- c(NA, diff(Male_Unemp_USA$Male_Unemp_USA))
mean_unemp_diff1 <- mean(Male_Unemp_USA$Male_Unemp_USA, na.rm = TRUE)
Male_Unemp_USA$Male_Unemp_USA[is.na(Male_Unemp_USA$Male_Unemp_USA)] <- mean_unemp_diff1

ggplot(Male_Unemp_USA, aes(x = Date, y = Male_Unemp_USA)) +
  geom_line() +
  geom_hline(yintercept = mean_unemp_diff1, color = "red", linetype = "dashed") +
  labs(title = "USA Male Unemployment Over Time (Differenced)")

adf_test_result1 <- adf.test(Male_Unemp_USA$Male_Unemp_USA)
print(adf_test_result1)

# Both unemployment data are nonstationary which had to be fixed


# Merge USA_GDP and USA_CPI based on 'Date'
data <- merge(USA_GDP, USA_CPI, by = "Date")
data <- merge(data, Female_Unemp_USA, by = "Date")
data <- merge(data, Male_Unemp_USA, by = "Date")


###############################################################################
#Successfully Combined My datasets for USA#
##############################################################################

#female

#ACF and PACF
ac <- acf(Female_Unemp_USA$Female_Unemp_USA, lag.max = 25, main = "ACF for Unemployed Females in USA")
pac <- pacf(Female_Unemp_USA$Female_Unemp_USA, lag.max = 25, main = "PACF for Unemployed Females in USA")

#AIC and BIC
calculate_aic_bic <- function(p, q, data) {
  arma_model <- arima(Female_Unemp_USA$Female_Unemp_USA, order = c(p, 0, q))
  aic_value <- AIC(arma_model)
  bic_value <- BIC(arma_model)
  return(data.frame(Model = paste("ARMA(", p, ", ", q, ")", sep = ""), AIC = aic_value, BIC = bic_value))
}

aic_bic_values <- data.frame(Model = character(0), AIC = numeric(0), BIC = numeric(0))

p_values <- 0:6
q_values <- 0:6

for (p in p_values) {
  for (q in q_values) {
    if (p + q <= 6) {  
      aic_bic_values <- rbind(aic_bic_values, calculate_aic_bic(p, q, Female_Unemp_USA))
    }
  }
}

best_aic_model <- aic_bic_values[which.min(aic_bic_values$AIC), ]
best_bic_model <- aic_bic_values[which.min(aic_bic_values$BIC), ]

worst_aic_model <- aic_bic_values[which.max(aic_bic_values$AIC), ]
worst_bic_model <- aic_bic_values[which.max(aic_bic_values$BIC), ]

print("AIC and BIC Table:")
print(aic_bic_values)
print(paste("Best AIC Model:", best_aic_model$Model))
print(paste("Best BIC Model:", best_bic_model$Model))
print(paste("Worst AIC Model:", worst_aic_model$Model))
print(paste("Worst BIC Model:", worst_bic_model$Model))

best_arma_model <- arima(Female_Unemp_USA$Female_Unemp_USA, order = c(1, 0, 5))
worst_arma_model <- arima(Female_Unemp_USA$Female_Unemp_USA, order = c(3, 0, 0))

best_residuals <- residuals(best_arma_model)
worst_residuals <- residuals(worst_arma_model)

par(mfrow = c(2, 1))
par(mar = c(5, 5, 5, 5))
acf(best_residuals, main = paste("ACF of Residual (Best ARMA model):", best_bic_model$Model))
acf(worst_residuals, main = paste("ACF of Residual (Worst ARMA model):", worst_bic_model$Model))
pacf(best_residuals, main = paste("PACF of Residual (Best ARMA model):", best_bic_model$Model))
pacf(worst_residuals, main = paste("PACF of Residual (Worst ARMA model):", worst_bic_model$Model))


#male

#ACF and PACF

ac1 <- acf(Male_Unemp_USA$Male_Unemp_USA, lag.max = 25, main = "ACF for Unemployed Males in USA")
pac1 <- pacf(Male_Unemp_USA$Male_Unemp_USA, lag.max = 25, main = "PACF for Unemployed Males in USA")

#AIC and BIC
calculate_aic_bic1 <- function(p, q, data) {
  arma_model1 <- arima(Male_Unemp_USA$Male_Unemp_USA, order = c(p, 0, q))
  aic_value1 <- AIC(arma_model1)
  bic_value1 <- BIC(arma_model1)
  return(data.frame(Model1 = paste("ARMA(", p, ", ", q, ")", sep = ""), AIC = aic_value1, BIC = bic_value1))
}

aic_bic_values1 <- data.frame(Model1 = character(0), AIC = numeric(0), BIC = numeric(0))

p_values1 <- 0:6
q_values1 <- 0:6

for (p in p_values1) {
  for (q in q_values1) {
    if (p + q <= 6) {  
      aic_bic_values1 <- rbind(aic_bic_values1, calculate_aic_bic1(p, q, Male_Unemp_USA))
    }
  }
}

best_aic_model1 <- aic_bic_values1[which.min(aic_bic_values1$AIC), ]
best_bic_model1 <- aic_bic_values1[which.min(aic_bic_values1$BIC), ]

worst_aic_model1 <- aic_bic_values1[which.max(aic_bic_values1$AIC), ]
worst_bic_model1 <- aic_bic_values1[which.max(aic_bic_values1$BIC), ]

print("AIC and BIC Table:")
print(aic_bic_values1)
print(paste("Best AIC Model:", best_aic_model1$Model1))
print(paste("Best BIC Model:", best_bic_model1$Model1))
print(paste("Worst AIC Model:", worst_aic_model1$Model1))
print(paste("Worst BIC Model:", worst_bic_model1$Model1))

best_arma_model1 <- arima(Male_Unemp_USA$Male_Unemp_USA, order = c(0, 0, 4))
worst_arma_model1 <- arima(Male_Unemp_USA$Male_Unemp_USA, order = c(2, 0, 1))

best_residuals1 <- residuals(best_arma_model1)
worst_residuals1 <- residuals(worst_arma_model1)

par(mfrow = c(2, 1))
par(mar = c(5, 5, 5, 5))
acf(best_residuals1, main = paste("ACF of Residual (Best ARMA model):", best_bic_model1$Model1))
acf(worst_residuals1, main = paste("ACF of Residual (Worst ARMA model):", worst_bic_model1$Model1))
pacf(best_residuals1, main = paste("PACF of Residual (Best ARMA model):", best_bic_model1$Model1))
pacf(worst_residuals1, main = paste("PACF of Residual (Worst ARMA model):", worst_bic_model1$Model1))




######################################################################################
#Forecasting
####################################################################################

#female

total_observations <- length(Female_Unemp_USA$Female_Unemp_USA)
training_ratio <- 0.8  
training_count <- round(total_observations * training_ratio)
training_data <- window(Female_Unemp_USA$Female_Unemp_USA, end = training_count)
validation_data <- window(Female_Unemp_USA$Female_Unemp_USA, start = training_count + 1)
length(training_data)
length(validation_data)
arma_model <- arima(training_data, order = c(1, 0, 5))
forecast_length <- length(validation_data)
forecast_values <- predict(arma_model, n.ahead = forecast_length)$pred

#b
n <- length(Female_Unemp_USA$Female_Unemp_USA)
forecast_values <- numeric(n)
for (i in n) {
  forecast <- predict(arma_model, n.ahead = 1)$pred
  forecast_values[i] <- forecast
}

#c
dynamic_forecasts <- numeric(n)
for (i in (forecast_length + 1):n) {
  updated_arma_model <- arima(Female_Unemp_USA$Female_Unemp_USA[1:i], order = c(1, 0, 5))
  forecast <- predict(updated_arma_model, n.ahead = 1)$pred
  dynamic_forecasts[i] <- forecast
}

#d
df <- data.frame(
  Date = Female_Unemp_USA$Date,
  Original_Data = Female_Unemp_USA$Female_Unemp_USA,
  Dynamic_Forecast = c(rep(NA, forecast_length), dynamic_forecasts[(forecast_length + 1):n]),
  Static_Forecast = c(rep(NA, forecast_length), forecast_values[(forecast_length + 1):n])
)


# Impute missing values with mean
df$Original_Data[is.na(df$Original_Data)] <- mean(df$Original_Data, na.rm = TRUE)
df$Dynamic_Forecast[is.na(df$Dynamic_Forecast)] <- mean(df$Dynamic_Forecast, na.rm = TRUE)
df$Static_Forecast[is.na(df$Static_Forecast)] <- mean(df$Static_Forecast, na.rm = TRUE)

# Plot the data
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Original_Data, color = "Original Data")) +
  geom_line(aes(y = Dynamic_Forecast, color = "Dynamic Forecast")) +
  geom_line(aes(y = Static_Forecast, color = "Static Forecast")) +
  labs(title = "ARMA(1,5) Dynamic and Static Forecasts - Female", x = "Date", y = "Value") +
  scale_color_manual(values = c("Original Data" = "black", "Dynamic Forecast" = "blue", "Static Forecast" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())


#male

total_observations1 <- length(Male_Unemp_USA$Male_Unemp_USA)
training_ratio1 <- 0.8  
training_count1 <- round(total_observations1 * training_ratio1)
training_data1 <- window(Male_Unemp_USA$Male_Unemp_USA, end = training_count1)
validation_data1 <- window(Male_Unemp_USA$Male_Unemp_USA, start = training_count1 + 1)
length(training_data1)
length(validation_data1)
arma_model1 <- arima(training_data1, order = c(1, 0, 4))
forecast_length1 <- length(validation_data1)
forecast_values1 <- predict(arma_model1, n.ahead = forecast_length1)$pred

#b
n1 <- length(Male_Unemp_USA$Male_Unemp_USA)
forecast_values1 <- numeric(n)
for (i in n) {
  forecast1 <- predict(arma_model1, n.ahead = 4)$pred
  forecast_values1[i] <- forecast1
}

#c
dynamic_forecasts1 <- numeric(n)
for (i in (forecast_length + 4):n) {
  updated_arma_model1 <- arima(Male_Unemp_USA$Male_Unemp_USA[1:i], order = c(0, 0, 4))
  forecast1 <- predict(updated_arma_model1, n.ahead = 4)$pred
  dynamic_forecasts1[i] <- forecast1
}

#d
df1 <- data.frame(
  Date1 = Male_Unemp_USA$Date,
  Original_Data1 = Male_Unemp_USA$Male_Unemp_USA,
  Dynamic_Forecast1 = c(rep(NA, forecast_length1), dynamic_forecasts1[(forecast_length1 + 1):n]),
  Static_Forecast1 = c(rep(NA, forecast_length1), forecast_values1[(forecast_length1 + 1):n])
)

ggplot(df1, aes(x = Date1)) +
  geom_line(aes(y = Original_Data1, color = "Original Data")) +
  geom_line(aes(y = Dynamic_Forecast1, color = "Dynamic Forecast")) +
  geom_line(aes(y = Static_Forecast1, color = "Static Forecast")) +
  labs(title = "ARMA(1,4) Dynamic and Static Forecasts - Male", x = "Date", y = "Value") +
  scale_color_manual(values = c("Original Data" = "black", "Dynamic Forecast" = "blue", "Static Forecast" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())


# Impute missing values with mean
df1$Original_Data1[is.na(df1$Original_Data1)] <- mean(df1$Original_Data1, na.rm = TRUE)
df1$Dynamic_Forecast1[is.na(df1$Dynamic_Forecast1)] <- mean(df1$Dynamic_Forecast1, na.rm = TRUE)
df1$Static_Forecast1[is.na(df1$Static_Forecast1)] <- mean(df1$Static_Forecast1, na.rm = TRUE)

# Plot the data
ggplot(df1, aes(x = Date1)) +
  geom_line(aes(y = Original_Data1, color = "Original Data")) +
  geom_line(aes(y = Dynamic_Forecast1, color = "Dynamic Forecast")) +
  geom_line(aes(y = Static_Forecast1, color = "Static Forecast")) +
  labs(title = "ARMA(0,4) Dynamic and Static Forecasts", x = "Date", y = "Value") +
  scale_color_manual(values = c("Original Data" = "black", "Dynamic Forecast" = "blue", "Static Forecast" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())


##############################################
#Forecasting for 4 periods ahead
##############################################

#female

df$Date <- as.Date(df$Date)

ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Original_Data, color = "Original Data")) +
  geom_line(aes(y = Dynamic_Forecast, color = "Dynamic Forecast")) +
  geom_line(aes(y = Static_Forecast, color = "Static Forecast")) +
  labs(title = "ARMA(1,5) Dynamic and Static Forecasts - Female USA", x = "Date", y = "Value") +
  scale_color_manual(values = c("Original Data" = "black", "Dynamic Forecast" = "blue", "Static Forecast" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_labels = "%y-%b", date_breaks = "3 months", limits = as.Date(c("2019-01-01", "2023-10-01")))



#male

df1$Date <- as.Date(df1$Date)

ggplot(df1, aes(x = Date1)) +
  geom_line(aes(y = Original_Data1, color = "Original Data")) +
  geom_line(aes(y = Dynamic_Forecast1, color = "Dynamic Forecast")) +
  geom_line(aes(y = Static_Forecast1, color = "Static Forecast")) +
  labs(title = "ARMA(0,4) Dynamic and Static Forecasts - Male USA", x = "Date", y = "Value") +
  scale_color_manual(values = c("Original Data" = "black", "Dynamic Forecast" = "blue", "Static Forecast" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_datetime(date_labels = "%y-%b", date_breaks = "3 months", limits = as.POSIXct(c("2019-01-01", "2023-10-01")))



######################################################################
#VAR Models
######################################################################

library(openxlsx)
# 
# file_path <- "C:/Users/mmced/OneDrive - University of Texas at San Antonio/Desktop/UTSA Fall 23/Time Series/Poster Presentation/USAmerged_data.xlsx"
# write.xlsx(data, file_path, rowNames = FALSE)
# cat("Excel file saved at:", file_path, "\n")


Data <- read_excel("USAmerged_data.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
View(Data)

# gdp = y, cpi = pi, female unemployment = unemp1, male unemployment = unemp2

names(Data)[2] = "y"
names(Data)[3] = "pi"                 
names(Data)[4] = "unemp1"
names(Data)[5] = "unemp2"

TS_Data <- ts(Data[, -1], start = c(1949, 1), frequency = 4)
View(TS_Data)

Model1 <- VAR(TS_Data[, c("y", "pi", "unemp1", "unemp2")], p = 4, type = "const")
summary(Model1)

amat <- diag(4) 
amat[2,1] <- NA 
amat[3,1] <- NA 
amat[3,2] <- NA 
amat[4,1] <- NA 
amat[4,2] <- NA 
amat[4,3] <- NA

svar_model <- SVAR(Model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))

SVARy <- irf(svar_model, impulse = "y", response = "y") 
SVARy
plot(SVARy)

SVARypi <- irf(svar_model, impulse = "y", response = "pi") 
SVARypi
plot(SVARypi)

SVARyunemp1 <- irf(svar_model, impulse = "y", response = "unemp1") 
SVARyunemp1
plot(SVARyunemp1)

SVARyunemp2 <- irf(svar_model, impulse = "y", response = "unemp2") 
SVARyunemp2
plot(SVARyunemp2)



SVARpi <- irf(svar_model, impulse = "pi", response = "pi") 
SVARpi
plot(SVARpi)

SVARpiy <- irf(svar_model, impulse = "pi", response = "y") 
SVARpiy
plot(SVARpiy)

SVARpiunemp1 <- irf(svar_model, impulse = "pi", response = "unemp1") 
SVARpiunemp1
plot(SVARpiunemp1)

SVARpiunemp2 <- irf(svar_model, impulse = "pi", response = "unemp2") 
SVARpiunemp2
plot(SVARpiunemp2)


SVARunemp1 <- irf(svar_model, impulse = "unemp1", response = "unemp1") 
SVARunemp1
plot(SVARunemp1)

SVARunemp1y <- irf(svar_model, impulse = "unemp1", response = "y") 
SVARunemp1y
plot(SVARunemp1y)

SVARunemp1pi <- irf(svar_model, impulse = "unemp1", response = "pi") 
SVARunemp1pi
plot(SVARunemp1pi)

SVARunemp12 <- irf(svar_model, impulse = "unemp1", response = "unemp2") 
SVARunemp12
plot(SVARunemp12)


SVARunemp2 <- irf(svar_model, impulse = "unemp2", response = "unemp2") 
SVARunemp2
plot(SVARunemp2)

SVARunemp2y <- irf(svar_model, impulse = "unemp2", response = "y") 
SVARunemp2y
plot(SVARunemp2y)

SVARunemp2pi <- irf(svar_model, impulse = "unemp2", response = "pi") 
SVARunemp2pi
plot(SVARunemp2pi)

SVARunemp21 <- irf(svar_model, impulse = "unemp2", response = "unemp1") 
SVARunemp21
plot(SVARunemp21)











