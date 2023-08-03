# Load packages----
library(dlm)
library(dplyr)
library(ggplot2)
library(plotly)

# The data----
df = read.csv('goldHistoricalData_1yr.csv')
head(df)

df$Date = as.Date(as.character(df$Date), format = "%m/%d/%Y")
df = df %>% arrange(Date)
#df

weekdays(as.Date("2023-06-23"))
weekdays(as.Date("2023-06-26"))

# Plotting----
ggplot(df, aes(x = Date)) + 
    geom_line(aes(y = High), col = 'blue') +
    geom_line(aes(y = Low), col = 'orangered') +
    geom_ribbon(aes(ymax = High, ymin = Low), alpha = 0.3) +
    ylab('USD') + xlab('') + theme_bw()

# Modelling and forecast high price----
# model building
model_trend_high = dlmModPoly(order = 2, dV = 1, dW = c(2, 1), 
                              m0 = c(1920, 0))
model_season = dlmModSeas(4)
model_high = model_trend_high + model_season

# filtering
filtered_high = dlmFilter(df$High, model_high)

# distribution for one-step ahead forecast
model_high$FF %*% matrix(filtered_high$a[250,]) #mean
tail(filtered_high$f, 1) #mean provided by dlmFilter output
model_high$FF %*% dlmSvd2var(filtered_high$U.R, filtered_high$D.R)[[250]] %*% t(model_high$FF) + model_high$V #variance

forecast_high = dlmForecast(filtered_high)
forecast_high$f #forecasted value
forecast_high$Q #variance provided by dlmForecast output

# Modelling and forecast low prices----
model_trend_low = dlmModPoly(order = 2, dV = 1, dW = c(2, 1), 
                             m0 = c(1910, 0))
model_season = dlmModSeas(4)
model_low = model_trend_low + model_season

filtered_low = dlmFilter(df$Low, model_low)

#distribution for one-step ahead forecast
model_low$FF %*% matrix(filtered_low$a[250,])
tail(filtered_low$f, 1)
model_low$FF %*% dlmSvd2var(filtered_low$U.R, filtered_low$D.R)[[250]] %*% t(model_low$FF) + model_low$V

forecast_low = dlmForecast(filtered_low)
forecast_low$f
forecast_low$Q

# Plot the high price forecast----
fig <- plot_ly(df, x = ~Date) %>%
    add_markers(y = ~High, name = 'data', 
                marker = list(color = 'gray')) %>%
    add_lines(y = filtered_high$m[-1,1], name = 'filtered level',
              line = list(color = 'magenta', width = 3)) %>%
    add_markers(x = as.Date("2023-06-26"),
                y = forecast_high$f[1,1],
                name = 'one-step forecast value',
                marker = list(color = 'cornflowerblue', size = 8))
fig

# Plot the low price forecast----
fig <- plot_ly(df, x = ~Date) %>%
    add_markers(y = ~Low, name = 'data', 
                marker = list(color = 'gray')) %>%
    add_lines(y = filtered_low$m[-1,1], name = 'filtered level',
              line = list(color = 'magenta', width = 3)) %>%
    add_markers(x = as.Date("2023-06-26"), 
                y = forecast_low$f[1,1],
                name = 'one-step forecast value',
                marker = list(color = 'cornflowerblue', size = 8))
fig

# Statistical check on the models----
#plot the standardized one-step ahead forecast error
plot(df$Date, residuals(filtered_high, sd = F), type = 'l', 
     xlab = 'Date', ylab = 'standardized forecast error')
plot(df$Date, residuals(filtered_low, sd = F), type = 'l', 
     xlab = 'Date', ylab = 'standardized forecast error')

#statistical test of normality of one-step ahead forecast error
qqnorm(residuals(filtered_high, sd = F))
qqnorm(residuals(filtered_low, sd = F))

# Check with the real data----
df1 = read.csv('goldHistoricalData_5yr.csv')
head(df1, 10)