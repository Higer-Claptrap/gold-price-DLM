library(dlm)
library(dplyr)
library(ggplot2)
library(plotly)

df = read.csv('goldHistoricalData_1yr.csv')
df$Date = as.Date(as.character(df$Date), format = "%m/%d/%Y")
df = df %>% arrange(Date)
#df

#make timeplot
ggplot(df, aes(x = Date)) + 
    geom_line(aes(y = High), col = 'blue') +
    geom_line(aes(y = Low), col = 'orangered') +
    geom_ribbon(aes(ymax = High, ymin = Low), alpha = 0.3) +
    ylab('USD') + xlab('') + theme_bw()

#estimate using log-likelihood
build <- function(parm) {
    dlmModPoly(order = 2, dV = exp(parm[1]), 
               dW = c(exp(parm[2]), 1)) +
        dlmModSeas(4, dV = exp(parm[3]), 
                   dW = c(exp(parm[4]), exp(parm[5]), 1))
}
fit_high <- dlmMLE(df$High, rep(0, 5), build)
fit_low <- dlmMLE(df$Low, rep(0, 5), build)

fit_high$convergence
fit_low$convergence

#build model
model_high <- build(fit_high$par)
model_high

model_low <- build(fit_low$par)
model_low

#filtering and forecasting
filtered_high = dlmFilter(df$High, model_high)
forecast_high = dlmForecast(filtered_high)
forecast_high$f

filtered_low = dlmFilter(df$Low, model_low)
forecast_low = dlmForecast(filtered_low)
forecast_low$f

#visualize forecasts
fig <- plot_ly(df, x = ~Date) %>%
    add_markers(y = ~High, name = 'data', 
                marker = list(color = 'gray')) %>%
    add_lines(y = filtered_high$m[-1,1], name = 'filtered level',
              line = list(color = 'magenta')) %>%
    add_markers(x = as.Date("2023-06-26"),
                y = forecast_high$f,
                name = 'one-step forecast value',
                marker = list(color = 'cornflowerblue'))
fig

fig <- plot_ly(df, x = ~Date) %>%
    add_markers(y = ~Low, name = 'data', 
                marker = list(color = 'gray')) %>%
    add_lines(y = filtered_low$m[-1,1], name = 'filtered level',
              line = list(color = 'magenta')) %>%
    add_markers(x = as.Date("2023-06-26"), 
                y = forecast_low$f,
                name = 'one-step forecast value',
                marker = list(color = 'cornflowerblue'))
fig

#the real price
df1 = read.csv('goldHistoricalData_5yr.csv')
head(df1, 10)