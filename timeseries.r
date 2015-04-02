library(WDI)
gdp <- WDI(country=c('US','CA','GB','CN'), indicator=c('NY.GDP.PCAP.CD', 'NY.GDP.MKTP.CD'), start=1960,end=2011)
gdp
names(gdp) <- c('iso2c','country','year','PerCapGDP','GDP')
ggplot(gdp,aes(year, GDP, color=country,linetype=country))+geom_line()
us <-gdp$PerCapGDP[gdp$country =='United States']
us
summary(us)
us <-ts(us,start=min(gdp$year),end=max(gdp$year))
us
plot(us)
acf(us)
pacf(us)
plot(us)
library(forecast)
usBest <- auto.arima(us)
usBest
predict(usBest,n.ahead=5)
usFore <-forecast(usBest,h=5)
usFore
plot(usFore)
usNew <- WDI(country='US', indicator='NY.GDP.PCAP.CD',2012,2013)
usNew
