library(dslabs)
library(dplyr)
library(ggplot2)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(gridExtra)
library(Lahman)
library(dslabs)
library(AER)
library(tseries)
library(dynlm)
library(stargazer)
library(forecast)
library(mFilter)
library(data.table)
library(caTools)
library(viridis)
library(hrbrthemes)
library(scales)

library(tikzDevice)
require(tikzDevice)



ds_theme_set()




#create a data set

countries <- rep(x = c("Pakistan", "India", "Bangladesh", "USA", "South Africa", "France"))

pakistangroups <- rep(x = c("National", "Sindh", "KPK", "Punjab", "Baluchistan"))


#condition <- rep(x = c("Top1PercentWealthShare", "Top1PercentIncomeShare"))



Top1PercentShareWealth <- rep(x = c(26, 32, 24.6, 35, 54.9, 26.8))




Top1PercentShareIncome <- rep(x = c(16.7, 21.7, 16.2, 19, 19.3, 8.9))


data <- data.frame(countries, Top1PercentShareWealth, Top1PercentShareIncome)

#wealth_income <- paste(data$Top1PercentShareWealth, data$Top1PercentShareIncome)



Top10PercentShareWealthPakistan <- rep(x = c(60, 55, 60, 60, 65)) #national, Sindh, KPK,
#Punjab and Baluchistan.

Top5PercentShareWealthPakistan <- rep(x = c(50, 40, 50, 50, 53)) #national, Sindh, KPK,
#Punjab and Baluchistan.


Top1PercentShareWealthPakistan <- rep(x = c(28, 20, 22, 28, 30)) #national, Sindh, KPK,
#Punjab and Baluchistan.



Bottom60PercentShareWealthPakistan <- rep(x = c(10, 10, 10, 10, 10)) #national, Sindh, KPK,
#Punjab and Baluchistan.


datapakistan <- data.frame(pakistangroups, Top10PercentShareWealthPakistan,
                           Top5PercentShareWealthPakistan,
                           Top1PercentShareWealthPakistan,
                           Bottom60PercentShareWealthPakistan)

View(datapakistan)

library(reshape)

datacombined <- melt(data, id.vars=c('countries'), var='combined')

View(datacombined)


datapakistancombined <- melt(datapakistan, id.vars=c('pakistangroups'), var='combined')

View(datapakistancombined)


#Barcharts

tikz(file = 'InequalityG1.tex', width=5, height=4)




graphinequality <- ggplot(datacombined, aes(x=countries, y = value,fill = combined)) +
  geom_bar(stat = "identity", position="dodge") +
  ggtitle(" Wealth and Income Inequality in 2021") +
  labs(y = "Wealth/Income Share of Top 1 Percent", 
       x = "Countries") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 60),
                     breaks = seq(0, 60, 10))


print(graphinequality)

dev.off()





tikz(file = 'WealthInequalityRegionalPakistan.tex', width=7, height=4)


# Below the top
#ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
#  geom_col() +
#  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")


graphinequalitypak <- ggplot(datapakistancombined, aes(x=pakistangroups, y = value,
    fill = combined)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = value),
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  ) +
  ggtitle(" Wealth Inequality Across Pakistan in 2014") +
  labs(y = "Wealth Share of Groups", 
       x = "National/Provinces") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1)) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 70),
                   breaks = seq(0, 70, 10))


print(graphinequalitypak)

dev.off()







