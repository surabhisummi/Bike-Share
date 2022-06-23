#load the data
rm(list=ls())
setwd("E:/2Sem_all_study_material/SDM/midTerms")
library(readxl)
mt<- read_excel("Bikeshare.xlsx", sheet="Data")
str(mt)

#feature engineering
colSums(is.na(mt))              #check for any missing value
mt$date <- as.Date(mt$date)
mt$year <- factor(format(mt$date, "%Y"))
mt$year <- relevel(mt$year, "2011")
mt$month <- factor(format(mt$date, "%m"))
mt$month <- relevel(mt$month, "jan")
levels(mt$month) <- list(jan="01", 
                         feb="02", 
                         march="03",
                         april="04",
                         may="05",
                         jun="06",
                         july="07",
                         aug="08",
                         sept="09",
                         oct="10",
                         nov="11",
                         dec="12")
mt$day <- factor(format(mt$date, "%a"))
mt$day <- relevel(mt$day, "Sun")
mt$hour <- factor(mt$hour)
mt$weekend <- ifelse(mt$day=="Sat" | mt$day =="Sun", 1,0)


#data visualization
hist(mt$count)
hist(log(mt$count))
hist(mt$registered)
hist(log(mt$registered))
hist(mt$casual)
hist(log(mt$casual))

#correlation test
library("PerformanceAnalytics")
mt_temp <- mt[, c(4:8)]
chart.Correlation(mt_temp)

boxplot(log(count)~year, data = mt)
boxplot(log(count)~month, data = mt)

#poisson model
p1 = glm(count ~ weather+fltemp+humidity+windspeed+hour+year+month+day+weekend,
         family = poisson(link = log), data = mt)
p2 = glm(registered ~ weather+fltemp+humidity+windspeed+hour+year+month+day+weekend,
         family = poisson(link = log), data = mt)
p3 = glm(casual ~ weather+fltemp+humidity+windspeed+hour+year+month+day+weekend,
         family = poisson(link = log), data = mt)


#overdispersion test
library(AER)
dispersiontest(p1)
dispersiontest(p2)
dispersiontest(p3)

#negative binomial model

library(MASS)
p4 = glm.nb(count ~ weather+fltemp+humidity+weekend+windspeed+hour+year+month+day, data = mt)
p5 = glm.nb(registered ~ weather+fltemp+humidity+weekend+windspeed+hour+year+month+day, data = mt)
p6 = glm.nb(casual ~ weather+fltemp+humidity+weekend+windspeed+hour+year+month+day, data = mt)

library(stargazer)
stargazer(p4,p5,p6, type = "text", single.row = TRUE)


