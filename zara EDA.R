library(tidyverse)
library(ggplot2)
library(dplyr)
library(psych)
library(forecast)

# Data Import & Structure 
data <- read.csv('C:\\Datasets\\Zara Sales\\zara.csv', sep = ";", quote = "\"" ,stringsAsFactors = FALSE)

head(data)

str(data)
# missing value check
colSums(is.na(data))

dim(data)

describe(data)
# remove unnecessary variables

data <- data[,-c(8:9, 11, 14)]
data2 <- data # data for data visualization

# variable type transformation 

data$Product.Position <- as.factor(data$Product.Position)
data$Product.Category <- as.factor(data$Product.Category)
data$brand <- as.factor(data$brand)
data$terms <- as.factor(data$terms)
data$section <- as.factor(data$section)
data$name <- as.factor(data$name)
data$currency <- as.factor(data$currency)

# data transformation

# Promotion : No - > 0 , Yes -> 1 
data$Promotion <- ifelse(data$Promotion == 'No', 0 ,
                        ifelse(data$Promotion == 'Yes',1,2))

# Seasonal : No - > 0 , Yes -> 1 
data$Seasonal <- ifelse(data$Seasonal == 'No',0,
                       ifelse(data$Seasonal=='Yes',1,2))

# section : MAN - > 0 , WOMEN -> 1 , 
data$section <- ifelse(data$section == 'MAN',0,
                       ifelse(data$section=='WOMEN',1,2))

head(data)
str(data)
summary(data)

#2 Data visualisation

cols = c('Product.Position','Promotion','Seasonal','section')

for (i in cols){
    print(ggplot(data2, aes(x=data2[,i],fill = data2[,i])) + geom_bar() + ggtitle(paste(i,'count'))+ xlab(i) + theme_bw() + theme(legend.position = 'none'))
}
for(i in cols){
    print(ggplot(data2, aes(x=data2[,i], y=price, fill= data2[,i])) + geom_boxplot() + xlab(i) +ggtitle(paste(i,'vs price'))+ theme_bw() + theme(legend.position = 'none') )
}
for(i in cols){
    print(ggplot(data2, aes(x=data2[,i], y=Sales.Volume, fill= data2[,i])) + geom_boxplot() + xlab(i) +ggtitle(paste(i,'vs Sales.Volume'))+ theme_bw() + theme(legend.position = 'none') )
}
ggplot(data2, aes(x=price)) + geom_density() + ggtitle('The density of commodity prices') + xlab('price')
# a lot of items under $100
ggplot(data2, aes(x=price, y= Sales.Volume)) + geom_smooth(se=F)
# relation between price and sales
# average price by group
data2_promotion <- data2 %>% group_by(Promotion) %>% summarize(N = n(), avg_price = round(mean(price,na.rm=T)))
data2_promotion
ggplot(data2_promotion, aes(x=Promotion, y= avg_price,fill = Promotion)) + geom_col()
# average price of productions on promotion
data2_Seasonal <- data2 %>% group_by(Seasonal) %>% summarize(N = n(), avg_price = round(mean(price,na.rm=T)))
data2_Seasonal
ggplot(data2_Seasonal, aes(x=Seasonal,y=avg_price, fill = Seasonal)) + geom_col()
# not much difference in average price
# Average calculation according to the number of people allocated
data2_section <- data2 %>% group_by(section) %>%  summarize(N = n(), avg_price = round(mean(price,na.rm=T))) 
data2_section

# If pick 30 people and average them

data2_section30 <- data2 %>% group_by(section) %>% sample_n(size = 30)%>% summarize(N = n(), avg_price = round(mean(price,na.rm=T))) 
data2_section30
ggplot(data2_section30, aes(x=section,y=avg_price, fill = section)) + geom_col()
# average price of male products is higher
data2_promotion2 <- data2 %>% group_by(Promotion) %>% summarize(N = n(), avg_Sales.Volume = round(mean(Sales.Volume,na.rm=T)))
data2_promotion2
ggplot(data2_promotion2,aes(x=Promotion, y= avg_Sales.Volume,fill = Promotion)) + geom_col()
# not a lot of difference in average sales volume
data2_Seasonal2 <- data2 %>% group_by(Seasonal) %>% summarize(N = n(), avg_Sales.Volume = round(mean(Sales.Volume,na.rm=T)))
data2_Seasonal2
ggplot(data2_Seasonal2, aes(x=Seasonal,y=avg_Sales.Volume, fill = Seasonal)) + geom_col()
# not a lot of difference in average sales volume
# Average calculation according to the number of people allocated
data2_section2 <- data2 %>% group_by(section) %>%  summarize(N = n(), avg_Sales.Volume = round(mean(Sales.Volume,na.rm=T))) 
data2_section2
ggplot(data2_section2, aes(x=section,y=avg_Sales.Volume, fill = section)) + geom_col()
# the average sales volume for men and women are similar, but shows women buy more because the number of women is smaller

# 3 Price & sales.volume prediction

# modeling
# linear regression
md_lr <- lm(price ~Promotion + Seasonal+ section + Sales.Volume ,data=data)
summary(md_lr)
step(md_lr,direction = "backward")
# select variables
md_lr <- lm(price ~ Promotion + section , data = data)
summary(md_lr)
# regression equation: 86.014 + Promotion * 12.298 + section * -20.819
plot(md_lr)
pred <- 86.014 + data$Promotion * 12.298 + data$section * -20.819
accuracy(data$price,pred)
md_lr2 <- lm(Sales.Volume ~  price + Promotion + Seasonal+ section  ,data=data)
summary(md_lr2)
# The regression model that predicts Sales.Volume is not statistically significant
