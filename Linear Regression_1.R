#Importing Libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(GGally)
library(correlation)
library(tibble)
library(reshape2)
library(purrr)
library(data.table)
# Importing & Merging the dataset
dt_lr <- read.csv("cubic_zirconia.csv")

######################INITIAL EXPLORATORY ANALYSIS#######################################################
nrow(dt_lr)
ncol(dt_lr)
summary(dt_lr) #Summary stats
rm(list=setdiff(ls(), "dt_lr")) #To clear environment OPTIONAL
glimpse(dt_lr) #View Structure
View(t(t((sapply(dt_lr,class,simplify = TRUE))))) #View column datatypes

#Convert Character to factor/numeric
dt_lr[,-which(names(dt_lr) %in% c('cut','color','clarity'))] <- lapply(dt_lr[,-which(names(dt_lr) %in% c('cut','color','clarity'))],as.numeric)
dt_lr[,which(names(dt_lr) %in% c('cut','color','clarity'))] <- lapply(dt_lr[,which(names(dt_lr) %in% c('cut','color','clarity'))],as.factor)
sapply(dt_lr,class)

##################univariate EDA####################

#Summary Stats
summary(dt_lr)

#PLOTS:NUMERIC VARS

#Scatter Plot
plt.sc <- dt_lr%>%keep(is.numeric)%>% 
  gather() %>% 
  ggplot(aes(value)) + 
  geom_density() + 
  facet_wrap(~key,scales = 'free')

dt_lr.num <- dt_lr%>%keep(is.numeric)

#Box Plot
plt.box <- dt_lr%>%keep(is.numeric)%>% 
  gather() %>% 
  ggplot(aes(y = value)) + 
  geom_boxplot() + 
  facet_wrap(~key,scales = 'free')

#QQ Plot
plt.qq <- dt_lr%>%keep(is.numeric)%>%
  gather()%>%
  ggplot(aes(sample = value)) +
  stat_qq() + 
  stat_qq_line()+
  facet_wrap(~key,scales = 'free')

#PLOTS: FACTORS
par(mfcol = c(2,2)) #Set grid for plot
par(mar = c(1,1,1,1)) #Set margins
dt_lr.fct <- dt_lr%>%keep(is.factor)
plt.fct <- apply(dt_lr.fct,2,function(x) barplot(prop.table(table(x)),xlab = x)) # Remove prop.table for frequency counts

##################Bivariate EDA####################
#NUMERIC V/S NUMERIC
plt.pair.num <- ggpairs(dt_lr.num)
plt.pair.num

#NUMERIC V/S FACTOR (Identify interesting combinations to plot)
ggplot(dt_lr,aes(`AirportName`,`Departure Delay`, color = `AircraftType`)) + geom_bar(stat = "identity") #Change column names
 
#FACTOR V/S FACTOR


######################DATA CLEANING & TRANSFORMATIONS####################################################
#Outlier removal
sapply(dt_lr.num,function(x) quantile(x,probs = c(0.95,0.99),na.rm = T)) #View 95%ile & 99%ile values
t(sapply(dt_lr.num,function(x) parameters::skewness(x))) #View Skewness
sapply(dt_lr.num,function(x) x[x>quantile(x,probs = 0.99,na.rm = T)] <- quantile(x,probs = 0.99,na.rm = T))

qt_rep <- function (y)
              {
                q_calc <- quantile(y,0.99,na.rm = T)
                y[y>q_calc] <- q_calc
                y
              }

outlier_remover <- function(x)
  {for (i in 1:ncol(x))
  {
    ifelse (parameters::skewness(x[,i])[1]>1.5,qt_rep(x[,i]),x[,i])
  }
  x
}

#Missing Value treatment
f.impute <- function (x) 
  {for (i in 1:ncol(x)) 
  {x[is.na(x[,i]),i] <- mean(x[,i],na.rm = T)} #replacement with mean
  x
  }

dt_lr.knn <- VIM::kNN(dt_lr.num,k = 4) #knn Imputation
dt_lr.num <- dt_lr.knn%>%keep(is.numeric)

#Final Dataset
dt_fin <- cbind(dt_lr.num,dt_lr.fct)
######################MODEL BUILDING#####################################################################
#Training-Test Split
sample <- sample.int(nrow(dt_fin),floor(0.8*nrow(dt_fin)))
train <- dt_fin[sample,]
test <- dt_fin[-sample,]

#Dummy Variable Coding
train <- mltools::one_hot(as.data.table(train))
test <- mltools::one_hot(as.data.table(test))

#Standard Linear Regression
lm_mdl <- lm(price~.,train)
summary(lm_mdl) #identify statistically insignificant predictors 

train <- train[,-c('X','z','cut_Very Good','color_J','clarity_VVS2')]
test <- test[,-c('X','z','cut_Very Good','color_J','clarity_VVS2')]

#Residual Analysis

#On Train Data
price.stdres <- rstandard(lm_mdl)
qqnorm(price.stdres,xlab = 'Normal Scores',ylab = 'Residuals',main = 'Residual Plot')
qqline(price.stdres)

#On Test Data
predicted_values <- predict(lm_mdl, newdata = test)
resid_test <- predicted_values - test$price
price.stdres_test <- scale(resid_test)
qq.plt_residual<- qqnorm(price.stdres_test,xlab = 'Normal Scores',ylab = 'Residuals',main = 'Residual Plot')
qqline(price.stdres_test)
