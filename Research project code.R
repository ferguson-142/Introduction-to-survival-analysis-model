# Set working directory

setwd("D:/Research Project (Essay Phase)/Survival Analysis")


# Read in the data

install.packages("readr")
install.packages("lubridate")
install.packages("survival")
install.packages("survminer")
install.packages("xtable")

library(readr)
library(survival)
library(survminer)
library(lubridate)
library(xtable)

survdata<-read.csv("Data_Ferguson.csv")
xtable(head(survdata))
View(survdata)
print.xtable(summary(survdata))
print(xtable(summary(survdata)), size="\\tiny")
str(survdata)
table<-table(survdata$gender,survdata$status)
xtable(table)

#converting status to numeric

survdata$status[survdata$status=="Alive"] <- 1
survdata$status[survdata$status=="Death"] <- 2



#create the censoring variable(right censoring)

survdata$censored[survdata$status== 2] <- 1 #not censored or experienced death
survdata$censored[survdata$status== 1] <- 0 #0 if an individual was censored


# computing age

survdata$Age <- (survdata$yearlastobserved - survdata$yearofbirth) 
summary(survdata)

#Formating dates

survdata %>% 
  mutate(
    daymonthyearobserved = as.Date(dmyobserved, format = "%Y-%m-%d"), 
    daymonthyearobserved = as.Date(dmyobserved, format = "%Y-%m-%d") 
  )



#Calculating survival times - lubridate

new_df<- survdata %>% 
       mutate(
         time_df = 
           as.duration(dmyentry  %--% dmyobserved) / dyears(1)*365.25
       )
View(new_df)
summary(new_df)

# Extracting month from the date in the dataset
new_df$monthobserved<- month(as.POSIXlt(new_df$dmyobserved,format="%Y-%m-%d"))
View(new_df)

#inspection of the duration variable distribution

hist(new_df$time_df,main="Duration variable distribution", col="green",xlab = "Days", 
     ylab = "Frequency",las=1)

#only keep rows where time_df is less than 0 
new_df2 <- subset(new_df, time_df > 0 & Age > 0)
summary(new_df2)


# inspection of the time_df variable distribution
par(mfrow=c(1,2))
hist(new_df$time_df,main="", col="green",xlab = "Days", 
     ylab = "Frequency",las=1)
hist(new_df2$time_df,main="", col="green",xlab = "Days", 
     ylab = "Frequency",las=1)


hist(subset(new_df2,status==1)$time_df,main="", col="green",xlab = "Days", 
     ylab = "Frequency",las=1)

hist(subset(new_df2,status==2)$time_df,main="", col="green",xlab = "Days", 
     ylab = "Frequency",las=1)



#Estimating survival curves with the Kaplan-Meier model

km_fit <- survfit(Surv(time_df,censored)~ 1,
                   data=new_df2,type="kaplan-meier")

print(km_fit)

#summarize the KM results using default time intervals

#and create a life table

summary(km_fit)

#summarize the KM results using default time intervals

#and create a life table

summary(km_fit,times = c(150,900*(1:1225)))

#Next to do in the essay - start here.


#Kaplan-Meier plot - base R

plot(survfit(Surv(time_df, censored) ~ 1, data = new_df2), 
     xlab = "Days", 
     ylab = "Overall survival probability")

#Kaplan-Meier plot - ggsurvplot

ggsurvplot(km_fit, data = new_df2, risk.table = TRUE,conf.int = TRUE,
           ggtheme = theme_minimal())


survdiff(Surv(time_df, censored) ~ gender, data = new_df2)

#Estimating survival curves with the Kaplan-Meier model2 with categorical variable

km_fit2 <- survfit(Surv(time_df,censored)~ gender,
                  data=new_df2,type="kaplan-meier")

print(km_fit2)


#summarize the KM results using default time intervals

#and create a life table

summary(km_fit2)

#summarize the KM results using default time intervals

#and create a life table

summary(km_fit2,times = c(150,1200*(1:1225)))


#Kaplan-Meier plot - ggsurvplot

ggsurvplot(km_fit2, data = new_df2, surv.median.line = "hv", risk.table = TRUE, conf.int = TRUE, pval=TRUE,pval.method = TRUE,
           tables.theme = theme_cleantable(),
           palette = "jco", ggtheme = theme_minimal())

ggsurvplot(km_fit2, data = new_df2,
           surv.median.line = "hv", # Add medians survival
           pval = TRUE,             # Add p-value and intervals
           
           conf.int = TRUE,        # Add the 95% confidence band
           risk.table = TRUE,      # Add risk table
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           palette = "jco",
           ggtheme = theme_bw())

#Estimate Cox proportional hazard (PH) model - Cox regression model

cox_reg<- coxph(Surv(time_df,censored)~ gender,
                 data=new_df2) 
summary(cox_reg)

#Estimate Cox proportional hazard (PH) model - Cox regression model

cox_reg1<- coxph(Surv(time_df,censored)~ gender + Age,
                 data=new_df2)

summary(cox_reg1)


#Estimate Cox proportional hazard (PH) model - Cox regression model

cox_reg2<- coxph(Surv(time_df,censored)~ gender + Age + monthobserved ,
                 data=new_df2)

summary(cox_reg2)

# Nested comparision

anova(cox_reg,cox_reg1,cox_reg2)

