rm(list=ls())
library(dplyr)
library(rio)
library(ggplot2)
trans.df = import('CreditCardTransactions.xlsx')
colnames(trans.df) = tolower(make.names(colnames(trans.df)))
str(trans.df)
trans.df %>% filter_all(any_vars(is.na(.))) #Looking at rows that have NAs
reduced.trans.df = na.omit(trans.df) #Omitting 4 Na rows
str(reduced.trans.df)
ggplot(data = reduced.trans.df, aes(x = transcount, y = transamount)) + 
  geom_point() +
  labs(x = "Transaction Count", y = "Transaction Amount") #Plotting the available numeric variables 

#Models for transamount
#Model 1
amount1.out =  lm(transamount ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data = reduced.trans.df)
summary(amount1.out) #Does not follow linearity and equality of variance
#Model 2
amount2.out =  lm(log(transamount) ~ transcount  + revolvingindicator + spendcategory+ wealthtag*cardtype, data = reduced.trans.df)
summary(amount2.out)

#Checking LINE assumptions
par(mfrow = c(2, 2))
#L
plot(amount2.out, 1, lwd=2)

#N
hist(amount2.out$residuals, col = 'red')
qqnorm(amount2.out$residuals)
qqline(amount2.out$residuals, col = 'red', lwd=2)

#Equality of Variances
plot(reduced.trans.df$transamount,rstandard(amount2.out),pch=19,main="Trans amount Residual Plot", ylim=c(-4,4))
abline(0,0,col="red",lwd=3)
#Checking I
car::durbinWatsonTest(amount2.out)

#Trying to correct for EOV
#WLS
wt = 1 / (amountlog.out$fitted.values^2)

wls = lm(log(transamount) ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data = reduced.trans.df, weights = wt)
summary(wls)

wls2 = wls = lm(transamount ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data = reduced.trans.df, weights = wt)
summary(wls2)

#FGLS
ols = lm(transamount ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data = reduced.trans.df)
temp <- lm(abs(ols$residuals) ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data=reduced.trans.df)
wt <- 1/temp$fitted^2
fgls1 <- lm(log(transamount) ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data=reduced.trans.df, weights=wt)
temp <- lm(abs(ols$residuals) ~ ols$fitted)
wt <- 1/temp$fitted^2
fgls2 <- lm(log(transamount) ~ transcount + wealthtag + cardtype + revolvingindicator + spendcategory, data=reduced.trans.df, weights=wt)
#None of these methods could correct for EOV.

#Stargazer for amount models.
library(stargazer)
stargazer(amount1.out, amount2.out, title="Models for TransAmount", align=TRUE, out = "amounts.html", single.row = TRUE)
#___________________________________________________________________________________________________________________________________________________________#

#Lets look at transcount now.

count1.out = lm(transcount ~ transamount + spendcategory + cardtype + revolvingindicator + wealthtag, data = reduced.trans.df)
summary(count1.out)

count2.out = lm(log(transcount) ~ transamount + I(transamount *transamount) + spendcategory   + revolvingindicator + wealthtag * cardtype, data = reduced.trans.df)
summary(count2.out)

par(mfrow = c(2, 2))
#L
plot(count2.out, 1, lwd=2)

#N
hist(count2.out$residuals, col = 'red')
qqnorm(count2.out$residuals)
qqline(count2.out$residuals, col = 'red', lwd=2)

#Equality of Variances
plot(reduced.trans.df$transcount,rstandard(amount2.out),pch=19,main="TransCount Residual Plot", ylim=c(-4,4))
abline(0,0,col="red",lwd=3)

#Checking I
car::durbinWatsonTest(count2.out)

#stargazer
library(stargazer)
stargazer(count1.out, count2.out, title="Models for TransCount", align=TRUE, out = "counts.html", single.row = TRUE)

