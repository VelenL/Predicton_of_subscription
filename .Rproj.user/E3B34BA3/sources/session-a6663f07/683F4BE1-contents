#-----------------------------------
# Install/Call Packages
#-----------------------------------
library(ggplot2)
library('datarium')
library('lattice')
install.packages("caret")
library("caret")
library("rpart") 
library("rpart.plot")
install.packages("e1071")
library("e1071") 
# -----------------------------------
# Import data
#-----------------------------------
bank<-read.csv("bankmarketing.csv", header=TRUE, stringsAsFactors = T)
#---------------------------------
# Data structure/cleaning
#---------------------------------
str(bank)
bank$age <- as.numeric(bank$age)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays[bank$pdays == '999'] <- "-1"
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
summary(bank)

# Check for number of missing values
sum(is.na(bank))
# Data is clear but too much unknown
bank[bank == 'unknown'] <- NA
bank <- na.omit(bank)
bank <- droplevels(bank)
summary(bank)
str(bank)
# check outliers and normality
attach(bank)
par(mfrow=c(2,5))
hist(campaign, main = "Histogram Campaign", xlab = "Campaign", col = "red3")
boxplot(campaign, main = "Campaign", col = "red3")
hist(duration, main = "Histogram Duration", xlab = "Duration", col = "red3")
boxplot(duration, main ="Duration", col = "red3")
hist(pdays, main = "Histogram Pday", xlab = "Pdays", col = "red3")
boxplot(pdays, main ="Pdays", col = "red3")
hist(previous, main = "Histogram Previous", xlab = "Previous", col = "red3")
boxplot(previous, main = "Previous", col = "red3")
hist(age, main = "Histogram Age", xlab = "Age", col = "red3")
boxplot(age, main = "Age", col = "red3")

logcampaign <- log(campaign)
logduration <- log(duration)
logpdays <- log(pdays)
logprevious <- log(previous)

par(mfrow=c(2,2))
hist(logcampaign, main = "Histogram log Campaign", xlab = "log Campaign", col = "red3")
boxplot(logcampaign, main ="log Campaign", col = "red3")
hist(logduration, main = "Histogram log Duration", xlab = "log Duration", col = "red3")
boxplot(logduration, main ="log Duration", col = "red3")
hist(logpdays, main = "Histogram log Pdays", xlab = "log Pdays", col = "red3")
boxplot(logpdays, main ="log Pdays", col = "red3")
#Nan produced so its pdays variable is not going to be transformed
hist(logprevious, main = "Histogram log Previous", xlab = "log Previous", col = "red3")
boxplot(logprevious, main ="log Previous", col = "red3")
# Irregular values are produced so previous variable is not going to be transformed

#Check link between variables
splom(~bank[c(1,11,12,13,14,21)], groups=NULL, axis.line.tck=0,axis.text.alpha=0)


#-------------------------
# Model estimation
#-------------------------
# The duration highly affects the output target (e.g., if duration=0 then y='no'). 
# Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. 
# Thus, this input should only be included for benchmark purposes and should be discarded 
# if the intention is to have a realistic predictive model.
set.seed(1)
row.number <- sample(1:nrow(bank), 0.8*nrow(bank))
train=bank[row.number,]
test=bank[-row.number,]
dim(train)
dim(test)

mod.1 <- glm(y~.-duration, data=train, binomial(link="logit"))
summary(mod.1)

# There are too much insignificant values, so delete them do another model

mod.2 <- glm(y~ job + duration+ contact + month + day_of_week + campaign + pdays + poutcome , data=train, binomial(link="logit"))
summary(mod.2)

## We can continue with these predictors right now 

#-----------------
# Model accuracy
#----------------
varImp(mod.2)
# pdays,contact, month are the three most important values

# Test model
prediction_test <- predict(mod.2, newdata = test, type = "response")
prop.table(table(test$y, prediction_test > 0.5))

# Almost 86% of the client who didn't subscribe a term deposit have been correctly predicted
# but only 2% of the client who did subscribe a term deposit have been correctly predicted


# Classification
decision_tree2 <- rpart(y ~ job + marital + education + default + housing + loan + contact + poutcome + month + day_of_week + campaign + pdays + duration, method="class", data=bank, control=rpart.control(minsplit=1), parms=list(split="information"))
summary(decision_tree2)
rpart.plot(decision_tree2, type=2, extra=1)


#Naive Bayes
bank$duration2<-ifelse(bank$duration<250,"<250",">250")
training_data <- as.data.frame(bank[1:dim(bank)[1]-1,]) 
training_data
test_data <- as.data.frame(bank[dim(bank)[1],])
test_data

model2 <- naiveBayes(y ~ job + marital + education + default + housing + loan + contact + poutcome + month + day_of_week + campaign + pdays + previous + duration2, training_data)
model2

# Predict with test_data
results2 <- predict(model2,newdata=test_data)
results2

# Now we will have a new customer
new_customer<-data.frame(age=35, job="entrepreneur", marital="married", education ="high.school", default= "no", housing="yes", loan="yes", contact="cellular", month="may", duration=523, campaign=2, pdays=2, previous=2, poutcome="failure", day_of_week="mon")

# Let's predict
new_customer2<- predict(decision_tree2,newdata=new_customer,type="class")
new_customer2 <- new_customer
new_customer2$duration2<-ifelse(new_customer$duration<250,"<250",">250")
results4 <- predict(model2, new_customer2)
results4
