df<-read.csv("cleandata.csv", header = T)
View(df)
summary(df)

#change the targdol to 0:1
df$targdol[df$targdol!=0]=1
df$targdol

#drop the index
df <- subset(df, select = -c(X) )

#factor

df$consistencycategory <- relevel(df$consistencycategory, ref = "Ref")
is.factor(df$consistencycategory)
contrasts(df$consistencycategory)

#season to category
is.factor(df$recentseason)
df$recentseason<-as.character(df$recentseason)
df$recentseason[df$recentseason=='Spring']=1
df$recentseason[df$recentseason=='Fall']=0
df$recentseason<-as.factor(df$recentseason)
contrasts(df$recentseason)

#yearsince _pur and fallord
summary(df$falord)
summary(df$years_since_purchase)

#split the month
#check the type of date and year
library(lubridate)
class(df$datelp6)
df$datelp6<-as.Date(df$datelp6)
df$lpmonth<-month(df$datelp6)
df$lpmonth<-as.integer(df$lpmonth)

#split the train and test
train<-df[df$train==1,]
test<-df[df$train==0,]



#choose the predictors
train<-subset(train, select = -c(datead6,datelp6,lpuryear,train) )
head(train)

train$targdol<-as.factor(train$targdol)
train$recentseason<-as.factor(train$recentseason)

#choose the variables in test
test<-subset(test, select = -c(datead6,datelp6,lpuryear,train) )
head(test)

test$targdol<-as.factor(test$targdol)
test$recentseason<-as.factor(test$recentseason)

#logistic regression
model <- glm(targdol ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
#library(InformationValue)
#vif(model)
#exp(coef(model$finalModel))


library(pscl)
pR2(model)

#cross validation
library(caret)
library(e1071)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
#model
#fitted.results <- predict(model,newdata=test,type='response')

mod_fit <- train(targdol ~.,  data=train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred = predict(mod_fit, newdata=test)
#pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, test$targdol)
summary(pred)
summary(mod_fit)
class(mod_fit)



#prediction
#optCutOff
#decide a optimal cutoff
library(InformationValue)
optCutOff <- optimalCutoff(test$targdol,p)[1] 
optCutOff

#fitted.results2 = predict(mod_fit, newdata=test_balanced)
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > optCutOff,1,0)
misClasificError <- mean(fitted.results != test_balanced$targdol)
print(paste('Accuracy',1-misClasificError))


library(ROCR)
p <- predict(model, newdata=test, type="response")

pr <- prediction(p, test$targdol)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#balance the test data set

nrow(test[test$targdol==1,])
nrow(test[test$targdol==0,])
#4726 targdol =1

test_1<-test[test$targdol==1,]
test_0<-test[test$targdol==0,]

test_selected<-test_0[sample(nrow(test_0), nrow(test_1)), ]
head(test_selected)
dim(test_selected)

test_balanced<-rbind(test_1,test_selected)
head(test_balanced)
dim(test_balanced)


#predict again

library(pscl)
pR2(model)

#decide a optimal cutoff
library(InformationValue)
optCutOff <- optimalCutoff(test$targdol,p)[1] 
optCutOff

#prediction
fitted.results <- predict(model,newdata=test_balanced,type='response')
fitted.results <- ifelse(fitted.results > optCutOff,1,0)
misClasificError <- mean(fitted.results != test_balanced$targdol)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model, newdata=test_balanced, type="response")
pr <- prediction(p, test_balanced$targdol)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

plotROC(test_balanced$targdol, p)

#
test_balanced$targdol<-as.factor(test_balanced$targdol)
#p<-as.factor(p)
sensitivity(test_balanced$targdol, p, threshold = optCutOff)
specificity(test_balanced$targdol, p, threshold = optCutOff)
confusionMatrix(test_balanced$targdol, pr, threshold = optCutOff)
# The columns are actuals, while rows are predicteds.

# Use your model to make predictions, in this example newdata = training set, but replace with your test set    

# use caret and compute a confusion matrix
confusionMatrix(test_balanced$targdol, p, threshold = optCutOff)
