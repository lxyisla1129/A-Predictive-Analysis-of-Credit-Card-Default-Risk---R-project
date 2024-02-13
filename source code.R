###################### Import packages ######################
library(tidyverse)
library(mice)
library(VIM)
library(grid)
library(RColorBrewer)
library(arulesViz)
library(dplyr)

###################### Data pre-processing ######################
# Import data
UCI_Credit_Card <- read.csv("~/Downloads/UCI_Credit_Card.csv")

# View variable types
head(UCI_Credit_Card,3)
dim(UCI_Credit_Card)
summary(UCI_Credit_Card)
str(UCI_Credit_Card)

# Check whether data is missing
apply(UCI_Credit_Card,2,function(x) sum(is.na(x))) %>%
  enframe() %>%
  filter(value > 0) %>%
  mutate(ratio = value/5000) %>%
  arrange(desc(ratio))
mice::md.pattern(UCI_Credit_Card)
VIM::aggr(UCI_Credit_Card,col=c('steelblue','red'),
         numbers = TRUE,
         sorVars = TRUE,
         labels = names(data),
         ces.axis = 7,
         gap = 3,
         ylab = c("Data missing pattern histogram","Mode"))

# Decode "EDUCATION" as: graduate student and above as 1, college as 2, high school as 3, other as 4, and convert to a factor variable 
# Decode "MARRIAGE" as: married as 1, unmarried as 2, other as 3, and convert to a factor variable
data1 <- UCI_Credit_Card %>% mutate(EDUCATION=ifelse(EDUCATION==0 | EDUCATION==4 | EDUCATION==5 | EDUCATION==6,4,EDUCATION),MARRIAGE=ifelse(MARRIAGE==0,3,MARRIAGE))
data1$EDUCATION <- as.factor(data1$EDUCATION)
data1$MARRIAGE <- as.factor(data1$MARRIAGE)

# Convert "AGE" to a numeric variable
data1$AGE<- as.numeric(data1$AGE)

# Convert "PAY 1~PAY 6" to a factor variable
data1$PAY_1 <- as.factor(data1$PAY_1)
data1$PAY_2 <- as.factor(data1$PAY_2)
data1$PAY_3 <- as.factor(data1$PAY_3)
data1$PAY_4 <- as.factor(data1$PAY_4)
data1$PAY_5 <- as.factor(data1$PAY_5)
data1$PAY_6 <- as.factor(data1$PAY_6)

# Convert 'SEX' and 'default.payment.next.month' to factor variables
data1$SEX<- as.factor(data1$SEX)
data1$default.payment.next.month <- as.factor(data1$default.payment.next.month)

# View the changed variable type
str(data1)

# Data Visualization
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(Rmisc)
par(family='STHeiti')

# Check the distribution of factorial sex, marital status, education, and default
bar1<-ggplot(data1,)+geom_bar(width=1, aes(x=factor(1),fill=SEX))+coord_polar(theta="y") 
bar2<-ggplot(data1)+geom_bar(width=1, aes(x=factor(1),fill=MARRIAGE))+coord_polar(theta="y")
bar3<-ggplot(data1)+geom_bar(width=1, aes(x=factor(1),fill=EDUCATION))+coord_polar(theta="y")
bar4<-ggplot(data1)+geom_bar(width=1, aes(x=factor(1),fill=default.payment.next.month))+coord_polar(theta="y")
multiplot(bar1, bar2, bar3,bar4,cols=2)

# View the distribution of numeric ages
bar5<-ggplot(data1,aes(x=AGE))+geom_bar(fill ='lightseagreen')
bar5

# Check distribution of gender, marital status and education in different breach situations
bar6<-ggplot(data1)+geom_bar(aes(x=default.payment.next.month,fill=SEX),position="fill")
bar7<-ggplot(data1)+geom_bar(aes(x=default.payment.next.month,fill=MARRIAGE),position="fill")
bar8<-ggplot(data1)+geom_bar(aes(x=default.payment.next.month,fill=EDUCATION),position="fill")
multiplot(bar6, bar7,bar8,cols=2)

# Check age distribution under different default situations
box1<-ggplot(data1,aes(x=default.payment.next.month,y=AGE,fill=default.payment.next.month)) +geom_boxplot() + theme_bw() + labs(x ='default.payment.next.month', y = 'age') # Set the horizontal labels
box2<-ggplot(data1,aes(x=default.payment.next.month,y=LIMIT_BAL,fill=default.payment.next.month)) +geom_boxplot() + theme_bw() + labs(x ='default.payment.next.month', y = 'LIMIT_BAL') # Set the vertical labels
multiplot(box1,box2,cols=2)

###################### Use association rule algorithm to judge the degree of customer default risk ######################
# Association rule analysis must use data in transactions format, so as() is used to convert data.frame data into transactions objects
data2 <- as(data1, "transactions")  
dim(data2)
inspect(data2[1:5])
summary(size(data2))

# Generate a support function
itemFrequency(data2) %>% 
enframe() %>%
filter(value>0.4) %>%
arrange(desc(value))

# Visualize the data of variables with the top 15 support ratings
itemFrequencyPlot(data2,topN = 15,horiz=T) # topN = 15 indicates the variable with the top 15 support ratings

# View support for default separately
itemFreq <- itemFrequency(data2)
itemFreq %>% 
enframe() %>% 
filter(str_detect(name,"default.payment.next.month")) %>% 
head()

# The Apriori algorithm is used to analyze association rules, and the support level is set to 0.2 and the confidence level to 0.8
rules <- apriori(data2, parameter = list(support = 0.2, confidence = 0.8,maxlen = 10),appearance=list(rhs=c("default.payment.next.month=0"),default="lhs")) 
summary(rules)
inspectDT(sort(rules,by = "lift")) # Check the level of customer default

###################### Use random forest algorithm to model and predict default customers ######################
library(randomForest)
library(rsample)
library(pROC)
set.seed(100)
index <- initial_split(data1,prop =0.7)
train <- training(index)
test <- testing(index)
dim(train)
dim(test)
table(train$default.payment.next.month)

# ------------------------------------ test ------------------------------------
mtry_test <- randomForest(default.payment.next.month~.,data = train,importance = TRUE,proximity = TRUE)
mtry_test

# ------------------------------------------------------------------------------

# View error class condition
err <- as.numeric()
for (i in 2:(length(names(train))) - 1) {
  mtry_test <- randomForest(default.payment.next.month~., data = train, mtry = i)
  err <- append(err, mean(mtry_test$err.rate)) }
print(err)
mtry <- which.min(err)
mtry
ntree_fit <- randomForest(default.payment.next.month~., data = train, mtry = mtry, ntree = 1000) 
ntree_fit
plot(ntree_fit)

# Importance judgment of variables
importance(x=ntree_fit)
varImpPlot(ntree_fit, main = "variable importance")

# Make predictions on the test set
pre_ran <- predict(ntree_fit,newdata = test,type= "class" )

# Output confusion matrix
rf.cf <- caret::confusionMatrix(as.factor(pre_ran),as.factor(test$default.payment.next.month))
rf.cf

# Evaluate the model and calculate the ROC and AUC
rf.test2 <- predict(ntree_fit,newdata = test,type = "prob")
head(rf.test2)
plot(margin(ntree_fit,test$default.payment.next.month),main="The probability of an observation is judged to be correct")
rroc.rf <- roc(test$default.payment.next.month,as.numeric(rf.test2))
rroc.rf

# Draw the ROC curve
ran_roc <- roc(test$default.payment.next.month,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("black", "black"), max.auc.polygon=TRUE,auc.polygon.col="lightseagreen", print.thres=TRUE,main='ROC curve')
