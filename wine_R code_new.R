rm(list=ls())
setwd("C:/deepak/study material/edwisor ,dataanlytics/business case study/CaseStudyOnWines")
getwd()
wine_red=read.csv("red_wine.csv")
wine_red$type=with(wine_red,"red")     #(add type to new column)
wine_white=read.csv("white_wine.csv")
wine_white$type=with(wine_red,"white") #(add type to new column)
master_data=rbind(wine_red,wine_white)
dim(master_data)
#exploration of data
str(master_data)
table(master_data$quality)
str(master_data)
#load packages
library('ggplot2')
library('ggthemes')
library('scales') 
library(dplyr) 
library('randomForest')
library("caret")
#data exploration
head(master_data)
str(master_data)
length(unique(master_data$quality))
hist(master_data$quality,col=blues9)
#binning quality
#classify the data into good ,normal and bad 
master_data$qualitycat[master_data$quality<=5]='bad'
master_data$qualitycat[master_data$quality >6 ]='good'
master_data$qualitycat[master_data$quality == 6]="normal"
table(master_data$qualitycat)
#Missing value analysis
sum(is.na(master_data))
#to find data related to type and quality
table1<- table(master_data$qualitycat,master_data$type)
barplot(table1,legend.text = T,beside=T)
#visual inspection for indepth understanding
summary(master_data$fixed.acidity)

#normalise data

  for(i in c(1,4,6,8,11)){ 
    master_data[,i]= (master_data[,i] - min(master_data[,i]))/(max(master_data[,i])-min(master_data[,i]))
  }
#boxplot to check distribution of data
boxplot(master_data[,1:11],col=blues9,xlab="variable",las=2,ylim=c(0,1))

# outliers analysis

for(i in 1:11){ 
  y =  master_data [,i][! master_data [,i] %in% boxplot.stats(master_data [,i])$out]
}

master_data = master_data[-y,]
#histogram to check frequency level distribution
new.function =function(x,xaxis,title) {
  plot= hist(x,xlab= xaxis, main= title,col=blues9,freq = T,breaks = 30,xlim = c(0,1))
return(plot)
  }
hist1 = new.function(master_data$fixed.acidity,"acidity","distribution of fixed.acidity")
hist2 = new.function(master_data$volatile.acidity,"volatile","distribution of volatile acidity")
hist3 = new.function(master_data$residual.sugar,"residual sugar","distribution of residual sugar")
hist4 =new.function(master_data$density,"m/v","m/v distribution ")
#most of the variables are skewed so we divide data into train and test with  median approach over data,train & test
#let's see variation in median using boxplot 
table1<- table (master_data$qualitycat,master_data$type) 
mosaicplot(table1,col=c(3,4))
new.function1 =function(x,title) {
  plot= boxplot(x~master_data$qualitycat*master_data$type,main= title,col=c(3:5),las=2)
  return(plot)
}
boxplot1 = new.function1(master_data$fixed.acidity,"fixed.acidity~qualitycat*type")
boxplot2 = new.function1(master_data$volatile.acidity,"volatile.acidity")
boxplot3 = new.function1(master_data$citric.acid,"citric.acid")
boxplot4 = new.function1(master_data$residual.sugar,"residual.sugar")
boxplot5 = new.function1(master_data$chlorides,"chlorides")
boxplot6 = new.function1(master_data$free.sulfur.dioxide,"free SO2")
boxplot7 = new.function1(master_data$total.sulfur.dioxide,"Total S02")
boxplot8 = new.function1(master_data$density,"m/v")
boxplot9 = new.function1(master_data$pH,"pH")
boxplot10 = new.function1(master_data$sulphates,"SO4")
boxplot11 = new.function1(master_data$alcohol,"alcohol")
#as different variable affects different type of wine so i have to build model seperately for red and white wine 
#divide the data into train and test
master_data$qualitycat=as.factor(master_data$qualitycat)
master_data$type=as.factor(master_data$type)
master_data$quality =NULL
str(master_data)
train = master_data[sample(nrow(master_data) ,5200, replace = F),]
test = master_data[!(1:nrow(master_data)) %in% as.numeric(row.names(train)),]
#corelation plot
model_data<- master_data[sapply(master_data ,function(x) length(levels(factor(x)))>1)]
model_data<-model_data[,c(1:10)]
M<-cor(model_data)
#cor. plot
#install.packages("corrplot")
library(corrplot)
plot =corrplot(M,title ="correlation plot",t1.cex=0.5,t1.col="black")
#buid train and test data for red and white wine seperately
train_red = train[which(train$type == "red"),]
train_white = train[which(train$type == "white"),]
test_red = test[which(test$type == "red"),]
test_white = test[which(test$type == "white"),]
#Decision tree
install.packages(C5.0)
library(C50)
#for red wine 
#ruleModel =C5.0(qualitycat~.,data=train_red,rules=TRUE)
ruleModel = C5.0(qualitycat ~ total.sulfur.dioxide+alcohol+sulphates+volatile.acidity+pH+free.sulfur.dioxide , data = train_red, rules = TRUE)
summary(ruleModel)
#predict using test data
test_pred = predict(ruleModel, test_red[,-13])
summary(test_pred)
table(test_pred, test_red[,13])

#Visualize the confusion matrix
library(caret)
xtab = table(observed = test_red[ ,13], predicted = test_pred)
confusionMatrix(xtab)

#for white wine

ruleModel = C5.0(qualitycat ~ volatile.acidity + chlorides+alcohol+pH+residual.sugar+sulphates , data = train_white, rules = TRUE)
summary(ruleModel)
#predict using test data
test_pred = predict(ruleModel, test_white[,-13])
summary(test_pred)
table(test_pred, test_white[,13])

#Visualize the confusion matrix
library(caret)
xtab = table(observed = test_white[ ,13], predicted = test_pred)
confusionMatrix(xtab)

#randomforest model

#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library("caret")
#install.packages("quantreg")
#install.packages("pbkrtest")

#for red wine

rf_model <- randomForest(qualitycat ~ alcohol+sulphates+total.sulfur.dioxide +volatile.acidity +citric.acid, data= train_red,ntree=500,mtry=2,do.trace=100)
rf_model
# Show model lerror
plot(rf_model, ylim=c(0,0.36),col=c(1:4))
legend('topright', colnames(rf_model$err.rate), col=1:4, fill=1:4)

# Get importance
importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance = round(importance[ , 'MeanDecreaseGini'], 2))
# Create a rank variable based on importance
#install.packages("dplyr")
library(dplyr)
rankImportance = varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# Use ggplot2 to visualize the relative importance of variables

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +ggtitle("var. importance of red wine")
  labs(x = 'Variables') + coord_flip() + theme_few()
## Predict using the test set
prediction <- predict(rf_model, test_red[,-13])
xtab = table(observed = test_red[,13], predicted = prediction)
confusionMatrix(xtab)

#for white wine
rf_model1 <- randomForest(qualitycat ~ alcohol+density+total.sulfur.dioxide +volatile.acidity +free.sulfur.dioxide+residual.sugar, data= train_white,ntree=500,mtry=2,do.trace=100)
rf_model1
# Show model error
plot(rf_model1, ylim=c(0,0.36),col=c(1:4))
legend('topright', colnames(rf_model$err.rate), col=1:4, fill=1:4)

# Get importance
importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance = round(importance[ , 'MeanDecreaseGini'], 2))
# Create a rank variable based on importance
#install.packages("dplyr")
library(dplyr)
rankImportance = varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# Use ggplot2 to visualize the relative importance of variables

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +ggtitle("var. importance of white wine")
labs(x = 'Variables') + coord_flip() + theme_few()
## Predict using the test set
prediction <- predict(rf_model1, test_white[,-13])
xtab = table(observed = test_white[,13], predicted = prediction)
confusionMatrix(xtab)


