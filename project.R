set.seed(123)   
setwd("C:/Users/a_sta/OneDrive - Worcester Polytechnic Institute (wpi.edu)/Current/DS 2010/Project")
data <- read.csv("data_combined.csv")

fit <- lm(HappinessScore ~ GDPperCapita+LifeExpectancy+Freedom,data=data)
summary(fit)
data_numeric <-read.csv("data_combined.csv")
data_numeric$Country <- NULL
data_numeric$Happiness.Rank<-NULL
PCA = prcomp(data_numeric, center = TRUE, scale = TRUE)
summary(PCA)
pairs(data_numeric)

#################################
library(rpart)
fitTree <- rpart(HappinessScore ~ GDPperCapita+LifeExpectancy+Freedom,
             method="anova", data=data)
printcp(fitTree)
plotcp(fitTree) 
summary(fitTree) 
plot(fitTree, uniform=TRUE,
     main="Regression Tree for Happiness Score")
text(fitTree, use.n=TRUE, all=TRUE, cex=.8)

#############################################
#spliting data (70% training, 30% testing)
sample_size = floor(0.70*nrow(data))  
train_ind = sample(seq_len(nrow(data)),size = sample_size) 
train =data[train_ind,] 
test = data[-train_ind,]

##############################################
#regression training with 3 predictors 
trainFit <- lm(HappinessScore ~ GDPperCapita+LifeExpectancy+Freedom,data=train)
summary(trainFit)
predicted_vals <- predict(trainFit, test)
summary(predicted_vals)

MSE <- mean((test$HappinessScore - predicted_vals)^2)
errors <- abs((test$HappinessScore-predicted_vals)/test$HappinessScore)
MAPE <- mean(errors)*100 #Mean absolute percentage error

###################################
#regression tree 
library(tree)
tree = tree(HappinessScore ~ GDPperCapita+LifeExpectancy+Freedom, data = train)
summary(tree)
plot(tree,main="Regression Tree for Happiness Score")
text(tree, pretty = 1)

treepred <-  predict(tree, test)
treeMSE <- mean((test$HappinessScore - treepred)^2)
treeErrors <- abs((test$HappinessScore-treepred)/test$HappinessScore)
treeMAPE <- mean(treeErrors)*100
