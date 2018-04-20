df <- mtcars


head(df)
library(ggplot2)
library(ggthemes)
library(dplyr)
# Grab only numeric columns
num.cols <- sapply(df, is.numeric)

# Filter to numeric columns for correlation
cor.data <- cor(df)

cor.data
library(corrplot)
corrplot(cor.data,method = "number",outline = TRUE,is.corr = TRUE)

# Import Library
library(caTools)
# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(df$mpg, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(df, sample == TRUE)

# Testing Data
test = subset(df, sample == FALSE)


model <- lm(cyl ~ .,train)
summary(model)


#predictions
cyl.predictions <- predict(model,test) 


#Ridge regression

x <- model.matrix(cyl~., train)[,-2]
y <- train$cyl
t <- model.matrix(cyl~., test)[,-2]
p <-test$cyl

lambda <- 10^seq(10, -2, length = 100)
library(glmnet)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x, y, alpha = 0)

bestlam <- cv.out$lambda.min
bestlam
#make predictions
fit <- cv_fit$glmnet.fit
ridge.pred <- predict(ridge.mod, s = bestlam, newx = t)


#Lasso regression

lasso.mod <- glmnet(x, y, alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = t)
attributes(lasso.mod)
#check MSE
mean((cyl.predictions-p)^2)  #MSE for linear regression
mean((ridge.pred-p)^2)       #MSE for ridge regression
mean((lasso.pred-p)^2)       #MSE for LAsso regression



#R2 for linear model
SSE = sum((test$cyl -cyl.predictions )^2)
SST = sum( (test$cyl-mean(df$cyl) )^2)

R2 = 1 - SSE/SST
R2

#R2 for ridge regression
SSE = sum((test$cyl -ridge.pred )^2)
SST = sum( (test$cyl-mean(df$cyl) )^2)

R2 = 1 - SSE/SST
R2

#R2 for Lasso regression
SSE = sum((test$cyl -lasso.pred )^2)
SST = sum( (test$cyl-mean(df$cyl) )^2)

R2 = 1 - SSE/SST
R2

