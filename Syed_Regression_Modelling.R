######********_______********_______*********________********________***** #

# Problem 1

######********_______********_______*********________********________***** #


library(ggplot2)
library(GGally)


### Let's perform linear regression modeling for an Ozone 
##  dataset.

dat <- read.csv(file.choose())
str(dat)
head(dat)

## Let us look at the scatter plots

ggpairs(dat[,-1])


## We should remove the highly correlated variables - why?
## It will suffice to just have a single temperature variable.

ggpairs(dat[,c(-1,-7,-10)])

fit1 <- lm(ozone_reading~., data=dat[,c(-1,-3,-10)])

summary(fit1)
AIC(fit1)


plot(fit1)

## But, these ozone measurements are temporal!

plot(as.ts(dat$ozone_reading)) ## is there a temporal trend?

## Let's include month in the model!

fit2 <- lm(ozone_reading~., data=dat)

summary(fit2)
AIC(fit2)


## What do you see?

### We need to do some model diagnostics as well!

par(mfrow=c(2,2))
plot(fit2)				### How should we interpret the plots?

## Here is a good source for interpreting these plots:

## https://data.library.virginia.edu/diagnostic-plots/


### Homework Problem:

## Question: How can we take care of the curvature in the 
## residual vs. fitted_values plot? That is, we need to introduce
## some more predictor(s) in the model!

## Try some quadratic effects - or transformation of the response?

New_model.1 = lm(log(dat$ozone_reading) ~ .^2, data = dat[c(-3)])

New_model = lm(log(dat$ozone_reading) ~ .^2, data = dat)

summary(New_model)

plot(New_model.1, main = "Fit3: Model With Quadratic Affects With Exclusion of Pressure_Height Variable")

AIC(New_model.1) 

plot(New_model)
AIC(New_model)

######********_______********_______*********________********________***** #

# Problem 2

######********_______********_______*********________********________***** #

load(file.choose())
## inspect the loaded object(s)
ls()
prob_2_list ## this is the list object that contains various "models"


## So, there are 7 models in total. These models are increasing in 
## complexity from [[1]] to [[7]], as follws:

prob_2_list[[1]] ## the simplest model
prob_2_list[[2]]
prob_2_list[[3]]
prob_2_list[[4]]
prob_2_list[[5]]
prob_2_list[[6]]
prob_2_list[[7]] ## the most complex model

## You will need to choose the "best model" among these 7 models 
## using the K-fold cross-validation algorithm, with K=10.

##### First the dataset file is read and the variables are attached to reduce code complexity. ####

Problem.2 = read.csv(file.choose())

attach(Problem.2)


#### Here we have created all the linear regression models that have been mentione previously. The first model is simple as only two variables exist in the model but slowly the complexity increases as more and more variables are added. ####

Problem_2_model_1 = lm(y ~ x3 + x9 , data = Problem.2)

Problem_2_model_2 = lm(y ~ x3+ x4 + x9 , data = Problem.2)

Problem_2_model_3 = lm(y ~ x3 + x4 + x7 + x9 , data = Problem.2)

Problem_2_model_4 = lm(y ~ x3 + x4 + x7 + x9 + x37, data = Problem.2)

Problem_2_model_5 = lm(y ~ x2 +  x3 + x4 + x7 + x9 + x12 + x19 + x20 + x22 + x28 + x37, data = Problem.2)

Problem_2_model_6 = lm(y ~ x2 +  x3 + x4 + x7 + x9 + x12 + x19 + x20 + x22 + x28 + x37 + x10 + x11 + x27 + x30, data = Problem.2)

Problem_2_model_7 = lm(y ~ x1 + x2 + x3 +  x4 + x5 + x6 + x7 +  x8 +  x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40, data = Problem.2)


##### Here you can view the summary for each model in order to gain an understanding of the importance of the variables in the model and also view the R-squared values. ####

summary(Problem_2_model_1)

summary(Problem_2_model_2)

summary(Problem_2_model_3)

summary(Problem_2_model_4)

summary(Problem_2_model_5) # adjusted R-sqaured is the highest here

summary(Problem_2_model_6)

summary(Problem_2_model_7)


#### Adjusted R-squared values for each model has been placed into the following variables. They will come in hand when the plot is created. ####

model_1R <- summary(Problem_2_model_1)$adj.r.squared
model_2R <- summary(Problem_2_model_2)$adj.r.squared
model_3R <- summary(Problem_2_model_3)$adj.r.squared
model_4R <- summary(Problem_2_model_4)$adj.r.squared
model_5R <- summary(Problem_2_model_5)$adj.r.squared
model_6R <- summary(Problem_2_model_6)$adj.r.squared
model_7R <- summary(Problem_2_model_7)$adj.r.squared

Models_R <- rbind(model_1R, model_2R,model_3R, model_4R,model_5R, model_6R,model_7R)
Models_R
plot(Models_R, ylab = "Adjusted R-squared Values", xlab = "Increasing Model Complexity", pch = 17, col = "green")

##### AIC values for each model have been listed here. ####
AIC1 <- AIC(Problem_2_model_1)
AIC2 <- AIC(Problem_2_model_2)
AIC3 <- AIC(Problem_2_model_3)
AIC4 <- AIC(Problem_2_model_4)
AIC5 <- AIC(Problem_2_model_5)
AIC6 <- AIC(Problem_2_model_6)
AIC7 <- AIC(Problem_2_model_7)
Models_AICs <- rbind(AIC1,AIC2,AIC3,AIC4,AIC5,AIC6,AIC1)
Models_AICs
plot(Models_AICs, ylab = "AIC Values", xlab = "Increasing Model Complexity", pch = 17, col = "red")

#### This is for model 1 ####

# The code has been created in such a way that it takes samples of the dataset and then uses the 10 fold cross validation. It measures as a mean sqaure error. 

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ] # Here the sample is taken
fold.10 = 10  # The fold is set
folds <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE) # The the folds need to be broken and the continously be used one by one. thus function sequence to be used. 
order = 15 # This is degree of polynomial. We tried setting the polynomial degree higher but it provides an error when the model becomes too complex. 

r.square = matrix(data=NA,nrow=fold.10,ncol=order) # This allows for the error values to be placed in a matrix format

# Here a loop is created that will take the sample divide it into training and testing sets. The partitioning of the data is important for cross validation. The i stands for each fold. Once the dataset has partioned it goes forward to be placed as Cv error or r-sqaured error which then be called mean-rsqaure or mean sqaure error. 

for(i in 1:fold.10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- Problem.shuffled[testIndexes, ]
  train <- Problem.shuffled[-testIndexes, ]
  
  for (j in 1:order){
  # Here is the trainnig of the regression model. The poly function is used for the increase polynomial degree .
    Fit.train = lm(y ~ poly(x3 + x9, j) , data = Problem.2)
    fit.test = predict(Fit.train, newdata=test) # Here the prediction is made which will generate values to produce mean R square error values. 
    r.square[i,j] = cor(fit.test, test$y, use='complete')^2 
  }
}

fits.kfold <- colMeans(r.square)  # Each time it is run each set of Mean r square values are saved here. It was difficult creating the while thus we have taken a shortcut 
fits.kfold.1 <- colMeans(r.square)
fits.kfold.2 <- colMeans(r.square)
fits.kfold.3 <- colMeans(r.square)
fits.kfold.4 <- colMeans(r.square)

plot(colMeans(r.square), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_1 Cross Validation", type='l') # This is where the cross validation graph for model is built. 

me = (mean(fits.kfold)+mean(fits.kfold.1)+mean(fits.kfold.2)+mean(fits.kfold.3)+mean(fits.kfold.4))

average.cv = me/5 # Here we take the average. 


#### This is for model 2 ####

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ]
fold.10 = 10 
folds2 <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE)
order = 15
r.square2 = matrix(data=NA,nrow=fold.10,ncol=order)


for(i in 1:fold.10){
  #Segement data by fold using the which() function 
  testIndexes2 <- which(folds==i,arr.ind=TRUE)
  test2 <- Problem.shuffled[testIndexes, ]
  train2 <- Problem.shuffled[-testIndexes, ]
  #Use the test and train data partitions
  #Model fitting and evaluation
  for (j in 1:order){
    #training regression model on training folds
    Fit.train2 = lm(y ~ poly(x3+ x4 + x9, j) , data = Problem.2)
    #evaluating fit on the test fold
    fit.test2 = predict(Fit.train2, newdata=test2)
    r.square2[i,j] = cor(fit.test2, test2$y, use='complete')^2
  }
}

fits2.kfold <- colMeans(r.square2)
fits2.kfold.1 <- colMeans(r.square2)
fits2.kfold.2 <- colMeans(r.square2)
fits2.kfold.3 <- colMeans(r.square2)
fits2.kfold.4 <- colMeans(r.square2)

plot(colMeans(r.square2), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_2 Cross Validation", type='l')

me2 = (mean(fits2.kfold)+mean(fits2.kfold.1)+mean(fits2.kfold.2)+mean(fits2.kfold.3)+mean(fits2.kfold.4))

average.cv2 = me2/5


#### This is for model 3 ####

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ]
fold.10 = 10 
folds3 <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE)
order = 15
r.square3 = matrix(data=NA,nrow=fold.10,ncol=order)


for(i in 1:fold.10){
  #Segement data by fold using the which() function 
  testIndexes3 <- which(folds==i,arr.ind=TRUE)
  test3 <- Problem.shuffled[testIndexes, ]
  train3 <- Problem.shuffled[-testIndexes, ]
  #Use the test and train data partitions
  #Model fitting and evaluation
  for (j in 1:order){
    #training regression model on training folds
    Fit.train3 = lm(y ~ poly(x3 + x4 + x7 + x9, j) , data = Problem.2)
    #evaluating fit on the test fold
    fit.test3 = predict(Fit.train3, newdata=test3)
    r.square3[i,j] = cor(fit.test3, test3$y, use='complete')^2
  }
}

fits3.kfold <- colMeans(r.square3)
fits3.kfold.1 <- colMeans(r.square3)
fits3.kfold.2 <- colMeans(r.square3)
fits3.kfold.3 <- colMeans(r.square3)
fits3.kfold.4 <- colMeans(r.square3)

plot(colMeans(r.square3), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_3 Cross Validation", type='l')

me3 = (mean(fits3.kfold)+mean(fits3.kfold.1)+mean(fits3.kfold.2)+mean(fits3.kfold.3)+mean(fits3.kfold.4))

average.cv3 = me3/5



#### This is for model 4 ####

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ]
fold.10 = 10 
folds4 <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE)
order = 15
r.square4 = matrix(data=NA,nrow=fold.10,ncol=order)


for(i in 1:fold.10){
  #Segement data by fold using the which() function 
  testIndexes4 <- which(folds==i,arr.ind=TRUE)
  test4 <- Problem.shuffled[testIndexes, ]
  train4 <- Problem.shuffled[-testIndexes, ]
  #Use the test and train data partitions
  #Model fitting and evaluation
  for (j in 1:order){
    #training regression model on training folds
    Fit.train4 = lm(y ~ poly(x3 + x4 + x7 + x9 + x37, j) , data = Problem.2)
    #evaluating fit on the test fold
    fit.test4 = predict(Fit.train4, newdata=test4)
    r.square4[i,j] = cor(fit.test4, test4$y, use='complete')^2
  }    
}

fits4.kfold <- colMeans(r.square4)
fits4.kfold.1 <- colMeans(r.square4)
fits4.kfold.2 <- colMeans(r.square4)
fits4.kfold.3 <- colMeans(r.square4)
fits4.kfold.4 <- colMeans(r.square4)

plot(colMeans(r.square4), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_4 Cross Validation",type='l')

me4 = (mean(fits4.kfold)+mean(fits4.kfold.1)+mean(fits4.kfold.2)+mean(fits4.kfold.3)+mean(fits4.kfold.4))

average.cv4 = me4/5


#### This is for model 5 ####

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ]
fold.10 = 10 
folds5 <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE)
order = 15
r.square5 = matrix(data=NA,nrow=fold.10,ncol=order)


for(i in 1:fold.10){
  #Segement data by fold using the which() function 
  testIndexes5 <- which(folds==i,arr.ind=TRUE)
  test5 <- Problem.shuffled[testIndexes, ]
  train5 <- Problem.shuffled[-testIndexes, ]
  #Use the test and train data partitions
  #Model fitting and evaluation
  for (j in 1:order){
    #training regression model on training folds
    Fit.train5 = lm(y ~ poly(x2 + x3 + x4 + x7 + x9 + x12 + x19 + x20 + x22 + x28 + x37, j) , data = Problem.2)
    #evaluating fit on the test fold
    fit.test5 = predict(Fit.train5, newdata=test5)
    r.square5[i,j] = cor(fit.test5, test5$y, use='complete')^2
  }    
}

fits5.kfold <- colMeans(r.square5)
fits5.kfold.1 <- colMeans(r.square5)
fits5.kfold.2 <- colMeans(r.square5)
fits5.kfold.3 <- colMeans(r.square5)
fits5.kfold.4 <- colMeans(r.square5)

plot(colMeans(r.square5), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_5 Cross Validation", type='l')

me5 = (mean(fits5.kfold)+mean(fits5.kfold.1)+mean(fits5.kfold.2)+mean(fits5.kfold.3)+mean(fits5.kfold.4))

average.cv5 = me5/5


#### This is for model 6 ####

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ]
fold.10 = 10 
folds6 <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE)
order = 15
r.square6 = matrix(data=NA,nrow=fold.10,ncol=order)


for(i in 1:fold.10){
  #Segement data by fold using the which() function 
  testIndexes6 <- which(folds==i,arr.ind=TRUE)
  test6 <- Problem.shuffled[testIndexes, ]
  train6 <- Problem.shuffled[-testIndexes, ]
  #Use the test and train data partitions
  #Model fitting and evaluation
  for (j in 1:order){
    #training regression model on training folds
    Fit.train6 = lm(y ~ poly(x2 +  x3 + x4 + x7 + x9 + x12 + x19 + x20 + x22 + x28 + x37 + x10 + x11 + x27 + x30, j) , data = Problem.2)
    #evaluating fit on the test fold
    fit.test6 = predict(Fit.train6, newdata=test6)
    r.square6[i,j] = cor(fit.test6, test6$y, use='complete')^2
  }    
}

fits6.kfold <- colMeans(r.square6)
fits6.kfold.1 <- colMeans(r.square6)
fits6.kfold.2 <- colMeans(r.square6)
fits6.kfold.3 <- colMeans(r.square6)
fits6.kfold.4 <- colMeans(r.square6)

plot(colMeans(r.square6), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_6 Cross Validation",type='l')

me6 = (mean(fits6.kfold)+mean(fits6.kfold.1)+mean(fits6.kfold.2)+mean(fits6.kfold.3)+mean(fits6.kfold.4))

average.cv6 = me6/5

#### This is for model 7 ####

Problem.shuffled <- Problem.2[sample(nrow(Problem.2)), ]
fold.10 = 10 
folds7 <- cut(seq(1,nrow(Problem.shuffled)),breaks=fold.10,labels=FALSE)
order = 15
r.square7 = matrix(data=NA,nrow=fold.10,ncol=order)


for(i in 1:fold.10){
  #Segement data by fold using the which() function 
  testIndexes7 <- which(folds==i,arr.ind=TRUE)
  test7 <- Problem.shuffled[testIndexes, ]
  train7 <- Problem.shuffled[-testIndexes, ]
  #Use the test and train data partitions
  #Model fitting and evaluation
  for (j in 1:order){
    #training regression model on training folds
    Fit.train7 = lm(y ~ poly(x1 + x2 + x3 +  x4 + x5 + x6 + x7 +  x8 +  x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40, j) , data = Problem.2)
    #evaluating fit on the test fold
    fit.test7 = predict(Fit.train7, newdata=test7)
    r.square7[i,j] = cor(fit.test7, test7$y, use='complete')^2
  }    
}

fits7.kfold <- colMeans(r.square7)
fits7.kfold.1 <- colMeans(r.square7)
fits7.kfold.2 <- colMeans(r.square7)
fits7.kfold.3 <- colMeans(r.square7)
fits7.kfold.4 <- colMeans(r.square7)

plot(colMeans(r.square7), ylab = "Mean R-squared", xlab = "Polynomial Degree", main = "Problem_2_model_7 Cross Validation", type='l')

me7 = (mean(fits7.kfold)+mean(fits7.kfold.1)+mean(fits7.kfold.2)+mean(fits7.kfold.3)+mean(fits7.kfold.4))

average.cv7 = me7/5

# Average cv values for each model
Model.cv1 <- average.cv 
Model.cv2 <- average.cv2
Model.cv3 <- average.cv3
Model.cv4 <- average.cv4
Model.cv5 <- average.cv5
Model.cv6 <- average.cv6
Model.cv7 <- average.cv7

Models_cv <- rbind(average.cv, average.cv2, average.cv3, average.cv4, average.cv5,average.cv6,average.cv7)
Models_cv
plot(Models_cv, ylab = "CV Values", xlab = "Increasing Model Complexity", pch = 17, col = "blue")


par(mfrow=c(1,3))
plot(Models_cv, ylab = "CV Values", xlab = "Increasing Model Complexity", pch = 17, col = "blue")






































































######********_______********_______*********________********________***** #

# Problem 3

######********_______********_______*********________********________***** #



# First upload the packages require to run some of te functions. 
library(glmnet)
library(ggplot2)
library(GGally)
library(pROC)
library(MASS)

#### The dataset csv file is read in to the variable Diabetes.1####

Diabetes.1 = read.csv(file.choose())

#### Look at some of the variables of the date####

str(Diabetes.1)

####set.seed so the sampling remains consistent. The set.seed is important as it can affect you analysis downstream. ####

set.seed(1245)

#### If you wan't you can view the dataset in a tabular format to get a complete understanding of its structure ####

View(Diabetes.1)

attach(Diabetes.1)


##### The logistic regression model is conducted, and the variables of x are placed into a matrix format####

str(Diabetes.1)
f0 <- lm(positive ~ .^2, data=Diabetes.1[,-9])

X = model.matrix(f0)[,-1]

X <- cbind(X,X[,1:8]^2)

y = Diabetes.1$positive

colnames(X) ## duplicate names!

colnames(X) <- c(colnames(model.matrix(f0)[,-1]), paste(colnames(X)[1:8],2,sep=""))

colnames(X)


#### This is where sampling takes place. the dateset is divided into 85% for training and 15% for validation. ####

round(dim(X)[1]*.15)

test.index <- sample.int(dim(X)[1],round(dim(X)[1]*.15), replace = FALSE)


#### A loop is created for elastic net regression in order to go through the the alpha values 0,1 and generate a model for each, in this case we have selected lambda.min for each and finally AUC values are generated for each alpha/lambda.min pair. #### The first loop is to run the cross validation for each alpha

list.of.fits <- list()

for (i in 0:100) { fit.name <- paste0("alpha", i/100)

list.of.fits[[fit.name]] <- cv.glmnet(X[-test.index,], y[-test.index], type.measure = "auc", alpha=i/100, family = "binomial")}


#### Another loop is created to predict AUC values for each alpha/lambda.min pair and place them in a dataframe format. 

results <- data.frame()

for (i in 0:100) {fit.name <- paste0("alpha", i/100)
  
prds.train <- predict(list.of.fits[[fit.name]],newx = X[-test.index,], s=list.of.fits[[fit.name]]$lambda.min, type = "response")
  
   
auc.train <- roc(y[-test.index], prds.train, levels = c(0,1), direction = "<")

lambds = (list.of.fits[[fit.name]]$lambda.min)
  
temp <- data.frame(alpha=i/100, AUC = auc.train$auc[[1]], fit.name = fit.name, lambda = lambds[[1]])

results <- rbind(results, temp)}

#### You can View the results in tabular format and from their select the best alpha/lambda.min pair based on the AUC values. There is a sorting option when you view the dataframe####

View(results)

#Our best alpha value is 0.94 at 0.0002153102 lambda value #

#### Plotting the auc training set values along with the alpha values. 

plot(results$alpha, results$AUC, type = "b", ylab = "AUC Values for Training Set", xlab = "Alpha Values from 0 to 1.0" , main = "Plot of AUC Values Against the Alpha Values (0,1)")


##### Running another loop in order to get the auc values along with alpha values based on the testing set. ####


results.1 <- data.frame()

for (i in 0:100) {fit.name <- paste0("alpha", i/100)

prds.test <- predict(list.of.fits[[fit.name]],newx = X[test.index,], s=list.of.fits[[fit.name]]$lambda.min, type = "response")

auc.test <- roc(y[test.index], prds.test, levels = c(0,1), direction = "<")

lambds = (list.of.fits[[fit.name]]$lambda.min)

temp.1 <- data.frame(alpha=i/100, AUC = auc.test$auc[[1]], fit.name = fit.name, lambda = lambds[[1]])

results.1 <- rbind(results.1, temp.1)}

View(results.1)

plot(results.1$alpha, results.1$AUC, type = 'b', ylab = "AUC Values for Validation Set", xlab = "Alpha Values from 0 to 1.0" , main = "Plot of AUC Values Against the Alpha Values (0,1)")

auc.test$auc


#### 0.83 alpha value along with lambda min seems to be the best in the model ####


##### The confustion matrix for training set ####

conf.mat1 <- table(y=y[-test.index],yhat=as.numeric(prds.train>.5))

conf.mat1


####Function to get the sensitivity and specificty values####

sn.sp <- function(mat){
  sn <- mat[2,2]/sum(mat[2,])
  sp <- mat[1,1]/sum(mat[1,])
  return(unlist(list(sensitivity=sn, specificity=sp)))
}



#### Here is a method to check the specificity and sensitivity thresholds. You will notice that they have been identified correctly #####

snsp.test.c <- cbind(auc.test$sensitivities,auc.test$specificities)

indx2 <- which.max(apply(snsp.test.c,1,min))  ### min-max approach!
indx2
snsp.test.c[indx2,]

cutoff2 <- auc.test$thresholds[indx2]
cutoff2

#### This is where we have plotted the ROC curve for the validation test.#### 

plot(auc.test, main = "Plot of ROC Curve Via Elastic Net Regression")
abline(h=snsp.test.c[indx2,1],v=snsp.test.c[indx2,2], col='blue', lty=2)
par(mfrow=c(1,1))


#### In case you want to visualize the coefficients here is a plot. Many have been suprressed other than 1 which seems to be present slightly indicating that it is a vital one. #### 

library(coefplot)

coefplot(list.of.fits[[fit.name]],list.of.fits[[fit.name]]$lambda.min, sort = "magnitude")



#+++++****++++++****++++++++++*******++++++++++

#### This is where we have conducted lasso regression #####

#+++++****++++++****++++++++++*******++++++++++

set.seed(1245)

round(dim(X)[1]*.15)

test.index <- sample.int(dim(X)[1],round(dim(X)[1]*.15), replace = FALSE)

system.time(cvx <- cv.glmnet(X[-test.index,],y[-test.index], nfolds = 10, family="binomial", alpha=1, type.measure = "auc"))

plot(cvx)

prds.train <- predict(cvx,newx = X[-test.index,], type = "response", s=cvx$lambda.min)[,1]
prds.test <- predict(cvx,newx = X[test.index,], type = "response", s=cvx$lambda.min)[,1]
auc.test.2 <- roc(y[test.index],prds.test, levels = c(0,1), direction = "<")

snsp.test.2 <- cbind(auc.test.2$sensitivities,auc.test.2$specificities)

indx2.3 <- which.max(apply(snsp.test.2,1,min))  ### min-max approach!
indx2.3
snsp.test.2[indx2,]

cutoff2.1 <- auc.test$thresholds[indx2.3]
cutoff2.1

plot(auc.test.2, main = "Plot of ROC Via Lasso Regression")
abline(h=snsp.test.2[indx2.3,1],v=snsp.test.2[indx2.3,2], col='blue', lty=2)
par(mfrow=c(1,1))


#+++++****++++++****++++++++++*******++++++++++

#### This is where we have conducted Ridge regression #####

#+++++****++++++****++++++++++*******++++++++++


system.time(cvx.1 <- cv.glmnet(X[-test.index,],y[-test.index], nfolds = 10, family="binomial", alpha=0, type.measure = "auc"))

plot(cvx.1)

prds.train <- predict(cvx.1,newx = X[-test.index,], type = "response", s=cvx.1$lambda.min)[,1]
prds.test <- predict(cvx.1,newx = X[test.index,], type = "response", s=cvx.1$lambda.min)[,1]
auc.test.1 <- roc(y[test.index],prds.test, levels = c(0,1), direction = "<")

snsp.test.1 <- cbind(auc.test.1$sensitivities,auc.test.1$specificities)

indx2.1 <- which.max(apply(snsp.test.1,1,min))  ### min-max approach!
indx2.1
snsp.test.1[indx2.1,]

cutoff2 <- auc.test.1$thresholds[indx2.1]
cutoff2

plot(auc.test.1, main = "Plot of ROC via Ridge Regression")
abline(h=snsp.test.1[indx2.1,1],v=snsp.test.1[indx2.1,2], col='blue', lty=2)
par(mfrow=c(1,1))

auc.test.1$auc
auc.test$auc

#### Visualize Coefficients #####

coefplot(cvx,cvx$lambda.min, sort = "magnitude") # For Lasso

coefplot(cvx.1,cvx.1$lambda.min, sort = "magnitude") # For Ridge

coefplot(list.of.fits[[fit.name]],list.of.fits[[fit.name]]$lambda.min, sort = "magnitude") #  For Elastic


##### Reference for the cross-validation function #####

#https://quantdev.ssri.psu.edu/sites/qdev/files/04_Regression_CrossValidation_2017_1115.html