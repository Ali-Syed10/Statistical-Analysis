##### BINF6970 Assignment2----
# Group members: Alisha Soares, Syed Ali, Kassaye

# Install and load the necessary packages
library(lars)
library(glmnet)
library(caret)
library(plotly)
library(coefplot)

# Obtain the diabetes data set from the lars package
?diabetes
data(diabetes)
attach(diabetes)

# Explore the data set
str(diabetes)
names(diabetes)
attributes(diabetes)
dim(diabetes$y)
is.vector(diabetes$y)
length(diabetes$y)
summary(diabetes$x2)
dim(diabetes$x2)
colnames(diabetes$x2)

## x2 has a larger variety of predictors. Regress (OLS - ordinary least squares) y on x2. 
model_reg <- lm(diabetes$y~diabetes$x2)
summary(model_reg)

# Multiple R squared value = 0.59. Only 59% of variation in dependent variables is explained by model. The variables sex, bmi and map are the most significant as the p values are close to zero.

set.seed(1212)

# First confirm whether x2 is standardized or not.
apply(diabetes$x2,2,mean)
apply(diabetes$x2,2,mean) < 1e-10
apply(diabetes$x2,2,var)

# Split the data into a training and test set. Where 90.5% of observations have been split in to a training dataset and 10% have been split into test dataset.
dt = sort(sample(nrow(diabetes), nrow(diabetes)*0.905))
train <- diabetes[dt, ]
test <- diabetes[-dt, ]

# Confirm the 90:10 ratio split of the dataset. 
dim(train)
dim(test)

#### PART 1: THE LASSO MODEL ----

# Fit LASSO model with 10 fold cross validation using the training set for design matrix x2 of the diabetes dataset.When alpha=1 it is LASSO. Use cv.glmnet to tune lambda and get the best fit model.
set.seed(1212)
cvxl <- cv.glmnet(train$x2, train$y, type.measure="mse", nfolds=10, alpha = 1)
attributes(cvxl)
plot(cvxl)

#The above plot shows the cross-validation plot where multiple log(lambda) values ranging for less than -4 to more than -4 have produced a meansquare error value. The plot also has shown two vertical dotted lines indicating the optimal lambda values that can be used to produce the model for regression of the diabetes design matrix x2 coefficients. 

# Check the lambda value, lambda.min and 1se values. There are about 100 values generated for lambda but the optimum that the function thinks two will be indicated by the lambda.min or lambda.1se. 
lambda <- cvxl$lambda
length(lambda)
cvxl$lambda.min
cvxl$lambda.1se

# Plot the lambda values against the CV-ERROR. Here we have also have changed the name mean square error to crossvalidation error or CV-error. 
is.vector(cvxl$name)
cvxl$name <- "CV-ERROR"
plot(cvxl)

# Check the coefficients for lambda.min
coef(cvxl, cvxl$lambda.min)

lasso.whole <- glmnet(diabetes$x2,diabetes$y,lambda=cvxl$lambda, alpha=1)
plot(lasso.whole)
coefpath(lasso.whole)
coefplot(lasso.whole, lambda=cvxl$lambda.min, sort = 'magnitude')

# Find the indices of the lambda values we want to use for the model
wh <- which(lambda >= cvxl$lambda.min & lambda <= cvxl$lambda.1se)  

sps.set <- lambda[wh]  ## and this is the set we want!
sps.set
length(sps.set)

#sps.set has all the plausible lambda values that will generate a good enough model. However there are multiple values generated and one has to be chosen that will eventually produce the best model. To further epxlore this issue the predict function is used and for each corresponding lambda value a mean-square value is generated. 

prd.sps.set <- predict(cvxl,newx=train$x2,s=sps.set )
dim(prd.sps.set)

# This is the function created to generate the mean square values for the given lambda values in the sps.set. 

mse <- function (i,y) {
  mean ( (y-i)^2 ) }		

# Here we generate the mean square values

mse.sps.set <- apply(prd.sps.set,2,mse,train$y)
mse.sps.set

# This is the part where we have applied the glmnet function to create a glment graph or a model that will apply the penalties set by the cross-validation. The coefpath will generate a model of the coefficients and indicate at what values of lambda do most of the coefficients begin to be suppressed. The coefplot will display the coefficients in a plot for the lambda.min value which after consideration of the mean square value has been selected as the best model in regards to the training set. 

cvxr.lasso <- glmnet(train$x2,train$y,lambda=cvxl$lambda, alpha=1)
coefplot(cvxr.lasso, lambda=cvxl$lambda.min, sort = 'magnitude')
coefpath(cvxr.lasso)

# Compare the LASSO models for predictive strength using log-scale. The 

plot(log(sps.set),mse.sps.set, ylab = "Mean Square Error of Train Design Matrix x2", xlab = "Lambda Values Extracted from the Cross Validation", main = "The Plot of Prediction Error of Response Values Y in Train Set")	## better on the log-sale

# Here display what lambda value produces what ean square error. 

train.lam <- cbind(log(sps.set), mse.sps.set)

train.lam

#The plot has also been generated but incase there is any confusion the cbind function has been used to indicate the values of mean-square error corresponding to each log lambda values. All this data shows that the lowest lambda value is the one that produces the lowest mean square error. 

#This shows that the lowest optimum lambda value is:

cvxl$lambda.min

# A loop for production of multiple boxplots has also been created in order to explore that this lambda value indeed is the best one. Perhaps the mean-square error can be low but it might affect the normal distribution. 

par(mfrow = c(1,1))

for (s in sps.set) {
  lasso.predict <- predict(cvxl, s = c(s), newx = train$x2)
  boxplot(lasso.predict, xlab = "Boxplot for Prediction Value Corresponding to each Lamda Value")
}

# The boxplot also show that this is distributed much better by the lambda.min value. You can compare the first plot which is the highest lambda value to the plot with lambda.min. Notice how the range of values have changed as well which is shown in the y-axis. this and the box itself has also shortened as well. This indicates that the data set has fitted well properly. 

# Now that we have established that lambda.min generates the best we will go ahead and make predictions for test set. 

prd.sps.set.1 <- predict(cvxl,newx=test$x2,s=sps.set )

mse <- function (i,y) {mean ( (y-i)^2 ) }	

mse.sps.set.1 <- apply(prd.sps.set.1,2,mse,test$y)

plot(sps.set,mse.sps.set.1)

plot(log(sps.set),mse.sps.set.1, ylab = "Mean Square Error of Test Design Matrix x2", xlab = "Lambda Values Extracted from the Cross Validation", main = "The Plot of Prediction Error of Response Values Y in Test Set")

qqnorm(mse.sps.set.1)

par(mfrow = c(1,1))

for (s in sps.set) {
  lasso.predict <- predict(cvxl, s = c(s), newx = test$x2)
  boxplot(lasso.predict, xlab = "Boxplot for Prediction Value Corresponding to each Lamda Value")
}

# Here you can compare the mean square error values for the given lambda values in regards to the both the training set and test set. 

compare.mse <- data.frame(mse.sps.set.1,mse.sps.set, sps.set)
names(compare.mse)[1] <- "Lasso.mse.test"
names(compare.mse)[2] <- "Lasso.mse.train"

View(compare.mse) 

# The function now shows the comparison of between the training and test mse values. We chose 9th the lambda.min and in comparison to test set the model that was produced using lambda.min has proven to be very effective. The reduced mse error clearly shows the the model has fitted well. 

# Same thing what was done previously is a plot of coefficents and the path has been produced. There is not much difference however, you will notice that one more coefficient has been given more importance. The best thing about the coefplot function is the magnitude which will display the coefficients with importance. 

cvxr.1 <- glmnet(test$x2,test$y,lambda=cvxl$lambda, alpha=1)

plot(cvxr.1)

coefpath(cvxr.1)
coefplot(cvxr.1, lambda=cvxl$lambda.min, sort = 'magnitude' )


#### Ridge Regression ----

# Now that the lasso regression has been completed same steps will be taken to conduct ridge regression. Few changes will be made but overall the code will be very similar. 

# Fit Ridge Regression model with 10 fold cross validation using the training set for design matrix x2 of the diabetes dataset. When alpha=0 it is ridge regression. Again use cv.glmnet to tune lambda and get the best fit model
cross.ridge.diabetes <- cv.glmnet(train$x2, train$y, type.measure="mse", nfolds=10, alpha = 0,  family = "gaussian")

cross.ridge.diabetes$name <- "CV - ERROR"

plot(cross.ridge.diabetes, main = "Plot of Cross-Validation Using Ridge Regression for Train Design Matrix x2")

# Here we have used the predict function to predict the y response values in regards to the training set. 

cross.ridge.predict <- predict(cross.ridge.diabetes, s = c(cross.ridge.diabetes$lambda.min,cross.ridge.diabetes$lambda.1se), newx = train$x2)

# The lambda values have been extracted and sorted from smallest to largest order. 

lambda <- sort(cross.ridge.diabetes$lambda)

# This function has extracted the range of lambda values of similar strength. The values will range from lambda.min to values that fall in the middle to lambda.1se. However the which function will actually take indices not the values themselves. 

wh <- which(lambda >= cross.ridge.diabetes$lambda.min & lambda <= cross.ridge.diabetes$lambda.1se)

# Here the lambda values will be displayed and saved in object sps.set.ridge. 

sps.set.ridge <- lambda[wh]

# You can check how many lambda values have been displayed. 

length(lambda[wh])

# This is where the predict function is used to predict the y response values using all the models or lambda value in the sps.set.ridge. 

prd.sps.set.ridge <- predict(cross.ridge.diabetes,newx=train$x2,s=sps.set.ridge )

#The visualization of the boxplots show that which of these lambda values 

par(mfrow = c(1,1))

for (s in sps.set.ridge) {
  cvridge.predict <- predict(cross.ridge.diabetes, s = c(s), newx = train$x2)
  boxplot(cvridge.predict, xlab = "Boxplot for Prediction Value Corresponding to each Lamda Value")
  
}

# Here we have computed the mean sqaure values for each lambda or model in the sps.set.ridge and using cbind() function the mean square values for each corresponding model has been displayed. 

mse <- function (i,y) {mean ( (y-i)^2 ) }	

mse.sps.set.ridge <- apply(prd.sps.set.ridge,2,mse, train$y)

cbind(mse.sps.set.ridge,sps.set.ridge )

cross.ridge.diabetes$lambda.min

# The glmnet function will allow for the creation of the coefficient path plot and the coefficient plot. 

cvx.ridge.1 <- glmnet(train$x2,train$y,lambda=cross.ridge.diabetes$lambda, alpha=0)

coefpath(cvx.ridge.1)

# The coefplot has been displayed for what we think is the best model which lambda.min. You will notice in comparison to the lasso plot the coefficients have been increased not many have been supressed or removed. 

coefplot(cvx.ridge.1,cross.ridge.diabetes$lambda.min, sort = 'magnitude')

# This is where have plotted the ridge regression mse against the lambda values in both regular and log(lambda) format. So far it seems to be linear. 

plot(sps.set.ridge,mse.sps.set.ridge)

plot(log(sps.set.ridge),mse.sps.set.ridge, ylab = "Mean Square Error of Train Design Matrix x2", xlab = "Lambda Values Extracted from the Cross Validation", main = "The Plot of Prediction Error of Response Values Y in Test Set")

# This is where we have used the predict function to get the list of y response values given the test set. 

prd.sps.set.1 <- predict(cross.ridge.diabetes,newx=test$x2,s=sps.set.ridge )

# This is where we have calculated the mean square error values of the test set givent the extracted lambda values from the training set from the cross-validation.

mse.sps.set.1 <- apply(prd.sps.set.1,2,mse,test$y)

# The mean square values have been plotted agains the lambda and log(lambda) values. You will notice the graph is not as linear but the mean square values are a lot lower which means this represents the coefficients are now displayed to their actual close value. While previously they had been supressed quite a bit. Coefficients that were not important are now important, and you will see it in more detail in the upcoming coefficient plot. 

plot(sps.set.ridge,mse.sps.set.1 )

plot(log(sps.set.ridge),mse.sps.set.1, ylab = "Mean Square Error of Test Design Matrix x2", xlab = "Lambda Values Extracted from the Cross Validation", main = "The Plot of Prediction Error of Response Values Y in Test Set")

cbind(mse.sps.set.ridge,mse.sps.set.1)

# See how covariates behave across lambda values. This where I have plotted the coefficients extracted from the glmnet function for the lambda.min. 
cvx.ridge <- glmnet(test$x2,test$y,lambda=cross.ridge.diabetes$lambda, alpha=0)

plot(cvx.ridge)

coefpath(cvx.ridge,xvar="lambda")

coefplot(cvx.ridge, lambda = cross.ridge.diabetes$lambda.min, sort = "magnitude")

# Overall though the best model in regards to the training set does not remain to be lambda.min as the mean square of a higher lambda value is much better the fact is there is not a signicant difference thus lambda.min still remains to be quite optimum. Below is the code that shows the coefficients for best model lambda.min in training, lambda.min in testing and best model in testing.

#cvx.ridge.1, cross.ridge.diabetes$lambda.min = best model in Training
# cvx.ridge, cross.ridge.diabetes$lambda.min = comparison to testing same lambda value
#cvx.ridge, sps.set.ridge[10] = best model in testing

cbind(coef(cvx.ridge.1, cross.ridge.diabetes$lambda.min), coef(cvx.ridge, cross.ridge.diabetes$lambda.min), coef(cvx.ridge, sps.set.ridge[10]))

#### Elastic Net ----

# Similar Step have been repeated but the alpha grid has been displayed to make the model very flexible

cvelastic <- cv.glmnet(train$x2, train$y, type.measure="mse", nfolds=10, alpha = seq(.1, .9, .01), family = "gaussian")

attributes(cvelastic)

par(mfrow=c(1,1))

cvelastic$name <- "CV - ERROR"

plot(cvelastic, main = "Plot of Cross-Validation Using Elastic Regression for Train Design Matrix x2")

# The first cross-validation graph is actually pre-liminary graph until the optimum alpha value has been figured out. 

enet <- trainControl(method = "cv", number = 10)

# The grid search for alpha values have been created so that a large number of lambda value will be created. 

tuneGrid <- expand.grid(.alpha = seq(.1, .9, .01),.lambda = seq(.5,7.5,1))

dim(tuneGrid)
head(tuneGrid)

set.seed(1212)

# Once the corss-validation had been done the training begins of the model using the training data with in consideration of the training set. 

system.time(db_net2 <- train(train$x2,train$y,method = "glmnet", trControl = enet, tuneGrid=tuneGrid))
par(mfrow=c(1,2))
par(mfrow=c(1,1))

# The alpha values that were produced due to the search grid have been extracted. 
alphas <- db_net2$results[,"alpha"]

# Each alpha produces a model, and the lambda is the model, thus all the lambda values have been extracted. 

lambdas <- db_net2$results[,"lambda"]
RMSE <- db_net2$results[,"RMSE"]

# This will display the best pair found for the alpha and lambda
w2 <- which.min(db_net2$results[,3]) ## the best pair!

db_net2$bestTune

# This function also displays the same thing

# This is the optimum alpha value

db_net2$bestTune[,1]

#After the reveal of optimum alpha, a new cross-valdiation graph was built.

cvelastic.1 <- cv.glmnet(train$x2, train$y, type.measure="mse", nfolds=10, alpha = c(db_net2$bestTune[,1]), family = "gaussian")

cvelastic.1$name <- "CV Error from Optimum Alpha"

plot(cvelastic.1)

# Here all the lambda values were extracted from the opitmum alpha cross validation. 

elastic.lambdas <- cvelastic.1$lambda

## Predictions 

prd.sps.e1 <- predict(cvelastic.1,newx=train$x2,s=c(cvelastic.1$lambda) )

mse.sps.set.e1 <- apply(prd.sps.e1,2,mse, train$y)

# After a series of calculation the cbind function will tell you the all the mean sqaure values that were generated by the lambda values from the optimum alpha that was selected. 

# Here we can take a look at mean square error values for all the lambda values that were extracted from the optimum alpha generated by the new cross-validation 
cbind(mse.sps.set.e1,cvelastic.1$lambda )

# This is mean sqaure error plot to get a better understanding of the mean sqaure errors and see if there is any strcuture such as if they are normalized. 

plot(log(cvelastic.1$lambda),mse.sps.set.e1,ylab = "Mean Square Error", xlab = "Log(Lambda) of Optimum Alpha", main = "Mean Square Error Plot of Lambda extracted from the Optimum Alpha for Train Set")

#Once that we have determined that lambda.min generated by the opitmum alpha is potentially the best model  The predict function was used to get the response values y for the training set. 

cvelastic.predict <- predict(cvelastic.1, s = c(cvelastic.1$lambda.min), newx = train$x2)

# This is where we have created the coefficients plot and path plot to get a better understanding of how much supression has it caused. 

cvela <- glmnet(train$x2, train$y,lambda=cvelastic.1$lambda, alpha= c(db_net2$bestTune[,1]))
cvela 
coefpath(cvela)
coefplot(cvela, lambda = cvelastic.1$lambda.min, sort = "magnitude")

# This will display all the response values for y for the test set for lambda values that were generated due to search grid

cvelastic.predict.1 <- predict(cvelastic, s = c(lambdas), newx = test$x2)

# The View() function will allow for better visualization  of response values for each model produced by the search grid 

View(cvelastic.predict.1)

cvela.1 <- glmnet(test$x2, test$y,lambda=lambdas)
coefpath(cvela.1)

# Here we have calculated mean square errors for all the models generated by the search grid. You can get a proper understanding of them through the View() function

mse.sps.set.e2 <- apply(cvelastic.predict.1,2,mse, test$y)

view.mse.e <- cbind(mse.sps.set.e2, lambdas)

plot(log(lambdas), mse.sps.set.e2)

View(view.mse.e)

# Observing from the values generated above the most optimum lambda value is 3.5 as it has the lowest difference is in between the mean square values. This is different from what was expected from the training set which we thought was lambda.min generated by the cross-validation. 

# This the repeat of the coefplot() function in order to show you the differences between the best model of coefficents produced by two best models. from training versus testing

# The the first one shows the testing model

coefplot(cvela.1, 3.5, sort = "magnitude", title = "Coefficient Plot for Testing Set for Elastic" )

# The second one shows the traininig model 
coefplot(cvela, lambda = cvelastic.1$lambda.min, sort = "magnitude", title = "Coefficient Plot for Training Set for Elastic")

#Overall the best model selected to be by observing both the training set and testing set is the lasso regression model. Due to the fact that the MSE is low and there isn't a extreme amount of difference between the importance of the coeffcients when it comes to both the training set and testing set.  



