
# The data set is taken from AnalyticsVidya.com 
# Now to convert the data set into proper table , use read.csv() function
loan_data <- read.csv("train_u6lujux_cvtuz9i.csv")
# To know the data type of each variable use str() function
str(loan_data)
# In the data set you will observe missing values in some columns
# To remove those missing values we use KNN imputation
loan_data2 <- loan_data
his <- preProcess(loan_data2[,-13],method = "knnImpute")
pre <- predict(his,loan_data2[,-13])
loan_data3 <- pre
# After applying KNN imputation you will observe no NA values, but your data has been preprocessed with center and scaling i.e the mean of centered and scaled data is 0 and standard deviation is 1
# To convert it back into the data that we got, use, 
av7 <- mean(loan_data$ApplicantIncome)
sd7 <- sd(loan_data$ApplicantIncome)
for(i in 1:614){
  loan_data3[i,7] <- (loan_data3[i,7] * sd7) + av7
}
av8 <- mean(loan_data$CoapplicantIncome)
sd8 <- sd(loan_data$CoapplicantIncome)
for(i in 1:614){
  loan_data3[i,8] <- (loan_data3[i,8] * sd8) + av8
}

sum9 <- 0
squ9 <- 0
avg9 <- 0
sd9 <- 0
count9 <- 0
for(i in 1:614){
  if(!is.na(loan_data[i,9])){
    count9 <- count9 + 1
    sum9 <- sum9 + loan_data[i,9]
  }
}
avg9 <- sum9/count9
for(i in 1:614){
  if(!is.na(loan_data[i,9])){
    ae <- (loan_data[i,9] - avg9)^2
    squ9 <- squ9 + ae
  }
}
sd9 <- sqrt(squ9/(count9 - 1))
for(i in 1:614){
  loan_data3[i,9] <- (loan_data3[i,9] * sd9) + avg9
}

sum10 <- 0
squ10 <- 0
avg10 <- 0
sd10 <- 0
count10 <- 0
for(i in 1:614){
  if(!is.na(loan_data[i,10])){
    count10 <- count10 + 1
    sum10 <- sum10 + loan_data[i,10]
  }
}
avg10 <- sum10/count10
for(i in 1:614){
  if(!is.na(loan_data[i,10])){
    ae <- (loan_data[i,10] - avg10)^2
    squ10 <- squ10 + ae
  }
}
sd10 <- sqrt(squ10/(count10 - 1))
for(i in 1:614){
  loan_data3[i,10] <- (loan_data3[i,10] * sd10) + avg10
}

sum11 <- 0
squ11 <- 0
avg11 <- 0
sd11 <- 0
count11 <- 0
for(i in 1:614){
  if(!is.na(loan_data[i,11])){
    count11 <- count11 + 1
    sum11 <- sum11 + loan_data[i,11]
  }
}
avg11 <- sum11/count11
for(i in 1:614){
  if(!is.na(loan_data[i,11])){
    ae <- (loan_data[i,11] - avg11)^2
    squ11 <- squ11 + ae
  }
}
sd11 <- sqrt(squ11/(count11 - 1))
for(i in 1:614){
  loan_data3[i,11] <- (loan_data3[i,11] * sd11) + avg11
}

for(i in 1:614){
 if(loan_data3[i,11] == 1.000000e+00){
   loan_data3[i,11] <- 1
 }
 else{
   loan_data3[i,11] <- 0
 }
}
loan_data3$Loan_Status <- loan_data$Loan_Status
# Now you will observe that the data has been converted back to its original form
View(loan_data3)
# Now convert the data type back to the data type of original data
loan_data3[,7] <- as.integer(loan_data3[,7])
loan_data3[,9] <- as.integer(loan_data3[,9])
loan_data3[,10] <- as.integer(loan_data3[,10])
# You can draw various plots using qplot() function which is available in ggplot2 package
qplot(Gender,Loan_Status,data = loan_data3)
qplot(Married,Loan_Status,data = loan_data3)
qplot(Dependents,Loan_Status,data = loan_data3)
qplot(Education,Loan_Status,data = loan_data3)
qplot(Self_Employed,Loan_Status,data = loan_data3)
qplot(ApplicantIncome,Loan_Status,data = loan_data3)
# In the above plot you will observe that when ApplicantIncome is between 40000-70000 Loan_Status is yes and when ApplicantIncome is >80000 Loan_Status is no
qplot(CoapplicantIncome,Loan_Status,data = loan_data3)
# In the above plot You will observe that when CoapplicantIncome is >20000 then Loan_Status is no and when CoapplicantIncome is between 10000-15000 then Loan_Status is also no
qplot(LoanAmount,Loan_Status,data = loan_data3)
# In the above plot you will observe that when LoanAmount >=600 then Loan_Status is yes
qplot(Loan_Amount_Term,Loan_Status,data = loan_data3)
qplot(Credit_History,Loan_Status,data = loan_data3)
qplot(Property_Area,Loan_Status,data = loan_data3)
# The above plots pdf's file have also been uploaded
# Notice that Loan_ID variable has unique values, so we can remove that variable to have better prediction
loan_data4 <- loan_data3[,-1]
# Now use set.seed() function so that your result does not change drastically when you run your code many times
set.seed(3240)
# Now let us break our data set into 2 parts training and testing use createDataPartition() function which is available in caret package
intrain <- createDataPartition(y = loan_data4$Loan_Status,p = 0.7,list = FALSE)
# in above command p=0.7 means we are breaking the data set in the ratio of 70:30
training <- loan_data4[intrain,]
testing <- loan_data4[-intrain,]
# training will be 70% of our data and testing will be 30%
# training will be used only for model fitting and testing will be used for prediction

# Now let us apply some Machine Learning Algorithms
# First let us apply decision tree algorithm
# use train() function, in that use method = "rpart" and subsampling is of crossvalidation
mod1 <- train(Loan_Status~.,method = "rpart",data = training,trControl = trainControl(method = "cv"))
# To know details of above model use below command 
mod1
# To make a plot of above splitting use fancyRpartPlot() function which is available in rattle package
fancyRpartPlot(mod1$finalModel)
mod1$finalModel
# Now let us apply prediction of above model fitting using predict() function
pre1 <- predict(mod1,newdata = testing)
# Now let us make a table of how much we got right
tr1 <- table(pre1,testing$Loan_Status)
# To know what is the percentage of our corret prediction use
cr1 <- ((tr1[1,1] + tr1[2,2])/(tr1[1,1]+tr1[1,2]+tr1[2,1]+tr1[2,2]))*100

# Now let us apply random forest algorithm , just use method = "rf"
mod2 <- train(Loan_Status~.,method = "rf",data = training,trControl = trainControl(method = "cv"),prox = TRUE)
pre2 <- predict(mod2,newdata = testing)
tr2 <- table(pre2,testing$Loan_Status)
cr2 <- ((tr2[1,1] + tr2[2,2])/(tr2[1,1]+tr2[1,2]+tr2[2,1]+tr2[2,2]))*100

# To get a plot of random forest use varImpPlot() function
mod3 <- randomForest(Loan_Status~.,data = training,ntree = 2000)
varImpPlot(mod3)
pre3 <- predict(mod3,newdata = testing)
tr3 <- table(pre3,testing$Loan_Status)

# To apply gradient boosting use method = "gbm" in train() function , for using GBM packes like gbm,survival,splines,parallel,plyr shoul be installed
mod4 <- train(Loan_Status~.,method = "gbm",data = training,trControl = trainControl(method = "cv"),verbose = FALSE)
pre4 <- predict(mod4,newdata = testing)
tr4 <- table(pre4,testing$Loan_Status)
cr4 <- ((tr4[1,1] + tr4[2,2])/(tr4[1,1]+tr4[1,2]+tr4[2,1]+tr4[2,2]))*100

# Now let us apply GLM algorithm using method = "GLM" in train() function
mod5 <- train(Loan_Status~.,method = "glm",data = training,trControl = trainControl(method = "cv"))
pre5 <- predict(mod5,newdata = testing)
tr5 <- table(pre5,testing$Loan_Status)
cr5 <- ((tr2[1,1] + tr5[2,2])/(tr5[1,1]+tr5[1,2]+tr5[2,1]+tr5[2,2]))*100

# Now let us apply combined prediction i.e combining 2 models, here we are combining GLM and random forest
inbuild <- createDataPartition(loan_data4$Loan_Status,p=0.7,list = FALSE)
builddata <- loan_data4[inbuild,]
validation <- loan_data4[-inbuild,]
# Now in builddata we will again create partition into training and testing dataset
intrain2 <- createDataPartition(builddata$Loan_Status,p=0.7,list = FALSE)
training2 <- builddata[intrain2,]
testing2 <- builddata[-intrain2,]
mod7 <- train(Loan_Status~.,method = "glm",data = training2,trControl = trainControl(method = "cv"))
mod8 <- train(Loan_Status~.,method = "rf",data = training2,trControl = trainControl(method = "cv"))
pred7 <- predict(mod7,newdata = testing2)
pred8 <- predict(mod8,newdata = testing2)
# Now we will combine above predictors and make a new data frame
predf <- data.frame(pred1=pred7,pred2=pred8,Result = testing2$Loan_Status)
# Now we will apply a combined model fitting on this new data frame
combmodfit <- train(Result~.,method = "gam",data = predf,trControl = trainControl(method = "cv"))
# In the above method="gam" shows that we are using Generalised Linear Model
combpred <- predict(combmodfit,predf)
# Now to see how what is the advantage of combined model prediction use the following command-
table(pred7,testing2$Loan_Status)
table(pred8,testing2$Loan_Status)
table(combpred,testing2$Loan_Status)

# Now apply above on validation data set
prev1 <- predict(mod7,validation)
prev2 <- predict(mod8,validation)
predf2 <- data.frame(pred1 = prev1,pred2 =  prev2,Result = validation$Loan_Status)
combpred2 <- predict(combmodfit,newdata = predf2)
trc <-  table(combpred2,validation$Loan_Status)
crc <- ((trc[1,1] + trc[2,2])/(trc[1,1]+trc[1,2]+trc[2,1]+trc[2,2]))*100

# Now you can see which model fitting gave you the best results.
