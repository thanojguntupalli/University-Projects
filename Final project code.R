#Final project code
#life expectancy
lifeExpectancy <- read.csv("C:\\Loyola University\\Applied Regression Analysis\\project\\LifeExpectancyData.csv")
head(lifeExpectancy)

#EDA

#plotting a histogram  of life expectancy
hist(lifeExpectancy$Life.expectancy , main = "Life Expectancy Distribution", col = "purple",
     xlab="Life expectancy",
     xlim=c(35,90),
     breaks = 15)

#boxplot life expectancy vs status
boxplot(lifeExpectancy$Life.expectancy~lifeExpectancy$Status,data=lifeExpectancy,
        main="Life expectancy vs Status",
        xlab="Status", ylab="Life expectancy",
        col = c("orange","red"))

# summary of lifeExpectancy vs Status
summary(lifeExpectancy$Life.expectancy~lifeExpectancy$Status)
tapply(lifeExpectancy$Life.expectancy, lifeExpectancy$Status, summary)

# EDA for missing data impute-univariate analysis
dev.off(dev.list()["RStudioGD"]) 
par(mfrow=c(3,5))
boxplot(lifeExpectancy$Life.expectancy,
        ylab = "Life Expectancy",
        main = "Boxplot of Life Expectancy",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$Adult.Mortality,
        ylab = "Adult Mortality",
        main = "Boxplot of Adult Mortality",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$Alcohol,
        ylab = "Alcohol",
        main = "Boxplot of Alcohol",
        col= "lightBlue",
        outcol="Red")
boxplot(lifeExpectancy$Hepatitis.B,
        ylab = "Hepatitis B",
        main = "Boxplot of Hepatitis B",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$BMI,
        ylab = "BMI",
        main = "Boxplot of BMI",
        col= "lightBlue",
        outcol="red")
boxplot(lifeExpectancy$Polio,
        ylab = "Polio",
        main = "Boxplot of Polio",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$Total.expenditure,
        ylab = "Total Expenditure",
        main = "Boxplot of Total Expenditure",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$Diphtheria,
        ylab = "Diphteria",
        main = "Boxplot of Diphteria",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$GDP,
        ylab = "GDP",
        main = "Boxplot of GDP",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$Population,
        ylab = "Population",
        main = "Boxplot of Population",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$thinness..1.19.years,
        ylab = "Thinness 1-19 years",
        main = "Boxplot of Thinness for 1-19 years old",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$thinness.5.9.years,
        ylab = "Thinness 5-9 years",
        main = "Boxplot of Thinness for 5-9 years old",
        col= "orange",
        outcol="red")
boxplot(lifeExpectancy$Income.composition.of.resources,
        ylab = "Income Composition",
        main = "Boxplot of Income Composition",
        col= "lightBlue",
        outcol="red")
boxplot(lifeExpectancy$Schooling,
        ylab = "Schooling",
        main = "Boxplot of Schooling",
        col= "orange",
        outcol="red")

dev.off(dev.list()["RStudioGD"]) 

#plot of life.expectancy versus Adult.Mortality
plot(y = lifeExpectancy$Life.expectancy,
     x = lifeExpectancy$Adult.Mortality,
     main = "LIFE EXPECTANCY VS ADULT MORTALITY",
     xlab = "ADULT MORTALITY",
     ylab = "LIFE EXPECTANCY",
     pch=18,
     col = "lightblue")

library(ggplot2)
ggplot(data = lifeExpectancy, 
       aes(x = Adult.Mortality, y = Life.expectancy, col = Status)) + geom_point()


#remove unnecessary variables required for our analysis
lifeExpectancy <- subset(lifeExpectancy, select = -c(Country, Year))

# converting categorical variable to factors
lifeExpectancy$Status <- as.factor(lifeExpectancy$Status)


# Finding missing values
missing_counts <- data.frame(Predictor = colnames(lifeExpectancy),
                             Nullcount=sapply(lifeExpectancy, function(x) sum(is.na(x))))

missing_counts[order(-missing_counts$Nullcount),]

# checking descriptive statistics of variables
summary(lifeExpectancy)

# Imputation of missing variables
#mean imputation
lifeExpectancy$Alcohol[is.na(lifeExpectancy$Alcohol)] <- mean(lifeExpectancy$Alcohol,  na.rm = TRUE)
lifeExpectancy$BMI[is.na(lifeExpectancy$BMI)] <- mean(lifeExpectancy$BMI,  na.rm = TRUE)
lifeExpectancy$Income.composition.of.resources[is.na(lifeExpectancy$Income.composition.of.resources)] <- mean(lifeExpectancy$Income.composition.of.resources,  na.rm = TRUE)

#median imputation
lifeExpectancy$Life.expectancy[is.na(lifeExpectancy$Life.expectancy)] <- median(lifeExpectancy$Life.expectancy,  na.rm = TRUE)
lifeExpectancy$Adult.Mortality[is.na(lifeExpectancy$Adult.Mortality)] <- median(lifeExpectancy$Adult.Mortality,  na.rm = TRUE)
lifeExpectancy$Hepatitis.B[is.na(lifeExpectancy$Hepatitis.B)] <- median(lifeExpectancy$Hepatitis.B,  na.rm = TRUE)
lifeExpectancy$Polio[is.na(lifeExpectancy$Polio)]  <- median(lifeExpectancy$Polio,  na.rm = TRUE)
lifeExpectancy$Diphtheria[is.na(lifeExpectancy$Diphtheria)] <- median(lifeExpectancy$Diphtheria,  na.rm = TRUE)
lifeExpectancy$Total.expenditure[is.na(lifeExpectancy$Total.expenditure)] <- median(lifeExpectancy$Total.expenditure,  na.rm = TRUE)
lifeExpectancy$GDP[is.na(lifeExpectancy$GDP)] <- median(lifeExpectancy$GDP,  na.rm = TRUE)
lifeExpectancy$Population[is.na(lifeExpectancy$Population)] <- median(lifeExpectancy$Population,  na.rm = TRUE)
lifeExpectancy$thinness..1.19.years[is.na(lifeExpectancy$thinness..1.19.years)] <- median(lifeExpectancy$thinness..1.19.years,  na.rm = TRUE)
lifeExpectancy$thinness.5.9.years[is.na(lifeExpectancy$thinness.5.9.years)] <- median(lifeExpectancy$thinness.5.9.years,  na.rm = TRUE)
lifeExpectancy$Schooling[is.na(lifeExpectancy$Schooling)] <- median(lifeExpectancy$Schooling,  na.rm = TRUE)

summary(lifeExpectancy)

#checking correlation between variables
corr <- round(cor(subset(lifeExpectancy, select =-c(Status))), 3)
library(corrplot)
corrplot(corr,type="full",method="circle",title="Correlation plot between variables",mar=c(0.1,0.1,0.1,0.1))

# regression among predictors
lm.model <- lm(Life.expectancy ~.,data=lifeExpectancy)
summary(lm.model)
X <- model.matrix(lm.model)[,-1]
for(i in 1:19){
  r2 <- summary(lm(X[,i]~X[,-i]))$r.squared
  cat(colnames(X)[i], '\t', r2, '\n')
}

#checking vif of predictors
library(car)
data.frame(vif(lm.model))
# removed variables with vif greater than 5
#infant.deaths and under.five.deaths., GDP and percentage.expenditure., and thinness..1.19.years and thinness.5.9.years.



#model building
modelle <- lm(Life.expectancy~. -under.five.deaths -percentage.expenditure -thinness.5.9.years,data=lifeExpectancy)
summary(modelle)
step(modelle)
#we get 15 significant predictors

# random split the data into 80% training and 20% test
set.seed(2022)
index.train <- sample(1:dim(lifeExpectancy)[1], 0.8 * dim(lifeExpectancy)[1])
data.train <- lifeExpectancy[index.train,]
data.test <- lifeExpectancy[-index.train,]

# fit a linear model on the training set
lm.model <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths +  Hepatitis.B  + Measles+BMI+Polio+Total.expenditure
               +Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+Income.composition.of.resources+Schooling+Status, 
               data=data.train)

# predict on the test set
yhat.test <- predict(lm.model, data.test)

# calculate test MSE
y.test <- data.test$Life.expectancy
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test

summary(lm.model)

# checking linear model assumptions
plot(lm.model$fitted.values,lm.model$residuals)
qqnorm(lm.model$residuals)
qqline(lm.model$residuals)


# applying square root transform to the model 
lm.modeltransform <- lm(sqrt(Life.expectancy) ~ sqrt(Adult.Mortality) + infant.deaths +  Hepatitis.B  + Measles+BMI+Polio+Total.expenditure
                        +Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+Income.composition.of.resources+Schooling+Status, 
                        data=data.train)

qqnorm(lm.modeltransform$residuals)
qqline(lm.modeltransform$residuals)

# applying log transform to the model 
lm.modeltransform <- lm(log(Life.expectancy) ~ log(Adult.Mortality) + infant.deaths +  Hepatitis.B  + Measles+log(BMI)+Polio+log(Total.expenditure)
                        +Diphtheria+HIV.AIDS+log(GDP)+log(Population+thinness..1.19.years)+Income.composition.of.resources+Schooling+Status, 
                        data=data.train)

qqnorm(lm.modeltransform$residuals)
qqline(lm.modeltransform$residuals)

# influential observations
lm.model <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths +  Hepatitis.B  + Measles+BMI+Polio+Total.expenditure
               +Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+Income.composition.of.resources+Schooling+Status, 
               data=data.train)
beta.change <- dfbeta(lm.model)

#7 fold cross validation

set.seed(2022)

# randomly shuffle the index
index.random <- sample(1:dim(lifeExpectancy)[1])

# split the data (index) into 7 folds 
groups <- cut(1:dim(lifeExpectancy)[1], 7, labels = FALSE)
index.fold <- split(index.random, groups)

# an empty vector to save individual MSE
MSEs <- c()

# 7-fold cross-validation
for(index.test in index.fold){
  
  # create training and test set
  data.test <- lifeExpectancy[index.test,]
  data.train <- lifeExpectancy[-index.test,]
  
  # fit a linear model on the training set
  lm.model <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths +  Hepatitis.B  + Measles+BMI+Polio+Total.expenditure
                 +Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+Income.composition.of.resources+Schooling+Status, 
                 data=data.train)
  
  # predict on the test set
  yhat.test <- predict(lm.model, data.test)
  
  # calculate test MSE
  y.test <- data.test$Life.expectancy
  MSE.test <- mean((y.test - yhat.test)^2)
  MSEs <- c(MSEs, MSE.test)
}
# plot 7 MSEs
plot(1:7, MSEs, type='b', col='red', xlab='Fold', ylab='MSE')

# Average 7 MSEs
MSE.test <-mean(MSEs)
# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test


# t-test on lifeexpectancy between developed and developing countries
t.test(life.data_developed$Life.expectancy,life.data_developing$Life.expectancy)

#checking the factors effecting life expectancy in developed and developing countries separately
#splits dataset by different Status character.
life.data_split<-split(lifeExpectancy,lifeExpectancy$Status)
str(life.data_split)

#Separates datasets based on Developed/Developing
life.data_developing<-life.data_split$Developing
life.data_developed<-life.data_split$Developed

summary(life.data_developing)
summary(life.data_developed)

life.data_developing <- subset(life.data_developing, select = -c(Status))
life.data_developed <- subset(life.data_developed, select = -c(Status))


#Building model with only developing countries records
developing_model<- lm(Life.expectancy ~. -under.five.deaths -percentage.expenditure -thinness.5.9.years,data=life.data_developing)
summary(developing_model)
step(developing_model)

# significant predictors are
# Adult.Mortality  infant.deaths Alcohol  Hepatitis.B  Measles  BMI  Polio  Total.expenditure Diphtheria  HIV.AIDS  GDP  Income.composition.of.resources  Schooling  

#Building model with only developed countries records
developed_model<- lm(Life.expectancy ~. -under.five.deaths -percentage.expenditure -thinness.5.9.years,data=life.data_developed)
summary(developed_model)
step(developed_model)

# significant predictors are
#Adult.Mortality  Alcohol BMI  Total.expenditure    Diphtheria  GDP thinness..1.19.years  Income.composition.of.resources  




