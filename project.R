data <- read.csv("2018.csv", header = TRUE, sep = ",")
sum(is.na(data))
dim(data)
data <- data[complete.cases(data),]


#Checking datatypes in each column
sapply(data, class)

#Removing ID column
data <- data[,colnames(data)!="ID"]

#Replacing column "name" with the length of the name string
data$name <- as.character(data$name)
data$name <- sapply(data$name,nchar)

colnames(data)[1] <- "nameLength"

# Converting deadline and launched to date type and then adding new column for campaign length and removing
# both columns
data$deadline <- as.Date(data$deadline,'%Y-%m-%d')
data$launched <- as.Date(data$launched,'%Y-%m-%d')
data["campLength"] <- data$deadline - data$launched

data$campLength <- as.integer(data$campLength)

data <- data[,!(colnames(data) %in% c("deadline","launched"))]

data <- data[,!(colnames(data) %in% c("currency","goal","usd.pledged"))]

data <- data[, colnames(data) != "pledged"]
sapply(data, class)

data$category <- droplevels.factor(data$category)
data$country <- droplevels.factor(data$country)
data$main_category <- droplevels.factor(data$main_category)
data$state <- droplevels.factor(data$state)

#Bar plot of startups by Country
barplot(-sort(-table(data$country)),main = "Startups by Country", xlab = "Number of Startups")

#Top 5 largest countries
names(-sort(-table(data$country))[1:5])

sum(data$country=="US")/length(data$country) 
#78% of crowdfunded startups listed on kickstarter are from the US

sum(data$country=="GB")/length(data$country) 
#8.9% of crowdfunded startups listed on kickstarter are from Great Britain

sum(data$country=="CA")/length(data$country) 
#3.9% of crowdfunded startups listed on kickstarter are from Canada


barplot(-sort(-table(data$main_category)),main = "Startups by Category", xlab = "Number of Startups") 
#Much more even spread than country

#Main_categories that account for 80% of the startups
names(-sort(-table(data$main_category))[1:8])

#Top 5 largest main categories in the US
names(-sort(-table(data$main_category[data$country=="US"]))[1:5])

#Since the category variable is just an expansion of the main_category predictor
#these 2 are bound to have a strong correlation and we need to remove one
#We chose to keep the category predictor because it has much more information
data <- data[,colnames(data) != "main_category"]
# y <- data[,colnames(data) == "state"]
library(nnet)

#Converting all categorical predictors to dummy variables

dCate <- with(data, data.frame(class.ind(category))[,1:158])
data[c(colnames(dCate))] <- dCate[c(colnames(dCate))]


# dMCate <- with(X, data.frame(class.ind(main_category))[,1:14])
# X[c(colnames(dMCate))] <- dMCate[c(colnames(dMCate))]

dCo <- with(data, data.frame(class.ind(country))[,1:21])
data[c(colnames(dCo))] <- dCo[c(colnames(dCo))]

sum(is.factor(data))

data <- data[,!(colnames(data) %in% c("category","country"))]

#Converting all int types to numeric to make them compatible with PCA
#and creating new data set with no state variable
data$campLength <- as.numeric(data$campLength)
data$nameLength <- as.numeric(data$nameLength)
data$backers <- as.numeric(data$backers)

data1 <- data[,colnames(data) != "state"]


#Calculating PC's for the data to see if data can be visualised well in 2D
#i.e. if PC1 and PC2 explain a significant variation in the data
pr.out <- prcomp(data1, scale=TRUE)

#Calculating the proportion of variance explained by each principal component
pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)

plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ",ylim=c(0,1),type='b')
#The principal component analysis can reduce dimenionsality but not useful for visualisation

#Splitting data1 into a train and test set to regress usd_real_pledged onto the other predctors
#besided state
train_ind <- sample(1:nrow(data1), (4/5)*nrow(data1))
data1_train <- data1[train_ind,]
data1_test <- data1[-train_ind,]

#Checking summary of the fit
summary(pledgedlm)
#Although some predictors do have a correlation with the pledged amount as suggested by the p-value
#for the F-test the linear model has an Rsquared score of 0.5492 from which we infer that a
#a linear model might not be the best suited to this data.

#Doing subset selection using backward selection
library(leaps)
regfit.pledged <- regsubsets(usd_pledged_real~.,data = data1_train,method = "forward")

#Checking summary of subset selection
fitsummary <- summary(regfit.pledged)
#Checking which one does best using BIC since it tends to favor smaller models
which.min(backfitsummary$bic)
plot(fitsummary$bic,xlab = "Number of Predictors",ylab = "BIC Value", main = "BIC vs Number of Predictors", type = "b")

#Checking the variance explained by the 8 predictor model and the RSS
fitsummary$rsq[8]
fitsummary$rss[8]
#Clearly even after feature selection and getting rid of the fear of overfitting the linear model
#is not adequate

#Converting all int types to numeric to make them compatible with PCA
#and creating new data set with no state variable
data$campLength <- as.numeric(data$campLength)
data$nameLength <- as.numeric(data$nameLength)
data$backers <- as.numeric(data$backers)

data2_success <- data[data$state %in% c("successful"),colnames(data) != "state"]

train_ind1 <- sample(1:nrow(data2_success), (4/5)*nrow(data2_success))
data2_success_normal <- data.frame(scale(data2_success[train_ind1,colnames(data2_success) != "usd_pledged_real"]))
data2_success_normal$usd_pledged_real <- data2_success$usd_pledged_real[train_ind1]
data2_success_normal_test <- data.frame(scale(data2_success[-train_ind1,colnames(data2_success) != "usd_pledged_real"]))
data2_success_normal_test$usd_pledged_real <- data2_success$usd_pledged_real[-train_ind1]

# Fitting a polynomial model with cross validation
library(boot)
cv.error2 = rep(0, 7) #13840.66 for first degree
for (i in 1:7) {
  glm.fit = glm(usd_pledged_real~poly(backers, i)+poly(usd_goal_real, i)+poly(campLength, i)+X3D.Printing+Camera.Equipment+Fabrication.Tools+ Flight+Gadgets+Gaming.Hardware+Hardware+Product.Design+Sound+Tabletop.Games+Technology+Video.Games+Wearables, data = data2_success_normal)
  cv.error2[i] = cv.glm(glmfit = glm.fit, data = data2_success_normal, K=10, cost = c)$delta[1]
}
plot(y=cv.error2,x=seq(1,7,by = 1), main = "K-Fold", type = "b")
cv.error2

# Fitting a generalized additive model (CV 14th 13061.06)
cv.error3 = rep(0, 20)
for (i in 1:20) {
  gam1 <- gam(usd_pledged_real~bs(backers,df = i)+bs(usd_goal_real,df=i)+bs(campLength,df = i)+X3D.Printing+Camera.Equipment+Fabrication.Tools+ Flight+Gadgets+Gaming.Hardware+Hardware+Product.Design+Sound+Tabletop.Games+Technology+Video.Games+Wearables,data = data2_success_normal)
  cv.error3[i] = cv.glm(glmfit = gam1, data = data2_success_normal, K=10, cost = c)$delta[1]
}
plot(y=cv.error3,x=seq(1,20,length.out = 20), main = "K-Fold", type = "b")
cv.error3
which.min(cv.error3)
length(cv.error3)
=======
#Fitting a linear regression model on the data
pledgedlm <- lm(usd_pledged_real~.,data = data1_train)