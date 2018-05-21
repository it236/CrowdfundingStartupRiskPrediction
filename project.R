data <- read.csv("2018.csv", header = TRUE, sep = ",")
sum(is.na(data))
dim(data)
data <- na.omit(data)

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

library(nnet)

d1 <- with(data, data.frame(class.ind(state))[,1:5])
data[c(colnames(d1))] <- d1[c(colnames(d1))]


dCate <- with(data, data.frame(class.ind(category))[,1:158])
data[c(colnames(dCate))] <- dCate[c(colnames(dCate))]


dMCate <- with(data, data.frame(class.ind(main_category))[,1:14])
data[c(colnames(dMCate))] <- dMCate[c(colnames(dMCate))]

dSt <- with(data, data.frame(class.ind(state))[,1:5])
data[c(colnames(dSt))] <- dSt[c(colnames(dSt))]

dCo <- with(data, data.frame(class.ind(country))[,1:21])
data[c(colnames(dCo))] <- dCo[c(colnames(dCo))]

sum(is.factor(data))

data <- data[,!(colnames(data) %in% c("category","main_category","state","country"))]

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




