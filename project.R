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





