# Melissa Hunfalvay. 
# Last Revised: 7-20-2021
# Data 630
# pima_diabetes Dataset. 
# Assignment 4
# Professor Firdu

#Section 1: Preparation 

# Set the working directory 
setwd("/Users/melissahunfalvay/Documents/HUN/My Professional Development/Machine Learning Data 630/Assignments/Assignment 4") 

# Display the file names in the current working directory
dir()

# Read the data
pima_diabetes<-read.csv(file="pima_diabetes.csv", header=TRUE, sep=",", as.is = FALSE)

# Preview the data
View(pima_diabetes)

#Preview the first 6 rows
head(pima_diabetes)

#Install the packages
# install.packages("neuralnet")
library("neuralnet")
library ("NeuralNetTools")
library ("nnet")
#install.packages("devtools")
library(devtools)
require(caret)
require(e1071)

# Section 2: Data Exploration

#Check data types
str(pima_diabetes) # note int, num, and factors, no Identifier

# Explore the descriptive nature of all variables 
summary(pima_diabetes) #check descriptive statistics
head(pima_diabetes)# show first 6 rows

# Explore variable = preg, type = int
head(pima_diabetes$preg, 100) # show first 100 raw variables
summary (pima_diabetes$preg) # Descriptive Statistics
sd(pima_diabetes$preg) # Standard deviation 
names(sort(-table(Data_preg$preg)))[1] # Mode
hist(pima_diabetes$preg) # histogram for distribution
boxplot(pima_diabetes$preg) #boxplot for outliers

# Explore variable = plas, type = int
head(pima_diabetes$plas, 100) # show first 100 raw variables
table(pima_diabetes$plas) # Count the number of values within each category
summary (pima_diabetes$plas) # Descriptive Statistics
sd(pima_diabetes$preg) # Standard deviation 
names(sort(-table(Data_plas$plas)))[1] # Mode
hist(pima_diabetes$plas) # histogram for distribution
boxplot(pima_diabetes$plas) #boxplot for outliers

# Explore variable = pres, type = int
head(pima_diabetes$pres, 100) # show first 100 raw variables
table(pima_diabetes$pres) # Count the number of values within each category
summary (pima_diabetes$pres) # Descriptive Statistics
table(pima_diabetes$pres) # Count of unique values
sd(pima_diabetes$pres) # Standard deviation 
names(sort(-table(Data_pres$pres)))[1] # Mode
hist(pima_diabetes$pres) # histogram for distribution
boxplot(pima_diabetes$pres) #boxplot for outliers
sum(pima_diabetes$insu<60) # how many records less than 60?

# Explore variable = skin, type = int
head(pima_diabetes$skin, 100) # show first 100 raw variables
table(pima_diabetes$skin) # Count the number of values within each category
summary (pima_diabetes$skin) # Descriptive Statistics
table(pima_diabetes$skin)/length(pima_diabetes$skin) # Percentage
sd(pima_diabetes$skin) # Standard deviation 
names(sort(-table(Data_skin$skin)))[1] # Mode
hist(pima_diabetes$skin) # histogram for distribution
boxplot(pima_diabetes$skin) #boxplot for outliers

# Explore variable = insu, type = int
head(pima_diabetes$insu, 100) # show first 100 raw variables
table(pima_diabetes$insu) # Count the number of values within each category
summary (pima_diabetes$insu) # Descriptive Statistics
table(pima_diabetes$insu)/length(pima_diabetes$insu) # Percentage
sd(pima_diabetes$insu) # Standard deviation 
names(sort(-table(Data_insu$insu)))[1] # Mode
hist(pima_diabetes$insu) # histogram for distribution
boxplot(pima_diabetes$insu) #boxplot for outliers
sum(pima_diabetes$insu>240) # how many records above 240?
sum(pima_diabetes$insu<15) # how many records less than 15?
quantile (pima_diabetes$insu, 0.99) #99th percentile data

# Explore variable = mass, type = num
head(pima_diabetes$mass, 100) # show first 100 raw variables
table(pima_diabetes$mass) # Count the number of values within each category
summary (pima_diabetes$mass) # Descriptive Statistics
table(pima_diabetes$mass)/length(pima_diabetes$mass) # Percentage
sd(pima_diabetes$mass) # Standard deviation 
names(sort(-table(Data_mass$mass)))[1] # Mode
hist(pima_diabetes$mass) # histogram for distribution
boxplot(pima_diabetes$mass) #boxplot for outliers

# Explore variable = pedi, type = num
head(pima_diabetes$pedi, 100) # show first 100 raw variables
table(pima_diabetes$pedi) # Count the number of values within each category
summary (pima_diabetes$pedi) # Descriptive Statistics
table(pima_diabetes$pedi)/length(pima_diabetes$pedi) # Percentage
sd(pima_diabetes$pedi) # Standard deviation 
names(sort(-table(Data_pedi$pedi)))[1] # Mode
hist(pima_diabetes$pedi) # histogram for distribution
boxplot(pima_diabetes$pedi) #boxplot for outliers

# Explore variable = age, type = int
head(pima_diabetes$age, 100) # show first 100 raw variables
table(pima_diabetes$age) # Count the number of values within each category
summary (pima_diabetes$age) # Descriptive Statistics
table(pima_diabetes$age)/length(pima_diabetes$age) # Percentage
sd(pima_diabetes$age) # Standard deviation 
names(sort(-table(Data_age$age)))[1] # Mode
hist(pima_diabetes$age) # histogram for distribution
boxplot(pima_diabetes$age) #boxplot for outliers

# Explore variable = class, type = factor
head(pima_diabetes$class, 100) # show first 100 raw variables
table(pima_diabetes$class) # Count the number of values within each category
summary (pima_diabetes$class) # Descriptive Statistics
table(pima_diabetes$class)/length(pima_diabetes$class) # Percentage
barplot(table(pima_diabetes$class)) # barplot for visualization of classes

# Check if the data has missing values
colSums(is.na(pima_diabetes))

#Section 3: Data Pre-processing

# Change the categorical variables - n/a
# remove variables? n/a
# Remove factors with too many levels - n/a

# Data Sanity checks

# Pregnancy variable
# Check women with greater than 10 pregnancies against age.
summary (pima_diabetes$preg) # Descriptive Statistics - check range
boxplot(pima_diabetes$preg) # Check outliers
max(pima_diabetes$preg)
pima_diabetes[which(pima_diabetes$preg>10),"age"] # legitimate, no one is 20's or early 30's

# Plasma/Glucose Variable
# Is it legitimate to have a zero plasma/glucose level? What is a reasonable lowest number? below 54 mg/dL Ref: https://medlineplus.gov/ency/patientinstructions/000085.htm 
summary (pima_diabetes$plas) # Descriptive Statistics - check range
boxplot(pima_diabetes$plas) # Check outliers
table(pima_diabetes$plas) # Unique counts before
pima_diabetes = pima_diabetes[which(pima_diabetes$plas >50),] # remove the patients with less than 54 mg/dL
summary (pima_diabetes$plas) # Descriptive Statistics - check range
boxplot(pima_diabetes$plas) #after
table(pima_diabetes$plas) # Unique counts after

# Diastolic Blood Pressure: pres, type = int
table(pima_diabetes$pres) # Count the number of values within each category
summary (pima_diabetes$pres) # Descriptive Statistics
sum(pima_diabetes$pres<1) # how many records less than 1?
sum(pima_diabetes$pres>120) # how many records more than 120?
pima_diabetes = pima_diabetes[which(pima_diabetes$pres >0),] # remove the patients with zero
pima_diabetes = pima_diabetes[which(pima_diabetes$pres <121),] # remove the patients with zero
summary (pima_diabetes$pres) # Descriptive Statistics
sum(pima_diabetes$pres<1) # how many records less than 1?
sum(pima_diabetes$pres>120) # how many records more than 120?

# Remove variables
pima_diabetes$skin<-NULL # remove variable
pima_diabetes$insu<-NULL # remove variable
pima_diabetes$pedi<-NULL # remove variable

# Mass (BMI) Variable
table(pima_diabetes$mass) # Count the number of values within each category
summary (pima_diabetes$mass) # Descriptive Statistics
sum(pima_diabetes$mass<1) # how many records less than 1?
pima_diabetes = pima_diabetes[which(pima_diabetes$mass >0),] # remove the patients with zero
summary (pima_diabetes$mass) # Descriptive Statistics
sum(pima_diabetes$mass<1) # how many records less than 1?

# Change the class to +/-
pima_diabetes$class<-ifelse(pima_diabetes$class=="tested_positive",1,0) # if tested positive =1, if not then 0

#scale the variables
pima_diabetes[1:4]<-scale(pima_diabetes[1:4])

str(pima_diabetes)

# Section 4: Model development

#make sure that the result is reproducible
set.seed(12345)
#split the data into a training and test set
ind <- sample(2, nrow(pima_diabetes), replace = TRUE, prob = c(0.7, 0.3))
train.data <- pima_diabetes[ind == 1, ]
test.data <- pima_diabetes[ind == 2, ]
str(train.data)
str(test.data)
#Build the model. If you receive a warning, rerun the command.
nn<-neuralnet(formula = class~preg+plas+pres+mass+age, data = train.data, hidden=6, err.fct="ce", linear.output = FALSE)
#names command displays the available neural network properties
names(nn)

#Run the commands to display the network properties
nn$call                  # the command we ran to generate the model
nn$response[1:10]        # actual values of the dependent variable for first 10 records
nn$covariate [1:12,]     # input variables that were used to build the model for first 12 records
nn$model.list            # list dependent and independent variables in the model
nn$net.result[[1]][1:10] # display the first 10 predicted probabilities
nn$weights               # network weights after the last method iteration
nn$startweights          # weights on the first method iteration
nn$result.matrix         # number of trainings steps, the error, and the weights 


#Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict [1:10]

# confusion matrix for the training set
trainPred <- compute(nn, train.data[, 0:5])$net.result
trainPred<-apply(trainPred, c(1), round)
table(trainPred, train.data$class, dnn =c("Predicted", "Actual"))
mean(trainPred==train.data$class)
confusionMatrix(table(trainPred, train.data$class), dnn=c("predicted", "actual"))

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:5])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$class, dnn =c("Predicted", "Actual"))
mean(testPred==test.data$class)
confusionMatrix(table(testPred, test.data$class), dnn=c("predicted", "actual"))

# Visualize the model
plot(nn)                 # plot the network
plotnet(nn, circle_col="pink") #may change node color
# Relative importance for each variable; only for network with 1 hidden layer and one output
garson(nn)  
#Relative importance for each variable; the network may have >=1 hidden layers >=1 output
olden(nn)
#install.packages("devtools")
library("devtools")
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# may use different color for positive and negative weight connections
plot.nnet(nn, pos.col = "green", neg.col = "red", circle.col="orange")
#change the circle size
plot.nnet(nn, pos.col = "green", neg.col = "red", circle.col="orange", circle.cex=5, bord.col="purple")
