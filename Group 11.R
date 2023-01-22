#######################################
### R SCRIPT FOR STATISTICS PROJECT ###
#######################################

#Purpose : Make our new health insurance firm competitive by leveraging patients' data 
#Authors : Wai Kong Ng, Johnas Chami, 


### 1. Install and load packages ###
install.packages("ggplot2")
install.packages('caTools')
library(tidyverse)
library(ggplot2)
library(caTools)


### 2. Create a dataset from our dataframe ###
data <- read.csv("C:\\Users\\jcamillius\\mydata\\health_insurance.csv")


### 3. Data Exploration ###
data
summary(data)   # Checking missing data and get the summary of our data

# Converting no. of children to catogorical variable
data$children=cut(data$children,breaks = c(0,1,2,3,4,5,6),labels = c("0children","1children","2children","3children","4children","5children"),right = FALSE)

head(data)   # Get to know the first five rows of our data 

tail(data)   # Get to know the last five rows of our data


### 3. Relation variables ###

# A. Age vs Charges 
options(warn=-1)
ggplot(data=data,mapping=aes(x=data$age,y=data$charges))+geom_point(col="green")+geom_smooth(col="red")+labs(title="charges vs age plot",x="age",y="charges")

# Plot of age and charges incorporating sex 
ggplot(data=data,aes(x=data$age,y=data$charges,colour=data$sex))+geom_point()+geom_smooth()+labs(title = " charges vs age incorporating sex",x="age",y=" charges")

# Plot of age and charges incorporating children or no. of dependents 
ggplot(data=data,aes(x=data$age,y=data$charges,colour=data$children))+geom_point()+geom_smooth()+labs(title = " charges vs age incorporating childern",x="age",y=" charges")

# Plot of charges and age incorporating region 
ggplot(data=data,mapping = aes(x=data$age,y=data$charges,colour=data$region))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges and age incoparating region ",x="age",y="charges")


# Plot of charges and age incorporating smoker status
ggplot(data=data,mapping = aes(x=data$age,y=data$charges,colour=data$smoker))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges and age incorporating the effect of smoking status ",x="age",y="charges")

cor(data$age,data$charges) # Correlation between age and charges

# B. BMI vs Charges 
# Plot of charges vs bmi using sex as an explanatory variable
ggplot(data=data,aes(x=data$bmi,y=data$charges,colour=data$sex))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges vs bmi and taking sex as an explanatory variable",x="bmi",y="charges")

# Plot of charges vs bmi using childern as an explanatory variable
ggplot(data=data,aes(x=data$bmi,y=data$charges,colour=data$children))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges vs bmi and taking no. of childern as an explanatory variable",x="bmi",y="charges")

# Plot of charges vs bmi using smoker  as an explanatory variable
ggplot(data=data,aes(x=data$bmi,y=data$charges,colour=data$smoker))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges vs bmi and taking smoker as an explanatory variable",x="bmi",y="charges")

# Plot of charges vs bmi using region  as an explanatory variable
ggplot(data=data,aes(x=data$bmi,y=data$charges,colour=data$region))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges vs bmi and taking region as an explanatory variable",x="bmi",y="charges")

# C. Charges vs Catogorical variables
ggplot(data=data,mapping = aes(x=1:NROW(data$charges),y=data$charges,colour=data$sex))+geom_point()+geom_smooth()+labs(title="plot of charges including sex as factor variable",x="frequency",y="charges")

# Charges  vs No. of childern 
ggplot(data=data,mapping = aes(x=1:NROW(data$charges),y=data$charges,colour=data$children))+geom_point()+geom_smooth(se=F)+labs(title="plot of charges including children as factor variable",x="frequency",y="charges")

# Charges vs Smoker status
ggplot(data=data,mapping = aes(x=1:NROW(data$charges),y=data$charges,colour=data$smoker))+geom_point()+geom_smooth()+labs(title="plot of charges including smoker as factor variable",x="frequency",y="charges")

# Charges vs Region
ggplot(data=data,mapping = aes(x=1:NROW(data$charges),y=data$charges,colour=data$region))+geom_point()+geom_smooth()+labs(title="plot of charges including smoker as factor variable",x="frequency",y="charges")

# D. Variables with high interactions
# Charges vs Age incorporating smoker status
ggplot(data=data,mapping = aes(x=data$age,y=data$charges,colour=data$smoker))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges and age incorporating the effect of smoking status ",x="age",y="charges")

# Charges vs bmi using smoker  as an explanatory variable
ggplot(data=data,aes(x=data$bmi,y=data$charges,colour=data$smoker))+geom_point(size=1)+geom_smooth()+labs(title = "plot of charges vs bmi and taking smoker as an explanatory variable",x="bmi",y="charges")

### 4. Modelling using Linear Regression ###
ggplot(data=data,aes(data$charges))+geom_histogram(mapping = aes(y=stat(density)),bins = 35,colour="black",fill="green")+geom_density(col="red",lwd=2,lty=1)+labs(title = "histogram of charges with density curve ",x="charges")

# A. Logarithmic transformation 
ggplot(data=data,aes(log(data$charges)))+geom_histogram(mapping = aes(y=stat(density)),bins = 35,colour="black",fill="green")+geom_density(col="red",lwd=2,lty=1)+labs(title = "histogram of log(charges) with density curve ",x="charges")

qqnorm(log(data$charges))
qqline(log(data$charges),lwd=3,lty=2,col="green") # Use qqplot to check how well the normalization process performed

boxplot(log(data$charges)) # Find outliers in charges 

# B. Split Training and Test data set
set.seed(9669) # Setting arbitrary seed to 9669
library(caTools)
split=sample.split(data,SplitRatio = 0.8)
data1=subset(data,split==TRUE)
data2=subset(data,split==FALSE)

model=lm(log(charges)~age+sex+bmi+children+smoker+region+age:smoker+bmi:smoker+I(log(age))+I(log(bmi))+age:children+age:region+smoker:children,data = data1)
model
summary(model)

#removing the observation which are creating large residuals 
data=data[c(-431,-398,-103),]

data=data[c(-354,-1132,-514),]

data=data[c(-521,-219,-4),]

data=data[c(-1020,-338,-1032),]

data=data[c(-1178,-525,-1094),]

data=data[c(-1317,-545,-304),]

data=data[c(-1199,-1147,-461),]

data=data[c(-421,-945,-1177),]

# Re splitting the data set again 
set.seed(9625) #setting arbitrary seed to 9625
library(caTools)
split=sample.split(data1,SplitRatio = 0.8)
data1=subset(data,split==TRUE)
data2=subset(data,split==FALSE)

# Run the model again with new data sets
model=lm(log(charges)~age+sex+bmi+children+smoker+region+age:smoker+bmi:smoker+I(log(age))+I(log(bmi))+age:children+age:region+smoker:children,data = data1)
model
summary(model)

# C. Residual analysis of model
# a. Residual vs Fitted plot 
plot(model,1,lwd=2,lty=2,col="green")

# b. Q-Q plot 
plot(model,2,col="blue")

# c. Scale location graph 
plot(model,3,lwd=3,lty=2,col="yellow")

# D. Testing the Model 
newdata=data2[,-7] # Testing of model for data2( test data )
newdata[c(1:6),]

pv=predict(model,newdata = newdata)

print('total variability explained by model ') # Total variability explained by model 
sum((pv-mean(log(data2$charges)))^(2))

print('total variability of data2')

sum((log(data2$charges)-mean(log(data2$charges)))^(2)) # Total variability of data2

# Proportion of variability explained by model on test data
print('proportion of variability explained by model on test data or R^(2) of model')

sum((pv-mean(log(data2$charges)))^(2))/sum((log(data2$charges)-mean(log(data2$charges)))^(2))

# E. Confidence interval for train and test data 
head(predict(model,data1,interval="predict")) # Train data 
head(predict(model,data2,interval="predict")) # Training data


### MAKE HEALTH INSURANCE GREAT AGAIN ###
