##### Install/Load the Libraries #####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(ggplot2)
library(psych)
library(dplyr)


##### Pull Adult Census Income from kaggle Website. Downloaded data called adult.csv will be submitted ####
income_data <- read.csv('adult.csv') # read provided dataset from https://github.com/l98033110/Havardx-Capstone
str(income_data)
income_data <- distinct(income_data) # remove duplicate rows

## Data Exploration
head(income_data)
income_data <- income_data %>% select(-fnlwgt) # fnlwgt stands for final weight which is not useful in prediction. Remove it.
income_data <- income_data %>% mutate(capital_net=capital.gain-capital.loss)%>%select(-capital.gain,-capital.loss)#combinte capital gain and loss columns into one column (capital_net) and remove gain and loss columns.
income_data %>% group_by(income) %>% tally() # check how many groups in income column, classification problem
#remove all rows with ?, add income_num column to 0/1 values based on income column, convert character to factor
income_data <- income_data %>% filter_all(all_vars(. !='?')) %>% mutate(income_num = ifelse(income == ">50K", 1, 0)) %>% mutate_if(is.character, as.factor)
# explore education.num/education columns
income_data %>% group_by(education) %>% summarize(mean = mean(income_num),n = n()) %>%arrange(desc(mean))
income_data %>% group_by(education.num) %>% summarize(mean = mean(income_num),n = n()) %>%arrange(desc(mean))
income_data <- income_data %>% select(-income_num) # remove income_num and income column is providing the same information
#Brief Conclusion: Higher education level may result in a higher possibility of well-paid employment.And education and education.num are the duplicate information for prediction.
income_data <- income_data %>% select(-education) # remove column education
# Age
age_hist <- income_data %>% ggplot(aes(x = age)) + geom_histogram(bins = 25, fill = "red") + 
labs(x = "Age", y= "Frequency", title = "Age Histogram")
print(age_hist)
age_income_density <- ggplot(income_data, aes(x = age, fill = income)) +
geom_density(alpha = 0.2) + labs(x = "Age",y = "Density",title = "Age Density Plot by Income Level")
print (age_income_density)
#Work Class
workclass_income_proportion <- ggplot(income_data, aes(x = workclass, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Workclass",y = "Observations",title = "Proportions of >50K in different work classes")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
print (workclass_income_proportion)
#Marital Status
marital_income_proportion <- ggplot(income_data, aes(x = marital.status, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Marital Status",y = "Observations",title = "Proportions of >50K in different marital statuses")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
print (marital_income_proportion)
#Relationship
relationship_income_proportion <- ggplot(income_data, aes(x = relationship, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Relationship",y = "Observations",title = "Proportions of >50K in different relationships")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
print (relationship_income_proportion)
income_data <- income_data %>% select(-relationship)
#Race
race_income_proportion <- ggplot(income_data, aes(x = race, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Race",y = "Observations",title = "Proportions of >50K in different races")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
print (race_income_proportion)
#Sex
sex_income_proportion <- ggplot(income_data, aes(x = sex, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Sex",y = "Observations",title = "Proportions of >50K in different races")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
print (sex_income_proportion)
#Occupation
occupation_income_proportion <- ggplot(income_data, aes(x = occupation, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Occupation",y = "Observations",title = "Proportions of >50K in different races")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
print (occupation_income_proportion)
#Capital_net (capital gan - capital loss)
capital_hist <- income_data %>% ggplot(aes(x = capital_net)) + geom_histogram(bins = 25, fill = "red") + 
  labs(x = "Capital_net", y= "Frequency", title = "capital Histogram")
print(capital_hist)
capital_income_density <- ggplot(income_data, aes(x = capital_net, fill = income)) +
  geom_density(alpha = 0.2) + labs(x = "Capital_net",y = "Density",title = "Capital_net Density Plot by Income Level") + xlim(-10000,25000)
print (capital_income_density)
income_data <-income_data%>% select(-capital_net)#remove capital_net column
#hours per week
hpw_hist <- income_data %>% ggplot(aes(x = hours.per.week)) + geom_histogram(bins = 25, fill = "red") + 
  labs(x = "Hours per week", y= "Frequency", title = "Hours per week Histogram")
print(hpw_hist)
hpw_income_density <- ggplot(income_data, aes(x = hours.per.week, fill = income)) +
  geom_density(alpha = 0.2) + labs(x = "Hours per week",y = "Density",title = "Hours-per-week Density Plot by Income Level")
print (hpw_income_density)
#Native country
native_income_proportion <- ggplot(income_data, aes(x = native.country, fill = income)) +
  geom_bar(alpha = 0.2) + labs(x = "Native Country",y = "Observations",title = "Proportions of >50K in different work classes")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
print (native_income_proportion)
native_income_proportion1 <- ggplot(income_data, aes(x = native.country, fill = income)) +
geom_bar(alpha = 0.2) + labs(x = "Native Country",y = "Observations",title = "Proportions of >50K in different work classes")+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(0,650) #To set y axis limit to see other native countries clearly
print (native_income_proportion1) 
# Test set will be 10% of data
set.seed(1000,sample.kind='Rejection')
test_index <- createDataPartition(y = income_data$income, times = 1, p = 0.1, list = FALSE)
trainset <- income_data[-test_index,]
testset <- income_data[test_index,]

#Decision Tree
library(rpart)
treeFit <- rpart(income ~. , data=trainset)
print(treeFit)
prediction2 <- predict(treeFit, newdata=testset, type='class')
acu2<-confusionMatrix(prediction2,testset$income)$overall[1]
acu2

#RandomForest
library(randomForest)
rfFit<- randomForest(income~.,data= trainset)
print(rfFit)
prediction1 <- predict(rfFit, newdata=testset, type='class')
acu1<-confusionMatrix(prediction1,testset$income)$overall[1]
acu1

###below codes are not run in my computer due to very very slow response.
#grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
#control <- trainControl(method="cv", number = 5)
#train_rf <- train(income ~ ., data = income_data,
#                  method = "rf",
#                  ntree = 150,
#                  trControl = control,
#                tuneGrid = grid,
#                  nSamp = 5000)
#ggplot(train_rf)
#train_rf$bestTune





