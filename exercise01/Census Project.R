dbPath <- 'C:/Users/matt/Dropbox/Documents/Resume and Applications/applications/rti/assessment/exercise01/exercise01.sqlite'

#install.packages("RSQLite")
#install.packages("gmodels")
#install.packages("plyr")

library(DBI) #Connection to a SQLLite database
library(gmodels) #Nice crosstabs with %s
library(plyr) #Recoding variables

#Connect to database
con <- dbConnect(RSQLite::SQLite(), dbPath)

#Get number of records to check connection
res <- dbSendQuery(con, "select count(*) from records")
fetch(res)
dbClearResult(res)

#Flatten database
joinQuery <- "SELECT r.id, r.age, w.name AS workclass, e.name AS educ,
                r.education_num, m.name AS marital, o.name AS occup,
                re.name AS relation, ra.name AS race, s.name AS gender,
                r.capital_gain, r.capital_loss, r.hours_week, c.name AS country, r.over_50k
              FROM records AS r 
                LEFT JOIN workclasses AS w
                  ON r.workclass_id = w.id
                LEFT JOIN education_levels AS e
                  ON r.education_level_id = e.id
                LEFT JOIN marital_statuses AS m
                  ON r.marital_status_id = m.id
                LEFT JOIN occupations AS o
                  ON r.occupation_id = o.id
                LEFT JOIN relationships AS re
                  ON r.relationship_id = re.id
                LEFT JOIN races AS ra
                  ON r.race_id = ra.id
                LEFT JOIN sexes AS s
                  ON r.sex_id = s.id
                LEFT JOIN countries AS c
                  ON r.country_id = c.id"

res <- dbSendQuery(con,joinQuery)
results <- fetch(res,-1) #-1 means fetch all rows
dbClearResult(res)
dbDisconnect(con)

#Write the dataframe to csv
csv <- "C:/Users/matt/Dropbox/Documents/Resume and Applications/Applications/RTI/Assessment/exercise01/census.csv"
write.csv(results,file=csv,row.names=F)

#Load data from csv. ? refers to a missing value in the original database
data <- read.csv(csv,na.strings=c("?"))

summary(data)
apply(data[,c(3:10,14)],2,table) #Frequency tables for categorical variables

#Variable recoding and preparation
data$over_50k <- factor(data$over_50k)
data$workclass2 <- revalue(data$workclass,c("Never-worked"="None","Without-pay"="None"))
#table(data$educ,data$education_num)
data$educ2 <- cut(data$education_num,breaks=c(-Inf,8,9,12,Inf),labels=c('LT HS','HS Grad','Some College','College Grad+'))
#table(data$educ2,data$education_num)
data$marital2 <- revalue(data$marital,c("Married-AF-spouse"="Married","Married-civ-spouse"="Married"))
data$native <- data$country == "United-States"
data$capGainDummy <- data$capital_gain > 0
data$capLossDummy <- data$capital_loss > 0
data$capGainLoss <- data$capital_gain - data$capital_loss


#Split the data into training, validation, and test
set.seed(452851)
index <- 1:nrow(data)
trainIndex <- sample(index, round(.7*nrow(data))) #Randomly select subset of rows for inclusion in training set
train <- data[trainIndex,]
notTrain <- data[-trainIndex,]
notTrainIndex <- 1:nrow(notTrain)
validateIndex <- sample(notTrainIndex, round(.2*nrow(data))) #Randomly select subset of rows for inclusion in validation set
validate <- notTrain[validateIndex,]
test <- notTrain[-validateIndex,] #Rows that aren't in training or validation are put in test
rm(notTrain)

#Confirm that the target variable is relatively balanced across the partitions
CrossTable(train$over_50k)
CrossTable(validate$over_50k)
CrossTable(test$over_50k)

#Logistic regression
model1 <- glm(over_50k~poly(age,2)+workclass2+educ2+marital2+occup+race+gender+capGainLoss+hours_week+native, data=train, family="binomial")
summary(model1)
exp(coef(model1)) #Odds ratios

#Note to self: Finalize model, check assumptions, continue
