#Code written by Matt Frei in Feb 2015 for RTI Application Process

#install.packages("RSQLite")
#install.packages("gmodels")
#install.packages("plyr")
#install.packages("vcd")
#install.packages('pROC')
#install.packages('tree')

library(DBI) #Connection to a SQLLite database
library(gmodels) #Nice crosstabs with %s
library(plyr) #Recoding variables 
library(car) #vif
library(vcd) #Tests of association
library(pROC) #ROC curve
library(ggplot2)
library(tree) #CART

##################################### ACCESS CENSUS DATA IN SQLITE DATABASE. FLATTEN AND EXPORT TO CSV #####################################


dbPath <- 'C:/Users/matt/Dropbox/Documents/Resume and Applications/applications/rti/assessment/exercise01/exercise01.sqlite'
csv <- "C:/Users/matt/Dropbox/Documents/Resume and Applications/Applications/RTI/Assessment/exercise01/census.csv"


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
write.csv(results,file=csv,row.names=F)






##################################### READ IN CSV and PREP DATA #####################################


#Load data from csv. ? refers to a missing value in the original database
data <- read.csv(csv,na.strings=c("?"))

# 
# table(complete.cases(data)) #7% of cases have at least one missing value
# M <- sapply(data, function(x) sum(is.na(x))) 
# M[M>0] #occup, workclass, and country have missing values

# summary(data)
# apply(data[,c(3:10,14)],2,table) #Frequency tables for categorical variables

#Variable recoding and preparation
work_char <- as.character(data$workclass) #I'll treat missing values as a level to avoid dropping observations
work_char[is.na(work_char)] <- 'Missing'
data$workclass <- factor(work_char)

occup_char <- as.character(data$occup)
occup_char[is.na(occup_char)] <- 'Missing'
data$occup <- factor(occup_char)

country_char <- as.character(data$country)
country_char[is.na(country_char)] <- 'Missing'
data$country <- factor(country_char)

data$over_50k <- factor(data$over_50k)
# CrossTable(data$over_50k) # 24% high income. 76% low.
data$workclass2 <- revalue(data$workclass,c("Never-worked"="None","Without-pay"="None"))
#table(data$educ,data$education_num)
data$educ2 <- cut(data$education_num,breaks=c(-Inf,8,9,12,Inf),labels=c('LT HS','HS Grad','Some College','College Grad+'))
#table(data$educ2,data$education_num)
relation_char <- as.character(data$relation)
relation_char_recoded <- ifelse(relation_char == "Husband" | relation_char == "Wife","Wife/Husband","Other")
#relation_char_recoded <- sapply(relation_char,function(x) if (x == "Husband" | x == "Wife") x else "Other") #This line is equivelant to the previous one
data$relation2 <- factor(relation_char_recoded)
data$ageCat1 <- cut(data$age,breaks=c(-Inf,29,49,64,Inf),labels=c('17-29','30-49','50-64','65+'))


data$marital2 <- revalue(data$marital,c("Married-AF-spouse"="Married","Married-civ-spouse"="Married"))
data$native <- data$country == "United-States"
data$capGainDummy <- data$capital_gain > 0
data$capLossDummy <- data$capital_loss > 0
data$capGainLoss <- data$capital_gain - data$capital_loss

# data$agelogage <- data$age*log(data$age) #Box Tidewell Transformation






##################################### PARTITION DATA AND MODEL #####################################


#Split the data into training (70%), validation (20%), and test (10%)
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


#####Logistic regression
# options("scipen"=100, "digits"=4) #Makes printing scientific notation less likely

model3 <- glm(over_50k~ageCat1+workclass2+educ2+marital2+occup+race+gender+capGainLoss+hours_week+native, data=train, family="binomial")
summary(model3)
OR <- exp(coef(model3)[-1])
OR
OR.CI <- exp(cbind(OR = coef(model3), confint(model3)))[-1,]
OR.CI

#ROC Curve for validation data
fitpreds = predict(model3,newdata=validate,type="response")
ROC_valid <- roc(validate$over_50k, fitpreds)
print(ROC_valid)
plot(ROC_valid)

#Since neither type of error is more "costly" here, I'll use the Youden index to choose a cutoff
coords(ROC_valid,x='best',best.method="youden") #.2292
ggplot(validate, aes(x=fitpreds, fill=over_50k)) + geom_density(alpha=.3) + geom_vline(xintercept=.2292, linetype="dotted")
decision <- ifelse(fitpreds >=.2292,1,0)
mean(decision != validate$over_50k) #19.8% missclassification

# model1_sw <- step(model3)
# summary(model1_sw) #All variables kept



#####Decision tree approach
maximalTree <- tree(over_50k~.,train[,!names(train) %in% c("id","country")])
plot(maximalTree)
text(maximalTree, pretty=0)

set.seed(144627)
cv_tree <- cv.tree(maximalTree,FUN=prune.misclass)
plot(cv_tree$size,cv_tree$dev,type="b") #5 leaves is adequate

prunnedTree <- prune.misclass(maximalTree,best=5)
plot(prunnedTree)
text(prunnedTree,pretty=0)

tree_pred = predict(prunnedTree,validate,type="class")
mean(tree_pred != validate$over_50k) #15.1% missclassification
