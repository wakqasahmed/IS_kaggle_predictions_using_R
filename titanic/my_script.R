####DataCamp - Chapter 1 - Raising anchors
###1.1 Loading CSV Data

# Assign the training set
train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"))

# Assign the testing set
test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"))

# Make sure to have a look at your training and testing set
print(train)
print(test)

###1.2 Understanding Data
str(train)

###1.3 Rose vs. Jack OR Female vs. Male
# Your train and test set are still loaded in
str(train)
str(test)

# Passengers that survived vs passengers that passed aways

# absolute numbers
table(train$Survived)

# percentages
prop.table(table(train$Survived))

# Males & females that survived vs males & females that passed away

#absolute numbers
table(train$Sex, train$Survived)

# row-wise proportions
prop.table(table(train$Sex, train$Survived),1)

###1.4 Does age play a role?
# Your train and test set are still loaded in
str(train)
str(test)

# Create the column child, and indicate whether child or no child
train$Child[train$Age == NA] <- NA
train$Child[train$Age >= 18] <- 0
train$Child[train$Age < 18] <- 1

# Two-way comparison
table(train$Child, train$Survived)
prop.table(table(train$Child, train$Survived), 1)

###1.5 Making your first predictions
# Your train and test set are still loaded in
str(train)
str(test)

# prediction based on gender 
test_one <- test

test_one$Survived[test_one$Sex == "male"] <- 0
test_one$Survived[test_one$Sex == "female"] <- 1

####DataCamp - Chapter 2 - From icebergs to tree

###2.1 Intro to decision trees
# Load in the R package  
library('rpart')

###2.2 Creating your first decision tree
# Your train and test set are still loaded in
str(train)
str(test)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancified tree
fancyRpartPlot(my_tree_two)

###2.3 Interpreting your decision tree

###2.4 Print and submit to Kaggle
# Your train and test set are still loaded in
str(train)
str(test)

# Make your prediction using the test set
my_prediction <- predict(my_tree_two, test, type="class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Check that your data frame has 418 entries
nrow(my_solution)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv" , row.names = FALSE)

###2.5 Overfitting, the iceburg of decision trees
# Your train and test set are still loaded in
str(train)
str(test)

# Create a new decision tree my_tree_three
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize your new decision tree
fancyRpartPlot(my_tree_three)

###2.6 Re-engineering our titanic dataset
# Your train and test set are still loaded in
str(train)
str(test)

# create a new train set with the new variable
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# Create a new decision tree my_tree_three
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data=train_two, method="class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

###2.7 Passenger title and survival rate
# train_new and test_new are available in the workspace
str(train_new)
str(test_new)

# Create a new model `my_tree_five`
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=train_new, method="class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_five)

# Make your prediction using `my_tree_five` and `test_new`
my_prediction <- predict(my_tree_five, test_new, type="class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

###3.1 What is Random Forest
# All data, both training and test set
all_data

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(combi$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]

###3.2 A Random Forest analysis in R
# train and test are available in the workspace
str(train)
str(test)

# Load in the package
library(randomForest)

# Train set and test set
str(train)
str(test)

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=train, importance=TRUE, ntree=1000)

# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

###3.3 Important variables
varImpPlot(my_forest)





















###4.1 Improving on my own - Full Script

# Assign the training set
train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"), na.strings=c('NA',''), stringsAsFactors=F)

# Assign the testing set
test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"), na.strings=c('NA',''), stringsAsFactors=F)

# train and test are available in the workspace
#str(train)
#str(test)

# Load in the package
library(randomForest)
library(rpart)

# Train set and test set
#str(train)
#str(test)

# All data, both training and test set
#test$Survived <- 'NA'
#all_data = rbind(train, test)
test$Survived <- 'NA'
all_data <- rbind(train, test)

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm=TRUE)

all_data$family_size <- all_data$SibSp + all_data$Parch + 1

all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(as.character(x), split='[,.]')[[1]][2]})
all_data$Title <- sub(' ', '', all_data$Title)
unique(all_data$Title)
all_data$Title[all_data$Title %in% c('Mlle', 'Mme', 'Ms', 'Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
all_data$Title <- factor(all_data$Title)

#Extract Cabin Num from Cabin 
all_data$CabinNum<-sapply(all_data$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
all_data$Deck<-sapply(all_data$Cabin,function(x) substr(x,1,1))
all_data$CabinNum<-as.numeric(all_data$CabinNum)
all_data$CabinPos<-NA

all_data$Sex[all_data$Sex == "male"] <- 0
all_data$Sex[all_data$Sex == "female"] <- 1

# How to fill in missing CabinPos values?
# We make a prediction of a passengers Class using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_cabinnum <- rpart(CabinNum ~ Pclass + Fare,
                            data=all_data[!is.na(all_data$CabinNum),], method="anova")
all_data$CabinNum[is.na(all_data$CabinNum)] <- predict(predicted_cabinnum, all_data[is.na(all_data$CabinNum),])

# How to fill in missing Deck values?
# We make a prediction of a passengers Class using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_deck <- rpart(Deck ~ Pclass + Fare + CabinNum,
                            data=all_data[!is.na(all_data$Deck),], method="anova")
all_data$Deck[is.na(all_data$Deck)] <- predict(predicted_deck, all_data[is.na(all_data$Deck),])

#Categorize as Front, Middle or End, based on Deck plans
#G Deck
all_data$CabinPos[all_data$Deck=="G"]<-'End'

#F Deck
all_data$CabinPos[all_data$Deck=="F"]<-'End'

#E Deck 1 - 26 - Front; 27 - 76  Middle; 77+ End
all_data$CabinPos[all_data$Deck=="E" & all_data$CabinNum < 27]<-'Front'
all_data$CabinPos[all_data$Deck=="E" & all_data$CabinNum >= 27 & all_data$CabinNum < 77]<-'Middle'
all_data$CabinPos[all_data$Deck=="E" & all_data$CabinNum >= 77] <- 'End'

#D Deck 1 - 37 Front; All else End
all_data$CabinPos[all_data$Deck=="D" & all_data$CabinNum < 38]<-'Front'
all_data$CabinPos[all_data$Deck=="D" & all_data$CabinNum >= 38]<-'End'

#C Deck 1 - 35 Front; 36 - 90 Middle; 91+ End
all_data$CabinPos[all_data$Deck=="C" & all_data$CabinNum < 36]<-'Front'
all_data$CabinPos[all_data$Deck=="C" & all_data$CabinNum >= 36]<-'Middle'

#B Deck 1 - 49 Front; 50+ Middle; No End
all_data$CabinPos[all_data$Deck=="B" & all_data$CabinNum < 50]<-'Front'
all_data$CabinPos[all_data$Deck=="B" & all_data$CabinNum >= 50]<-'Middle'

#A Deck 1 - 35 Front; 36-37 Middle; No End
all_data$CabinPos[all_data$Deck=="A" & all_data$CabinNum < 36]<-'Front'
all_data$CabinPos[all_data$Deck=="A" & all_data$CabinNum >= 36]<-'Middle'

all_data$CabinPos<-factor(all_data$CabinPos)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])


# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]


# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + CabinPos , data=train, importance=TRUE, ntree=1000)
# + Sex+ Age + SibSp + Parch + Fare+ Embarked + Title + CabinPos
# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

varImpPlot(my_forest)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "titanic/titanic_solution_rf.csv", row.names = FALSE)