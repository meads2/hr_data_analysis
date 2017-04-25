# ----------------------------------
#           Load Libraries
# ----------------------------------
library(ggplot2)
library(caret)
library(tidyr)
library(dplyr)
library(cluster)
library(ggcorrplot)
library(Amelia)
library(rpart)
library(MASS)
library(dummies)
library(caTools)
library(randomForest)

# Set random seed
set.seed(100)

# ----------------------------------
#        Load Data & Randomize
# ----------------------------------
# Full hr.data set
hr.data <- read.csv('hr_data.csv', header = T)
# Smaller set of data & minimize any systematic biases
hr.data <- sample_n(hr.data, 4000)

# ----------------------------------
#       View The Types Of Data
# ----------------------------------
hr.data.types <- glimpse(hr.data) 

# ----------------------------------
#      Visualize Distributions
# ----------------------------------
# Only Interested In Certain Columns


# ----------------------------------
#      Calculate Summary Stats.
# ----------------------------------
summary(hr.data)

# ----------------------------------
#      Visualize Missing Values
# ----------------------------------
missmap(hr.data, 
        col = c('red', 'white'),
        x.cex = .8,
        ylab = 'Row No.',
        xlab = 'Variable',
        y.cex = .3,
        main = 'Missing Data Of HR Data',
        rank.order = T
      )

#########################################
##                                     ##
##         1.Logistic Regression       ##
##                                     ##
#########################################
# Convert Factors Into Dummy Variables
sales.dummies <- dummy(hr.data$sales, sep = '_')
salary.dummies <- dummy(hr.data$salary, sep = '_')
hr.data.cleaned <- cbind(hr.data, sales.dummies, salary.dummies)
hr.data.cleaned <- hr.data.cleaned[,c(1:8,11:23)]

# ----------------------------------
#   Create Training & Test Splits
# ----------------------------------
# Create an 80/20 split
split <- round(nrow(hr.data.cleaned) * .80)
hr.train <- hr.data.cleaned[1:split, ]
hr.test <- hr.data.cleaned[(split + 1):nrow(hr.data.cleaned), ]

# ----------------------------------
#  Run Benchmark Logistic Regresion
# ----------------------------------
# Logistic Model to predict whether an employee has left
# FOR THIS LOGISTIC MODEL DO I HAVE TO WRAP THE RESPONSE
# VARIABLE "LEFT" IN AS.FACTOR? FOR THE LOGISTIC REGRESSION
# TO WORK PROPERLY?
hr.model <- glm(left ~ ., family = 'binomial', hr.train)
summary(hr.model)
hr.pred <- predict(hr.model, hr.test, type = 'response')

# ----------------------------------
#    Find Significant Cutoff Point
# ----------------------------------
# Create a ROC Curve To Find Cutoff Point
colAUC(hr.pred, hr.test$left, plotROC = T)
model.AUC <- colAUC(hr.pred, hr.test$left, plotROC = T)
abline(h=model.AUC, col = 'red')
text(.2,.9,cex = .8, labels = paste('Optimal Cutoff: ', round(model.AUC, 4)))

# ----------------------------------
#   Convert Probabilities To Class
# ----------------------------------
# 1 indicates the employee left,
# 0 indicates the employee stayed

# Covert model pred probabilities into classes
p.class <- ifelse(hr.pred > .7860, 1, 0)

# Create a confusion matrix
# QUESTION: HOW TO CHANGE THE MATRIX TO SHOW 
# 1 AS POSITIVE?
confusionMatrix(p.class, hr.test$left)


#########################################
##                                     ##
##         2.Cluster Analysis          ##
##                                     ##
#########################################
hr.clusters <- hclust(dist(hr.data[,1:8]))
plot(hr.clusters)

# Cluster Cut
hr.cluster.cut <- cutree(hr.clusters, 10)
cluster.counts <- table(hr.cluster.cut)
plot(cluster.counts)


#########################################
##                                     ##
##            3. Anova Test            ##
##                                     ##
#########################################
boxplot(hr.data)
# Tukey's T-Test !!!!
# Check All Assumptions !!!!



#########################################
##                                     ##
##          4. Random Forest           ##
##                                     ##
#########################################
# Get A Fresh Copy Of The hr.data
rf.data <- read.csv('hr_data.csv', header = T)
rf.data <- sample_n(rf.data, 4000)

# Split Data
rf.split <- round(nrow(rf.data) * .80)
rf.train <- rf.data[1:rf.split, ]
rf.test <- rf.data[(rf.split + 1):nrow(rf.data), ]

# Create Model
rf.model <- randomForest(as.factor(left) ~ ., rf.train, ntree = 20)
summary(rf.model)
rf.pred <- predict(rf.model, rf.test)
summary(rf.pred)

# View Confusion Matrix
confusionMatrix(rf.pred, rf.test$left)

