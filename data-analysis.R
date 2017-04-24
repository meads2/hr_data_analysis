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

# ----------------------------------
#        Load Data & Randomize
# ----------------------------------
# Full hr.data set
hr.data <- read.csv('hr_data.csv', header = T)
# minimize any systematic biases
rows <- sample(nrow(hr.data))
# reorganize hr.data to reflect random nature
hr.data <- hr.data[rows,]
# Smaller set of data
hr.data <- sample_n(hr.data, 8000)

# ----------------------------------
#       View The Types Of Data
# ----------------------------------
hr.data.types <- glimpse(hr.data) 

# ----------------------------------
#      Clean Data For Analysis
# ----------------------------------
# There are 2 varaibles that need to
# be made into dummy variables for
# proper analysis via the dummies
# library
sales.dummies <- dummy(hr.data$sales, sep = '_')
salary.dummies <- dummy(hr.data$salary, sep = '_')
# hr.data <- cbind(hr.data, dummy(hr.data$sales, sep = "_"), dummy(hr.data$salary, sep = '_'))
# Drop original catagorical variables

# ----------------------------------
#     Visualize Distributions
# ----------------------------------
ggcorrplot(hr.data,
           show.legend = T,
           method = 'circle',
           show.diag = T,
           title = 'Correlataions Of HR Data',
           colors = c('red', 'yellow', 'green'),
           legend.title = 'Correlation Scale'
           )

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


# ----------------------------------
#   Create Training & Test Splits
# ----------------------------------

# Create an 80/20 split
split <- round(nrow(hr.data), .80)
hr.train <- hr.data[1:split, ]
hr.test <- hr.data[(split + 1):nrow(hr.data), ]

# ----------------------------------
#  Run Benchmark Logistic Regresion
# ----------------------------------
# Logistic Model to predict whether an employee has left
hr.model <- glm(left ~ ., family = 'binomial', hr.train)
summary(hr.model)
hr.pred <- predict(hr.model, hr.test, type = 'response')
hr.pred

# ----------------------------------
#        Tune Logistic Model
# ----------------------------------

# ----------------------------------
#    Find Significant Cutoff Point
# ----------------------------------
# Create a ROC Curve To Find Point


# ----------------------------------
#   Convert Probabilities To Class
# ----------------------------------
# 1 indicates the employee left,
# 0 indicates the employee stayed
model.cutoff <- .5

# Covert model pred probabilities into classes
p.class <- ifelse(hr.pred > model.cutoff, 1, 0)

# Create a confusion matrix
confusionMatrix(p.class, hr.data$left)







#########################################
##                                     ##
##         2.Cluster Analysis          ##
##                                     ##
#########################################








#########################################
##                                     ##
##             3. Anova Test           ##
##                                     ##
#########################################
# Tukey's T-Test !!!!



