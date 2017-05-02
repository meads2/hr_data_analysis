# ----------------------------------
#           Load Libraries
# ----------------------------------
library(ggplot2)
library(caret)
library(dplyr)
library(pvclust)
library(Amelia)
library(dummies)
library(caTools)
library(randomForest)

# Set random seed
set.seed(100)

# Load Data & Randomize

# Full hr.data set
hr.data <- read.csv('hr_data.csv', header = T)

# Smaller set of data & minimize any system biases
hr.data <- sample_n(hr.data, 4000)

#########################################
##                                     ##
##          1. Data Exploration        ##
##                                     ##
#########################################

# View The Types Of Data
glimpse(hr.data) 

# Employee Status Chart
ggplot(hr.data, aes(x = left)) + 
       geom_histogram(bins = 2, binwidth = .5, fill = 'orange') + 
       labs(title = 'Employment Status Of HR Data', x = 'Status', y = 'Count')

# Distrbution By Employee Dept
ggplot(hr.data, aes(x = sales)) +
  geom_histogram(bins = 10,stat = 'count', binwidth = .5) +
  theme_minimal(base_size = 16, base_family = 'Roboto') + 
  geom_bar(aes(fill = 'red')) + 
  theme(axis.text.x = element_text(size=9, angle=45)) +
  labs(title = 'Employee Count By Department', x = 'Department', y = 'Count')

# Distribution Of Satisfaction Levels
hist(hr.data$satisfaction_level,
     breaks = 10,
     col = 'red', 
     main = 'Distribution Of Satisfaction Level', 
     xlab = 'Satisfaction Level', 
     ylab = 'Count'
     )

# Employee Leave By Projects
barData <- table(as.factor(hr.data$left), hr.data$number_project)
barplot(barData, 
        main="Employees Left Vs. Projects",
        xlab="No. of Projects", 
        col=c("blue","orange"),
        legend = rownames(barData), 
        beside=T)

# Calculate Summary Stats
summary(hr.data)

# Visualize Missing Values
missmap(hr.data, 
        col = c('orange', 'white'),
        x.cex = .8,
        ylab = 'Row No.',
        xlab = 'Variable',
        y.cex = .3,
        main = 'Missing Data Of HR Data',
        rank.order = T,
        y.labels = c(seq(1, 4000, by = 500)), 
        y.at = c(seq(1, 4000, by = 500))
      )

#########################################
##                                     ##
##        2.Logistic Regression        ##
##                                     ##
#########################################
# Convert Factors Into Dummy Variables
sales.dummies <- dummy(hr.data$sales, sep = '_')
salary.dummies <- dummy(hr.data$salary, sep = '_')
hr.data.cleaned <- cbind(hr.data, sales.dummies, salary.dummies)
hr.data.cleaned <- hr.data.cleaned[,c(1:8,11:23)]

# Create Training & Test Splits
# Create an 80/20 split
split <- round(nrow(hr.data.cleaned) * .80)
hr.train <- hr.data.cleaned[1:split, ]
hr.test <- hr.data.cleaned[(split + 1):nrow(hr.data.cleaned), ]

# Run Benchmark Logistic Regresion
# Logistic Model to predict whether an employee leaves
hr.model <- glm(left ~ ., family = 'binomial', hr.train)

# Summary Of Model
summary(hr.model)

# Find the best predictors
confint(hr.model, level = .95) 

# Model Predictions
hr.pred <- predict(hr.model, hr.test, type = 'response')

# Find Significant Cutoff Point
# Create a ROC Curve To Find Cutoff Point
# sensitivity vs. speceficity
model.AUC <- colAUC(hr.pred, hr.test$left, plotROC = T)
abline(h=model.AUC, col = 'red')
text(.2,.9,cex = .8, labels = paste('Optimal Cutoff: ', round(model.AUC, 4)))

# Convert Probabilities To Class
# 1 indicates the employee left,
# 0 indicates the employee stayed
# Covert model pred probabilities into classes
p.class <- ifelse(hr.pred > .7860, 1, 0)

# Create a confusion matrix
confusionMatrix(p.class, hr.test$left)

#########################################
##                                     ##
##         3.Cluster Analysis          ##
##                                     ##
#########################################
hr.clusters <- pvclust(hr.data[,1:8])
plot(hr.clusters, main = 'HR Data Cluster')

#########################################
##                                     ##
##            4. Anova Tests           ##
##                                     ##
#########################################

# ANOVA: Time Spent At Company By Sales
anova <- aov(time_spend_company ~ sales, data = hr.data)
summary(anova)
TukeyHSD(anova, conf.level = .95)
ggplot(hr.data, 
       aes(y = time_spend_company, x = sales)) + 
       geom_boxplot(outlier.color = 'red', 
                    outlier.size = .5,
                    fill = '#4c90ff', 
                    color = '#2a5fb7') + 
       labs(title = 'ANOVA: Time Spent At Company By Dept.', 
            x = 'Employee Department', 
            y = 'Time Spent At Company') +
       theme_minimal(base_family = 'Roboto') +
       theme(axis.text.x = element_text(size=9, angle=45))

# ANOVA: SATISFACTION LEVEL BY SALARY
anova2 <- aov(satisfaction_level ~ salary, data = hr.data)
summary(anova2)
TukeyHSD(anova2)
ggplot(hr.data, 
       aes(y = satisfaction_level, x = salary)) + 
       geom_boxplot(outlier.color = 'red', 
                    outlier.size = .5,
                    fill = c('#ff4f7d', '#4fa1ff', '#4fff95'), 
                    color = '#333333') + 
       labs(title = 'ANOVA: Satisfaction Level By Salary', 
            x = 'Salary Level', 
            y = 'Satisfaction Level') +
       theme_minimal(base_family = 'Roboto') +
       theme(axis.text.x = element_text(size=9, angle=45))

#########################################
##                                     ##
##           5. Random Forest          ##
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

