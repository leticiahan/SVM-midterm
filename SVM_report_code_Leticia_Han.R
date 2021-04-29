# Math 533 Midterm
# Support Vector Machine
# Leticia Han
# 10/15/2020

library(tidyverse)
library(gridExtra)

# Plots of the hyperplane and optimal separating hyperplane
x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
group = c(rep("Class 1", 4), rep("Class -1", 3))
df = data.frame(X1 = x1, X2 = x2, Class = group)
g1 <- ggplot(df, aes(X1, X2, group = Class)) + geom_point(aes(color = Class)) + geom_abline(slope = 1, intercept = -0.5) + geom_abline(slope = 0.9, intercept = -0.2) + geom_abline(slope = 1, intercept = -0.6) + theme(legend.position = 'none', panel.border = element_rect(linetype = 'solid', fill = NA), panel.grid = element_blank(), panel.background = element_rect(fill = 'white'))
g2 <- ggplot(df, aes(X1, X2, group = Class)) + geom_point(aes(color = Class)) +
  geom_abline(slope = 1, intercept = -0.5) +
  geom_abline(slope = 1, intercept = -1, linetype = 'dashed') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  theme(legend.position = 'none', panel.border = element_rect(linetype = 'solid', fill = NA),
        panel.grid = element_blank(), panel.background = element_rect(fill = 'white'))
grid.arrange(g1, g2, ncol = 2)

# Plots of the soft margin hyperplanes
x1 = c(3, 2, 4, 1, 3, 2, 4, 4, 2.7)
x2 = c(4, 2, 4, 4, 2.8, 1, 3, 1, 1.8)
group = c(rep("Class 1", 5), rep("Class -1", 4))
df = data.frame(X1 = x1, X2 = x2, Class = group)
g1 <- ggplot(df, aes(X1, X2, group = Class)) + geom_point(aes(color = Class)) + geom_abline(slope = 1, intercept = -0.5) + geom_abline(slope = 1, intercept = -1, linetype = 'dashed') + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + theme(legend.position = 'none', panel.border = element_rect(linetype = 'solid', fill = NA), panel.grid = element_blank(), panel.background = element_rect(fill = 'white'))


x1 = c(3, 2, 4, 1, 3, 3, 2, 4, 4, 2.7, 2)
x2 = c(4, 2, 4, 4, 2.8, 1.5, 1, 3, 1, 1.8, 2.5)
group = c(rep("Class 1", 6), rep("Class -1", 5))
df = data.frame(X1 = x1, X2 = x2, Class = group)
g2 <- ggplot(df, aes(X1, X2, group = Class)) + geom_point(aes(color = Class)) + geom_abline(slope = 1, intercept = -0.5) + geom_abline(slope = 1, intercept = -1, linetype = 'dashed') + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + theme(legend.position = 'none', panel.border = element_rect(linetype = 'solid', fill = NA), panel.grid = element_blank(), panel.background = element_rect(fill = 'white'))
grid.arrange(g1, g2, ncol = 2)

# Adult dataset: Preprocessing
adult <- read_csv('adult.csv', col_names = c("age", "workclass", "fnlwgt", "education", "education_num",
                                             "marital_status", "occupation", "relationship", "race", "sex",
                                             "capital_gain", "capital_loss", "hours_per_week",
                                             "native_country", "income"), col_types = cols())
summary(adult)
adult <- adult %>%
  filter_if(is.character, all_vars(!str_detect(., "\\?"))) %>%
  mutate_if(is.character, as.factor) %>%
  select(-c('capital_gain', 'capital_loss' , 'education_num'))
library(car)
vif(glm(income~., data = adult, family = "binomial"))
vif(glm(income~.-relationship, data = adult, family = "binomial"))
# Relationship is highly collinear with marital status, thus removed.
adult <- adult %>% select(-relationship)
library(miscset)
ggplotGrid(ncol = 2,
           lapply(c("workclass", "education", "marital_status", "occupation", "race", "native_country"),
                  function(col) {
                    ggplot(adult, aes_string(col)) + geom_bar() + coord_flip()
                  }))

adult <- adult %>% filter(workclass != "Without-pay")
adult$workclass <- droplevels(adult$workclass)
adult$workclass <- recode(adult$workclass, "c('Federal-gov', 'Local-gov', 'State-gov')='Gov';
                          c('Self-emp-inc', 'Self-emp-not-inc')='Self-emp'")
adult$education <- recode(adult$education,
                          "c('Preschool', '1st-4th', '5th-6th', '7th-8th', '9th', '10th', '11th',
                          '12th')='No-HS-grad';
                          c('Prof-school', 'Masters', 'Doctorate')='Post-grad';
                         c('Assoc-acdm', 'Assoc-voc') = 'Assoc'")
adult$marital_status <- recode(adult$marital_status, "c('Divorced', 'Separated')='Separated';
                               c('Married-AF-spouse', 'Married-civ-spouse',
                               'Married-spouse-absent')='Married'")
adult$occupation <- recode(adult$occupation, "c('Armed-Forces')='Protective-serv';
                           'Priv-house-serv'='Other-service'")
adult$race <- recode(adult$race, "c('Amer-Indian-Eskimo', 'Black', 'Asian-Pac-Islander')='Other'")
adult$native_country <- recode(adult$native_country, "'United-States'='United-States'; else='Foreign'")
ggplotGrid(ncol = 2,
           lapply(c("workclass", "education", "marital_status", "occupation", "race", "native_country"),
                  function(col) {
                    ggplot(adult, aes_string(col)) + geom_bar() + coord_flip()
                  }))
prop.table(table(adult$income))

# Training Data & Testing Data
# half of original dataset
set.seed(115)
adult <- adult[sample(1:nrow(adult), nrow(adult)*.5),]
prop.table(table(adult$income))

set.seed(123)
index <- sample(1:nrow(adult), nrow(adult)*.7)
train <- adult[index,]
test <- adult[-index, ]

# Function to calculate BER
ber <- function(pred, truth) { # balanced misclassification rate
  mat <- table(pred, truth)
  tpr <- mat[2,2]/sum(mat[,2])
  tnr <- mat[1,1]/sum(mat[,1])
  bcr <- (tpr + tnr)/2
  1-bcr
}

library(PRROC)

# Support Vector Machine
Train <- train
Train$income <- as.factor(ifelse(train$income == "<=50K", 'LT50', 'GT50')) # format for caret
library(caret)
library(e1071)
library(kernlab)
library(doParallel)
cl <- makeForkCluster(detectCores()-2)
registerDoParallel(cl)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     allowParallel = TRUE)
svmfit <- train(income ~ .,
                data = Train,
                method = "svmRadial",
                preProcess = c('center', 'scale'),
                metric = "Kappa",
                trControl = ctrl,
                tuneLength = 10)
stopCluster(cl)

svmfit
pred <- predict(svmfit, test, "prob")
pred.svm <- ifelse(pred[,1] >= 0.5, ">50K", "<=50K")
table(pred.svm, test$income)
(svm.ber <- ber(pred.svm, test$income))
y <- ifelse(test$income %in% "<=50K", 0, 1)
svm.auc <- roc.curve(scores.class0 = pred[,1], weights.class0 = y,
                     curve=TRUE)
plot(svm.auc)

# Logistic Regression
logfit <- glm(income ~., family = "binomial", data = train)
pred <- predict(logfit, newdata = test, type = "response")
pred.log <- ifelse(pred > .5, ">50K", "<=50K")
table(pred.log, test$income)
(log.ber <- ber(pred.log, test$income))
log.auc <- roc.curve(scores.class0 = pred, weights.class0 = y,
                     curve=TRUE)
plot(log.auc)


# Random Forest
cl <- makeForkCluster(detectCores()-2)
registerDoParallel(cl)
rffit <- train(income ~ .,
               data = Train,
               method = "rf",
               metric = "Kappa",
               trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE),
               tuneLength = 20)
stopCluster(cl)
pred <- predict(rffit, test, type = "prob")
pred.rf <- ifelse(pred[,1] > .5, ">50K", "<=50K")
table(pred.rf, test$income)
(rf.ber <- ber(pred.rf, test$income))
rf.auc <- roc.curve(scores.class0 = pred[,1], weights.class0 = y,
                    curve=TRUE)
plot(rf.auc)

# KNN
cl <- makeForkCluster(detectCores()-2)
registerDoParallel(cl)
knnfit <- train(income ~ .,
                data = Train,
                method = "knn",
                preProcess = "range",
                metric = "Kappa",
                trControl = ctrl,
                tuneLength = 10)
stopCluster(cl)

pred <- predict(knnfit, test, type = "prob")
pred.knn <- ifelse(pred[,1] > .5, ">50K", "<=50K")
table(pred.knn, test$income)
(knn.ber <- ber(pred.knn, test$income))
knn.auc <- roc.curve(scores.class0 = pred[,1], weights.class0 = y,
                     curve=TRUE)
plot(knn.auc)

# Naive Bayes
library(naivebayes)
nbfit <- naive_bayes(income~., train, laplace = 0.5)
pred.nb <- nbfit %class% test[,-11]
pred <- predict(nbfit, test[,-11], 'prob')
table(pred.nb, test$income)
(nb.ber <- ber(pred.nb, test$income))
nb.auc <- roc.curve(scores.class0 = pred[,2], weights.class0=y,
                    curve=TRUE)
plot(nb.auc)