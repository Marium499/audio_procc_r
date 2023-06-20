library(tuneR)
library(audio)
library(seewave)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(stringr)
library(matrixStats)
library(melfcc)
library(caTools)
library(class)
library(e1071)
library(caret)
library(factoextra)
library("FactoMineR")
library("ggcorrplot")
library("corrr")

df <- read.csv("output/df_final_combined_6_mfcc.csv")
pca_test <- df_final
rownames(pca_test) <- NULL
colSums(is.na(pca_test))
pca_test <- drop_na(pca_test)
summary(pca_test)


########### SVM

df_svm <- pca_test

df_svm$emotion = as.factor(df_svm$emotion)
df_svm$gender = as.factor(df_svm$gender)
df_svm_2 <- df_svm[, !(names(df_svm) %in% c('path', 'filename', 'dataset', 'duration', 'age', 'sample_rate'))]
X <- df_svm[, !(names(df_svm) %in% c('path', 'filename', 'dataset', 'duration', 'age', 'emotion','gender', 'sample_rate'))]
y <- df_svm$emotion
df_svm_2$emotion
set.seed(0)



parts = createDataPartition(df_svm_2$emotion, p = .8, list = F)
train = df_svm_2[parts, ]
test = df_svm_2[-parts, ]

train_emo <- train$emotion
train_gen <- train$gender
train <- train[, !(names(train) %in% c('emotion', 'gender'))]


scaler <- preProcess(train, method = "scale")
X_tr_rescaled <- predict(scaler, train)

X_tr_rescaled$emotion <- train_emo
X_tr_rescaled$gender <- train_gen





str(X_tr_rescaled)
train_control = trainControl(method = "cv", number = 5)

set.seed(50)

# training a Regression model while tuning parameters (Method = "rpart")
model = train(emotion~., data = X_tr_rescaled, method = "svmRadial", trControl = train_control)

# summarising the results
print(model)


#use model to make predictions on test data
pred_y = predict(model, test)

# confusion Matrix
confusionMatrix(data = pred_y, test$emotion)

################ KNN

classifier_knn <- knn(train = X_tr_rescaled,
                      test = test,
                      cl = X_tr_rescaled$emotion,
                      k = 3)


cm <- table(test$emotion, classifier_knn)
cm
misClassError <- mean(classifier_knn != test$emotion)
print(paste('Accuracy =', 1-misClassError))

