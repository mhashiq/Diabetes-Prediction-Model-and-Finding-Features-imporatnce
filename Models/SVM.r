library(e1071)
library(rpart)

svm_model_normal <- svm(class ~ ., data=Boston)
summary(svm_model_normal)

confusionMatrix(predict (svm_model_normal, test) , test$class)
