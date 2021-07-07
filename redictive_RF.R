library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

Boston[sapply(Boston, is.numeric)] <- lapply(Boston[sapply(Boston, is.numeric)], 
                                       as.factor)



set.seed(111)
boruta <- Boruta(class ~ ., data = Boston, doTrace = 2, maxRuns = 500)
print(boruta)


plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

bor <- TentativeRoughFix(boruta)
print(bor)

attStats(boruta)

#data Partition 
set.seed(222)
ind <- sample(2, nrow(Boston), replace = T, prob = c(0.6, 0.4))
train <- Boston[ind==1,]
test <- Boston[ind==2,]

train$class <- as.character(train$class)
train$class <- as.factor(train$class)

#Random Forest Model
set.seed(333) 
rf60 <- randomForest(class~., data = train) 

p <- predict(rf60, test)
confusionMatrix(p, test$class)

print(p)



library(ROCR)






