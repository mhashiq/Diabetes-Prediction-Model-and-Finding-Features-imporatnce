require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')



df <- data.table(Boston, keep.rownames = FALSE)


Boston <- sparse.model.matrix(class~.-1, data = Boston)
head(Boston)



output_vector = df[,class] == "Marked"


bst <- xgboost(data = Boston, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

importance <- xgb.importance(feature_names = Boston@age[[16]], model = bst)
head(importance)
