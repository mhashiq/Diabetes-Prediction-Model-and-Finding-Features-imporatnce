library(randomForest)
# devtools::install_github("MI2DataLab/randomForestExplainer")
library(randomForestExplainer)
#Reafffffffffffffffffff

library(readr)
Boston <- read_csv("df.csv")
View(Boston)

set.seed(2017)
forest <- randomForest(class ~ ., data = Boston, localImp = TRUE)

forest

#Various variable importance measures

importance_frame <- measure_importance(forest)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame

#Multi-way importance plot

plot_multi_way_importance(forest, size_measure = "no_of_nodes")
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)


#Compare measures using ggpairs

plot_importance_ggpairs(forest)
plot_importance_ggpairs(importance_frame)


#Compare different rankings

plot_importance_rankings(forest)
plot_importance_rankings(importance_frame)

#Conditional minimal depth

(vars <- important_variables(forest, k = 5, measures = c("mean_min_depth", "no_of_trees"))) 
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))

interactions_frame <- min_depth_interactions(forest, vars)
save(interactions_frame, file = "interactions_frame.rda")

plot_min_depth_interactions(forest) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
plot_min_depth_interactions(interactions_frame)

plot_predict_interaction(forest, Boston, "rm", "lstat")

#raff

explain_forest(forest, interactions = TRUE, data = Boston)
