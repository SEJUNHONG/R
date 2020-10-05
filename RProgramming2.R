#install.packages("dplyr")
#install.packages("tidymodels")
#install.packages("data.table")
#install.packages("ggplots")

library(dplyr)
library(data.table)
library(ggplot2)
library(tidymodels)

ks_project <- fread("catch_ks_project.csv", encoding="UTF-8")
ks_project %>% 
  select(-V1, -launched_new)
ks_project %>% 
  select(state) %>% 
  table()

# 데이터 split
set.seed(2020)
ks_split <- initial_split(ks_project, prop = 0.7, strata = state)
ks_train <- ks_split %>% training()
ks_test <- ks_split %>% testing()

# 데이터 최종 점검 (레시피 생성)
ks_recipe <- ks_train %>% 
  recipe(state ~ .) %>%
  step_rm(name, launched, deadline) %>% 
  step_string2factor(all_outcomes(), main_category) %>% 
  prep()

ks_preprocessed_train <- ks_recipe %>% juice()
ks_preprocessed_test <- ks_recipe %>% bake(ks_test)

# 5 - fold cross validation
ks_cv <- vfold_cv(ks_train, v=5, strata=state)

# Modeling
##1. Logistic Regression
log_set <- logistic_reg(mode="classification") %>% 
  set_engine("glm")
log_cv_result <- tune_grid(object=log_set, preprocessor=ks_recipe, 
                           resamples=ks_cv,
                           metrics=metric_set(accuracy))
                           
show_best(log_cv_result, metric = "accuracy")         

##2. RandomForest
install.packages("randomForest")
library(randomForest)
random_set<-rand_forest(mode="classification", trees = 100, mtry = tune()) %>% 
  set_engine("randomForest")
rf_parameter<-parameters(mtry(c(2,5)))
rf_grid<-grid_regular(rf_parameter, levels = 4)
rf_cv_result<-tune_grid(object=random_set, preprocessor = ks_recipe,
                        resamples = ks_cv, grid = ks_grid, 
                        metrics = metric_set(accuracy))
show_best(rf_cv_result, metric = "accuracy", n=4)                        
