library(tidyverse)
library(tidymodels)
library(vroom)
library(stacks)


## Read in the data
bikeTrain <- vroom("/Users/cicizeng/Desktop/STA348/KaggleBikeShare/train.csv")
bikeTest <- vroom("/Users/cicizeng/Desktop/STA348/KaggleBikeShare/test.csv")

## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

## Cleaning & Feature Engineering
bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime)%>%
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

##cross-validation folds
folds <- vfold_cv(bikeTrain, v = 10, repeats = 1)

##Create a control grid

untunedModel <- control_stack_grid() #If tuning over a grid
tunedModel <- control_stack_resamples() #If not tuning a model

######################
#####linear model#####
######################

## Define the model
lin_model <- linear_reg() %>%
  set_engine("lm")
## Set up the whole workflow
linear_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) 

##fit linear repression to folds
linreg_folds <- linear_workflow %>%
  fit_resamples(resamples=folds,
                control=tunedModel)


########################
##penalized regression##
########################



## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) 

preg_tuning_grid <- grid_regular(penalty(),
                                mixture(),
                                levels = 5)

## Run the CV1
preg_folds_fit <- preg_wf %>%
tune_grid(resamples=folds,
          grid=preg_tuning_grid,
          metrics=metric_set(rmse),
          control = untunedModel)  # including the control grid in the tuning ensures you can6
# call on it later in the stacked mod

####################
##regression tree###
####################

reg_tree <-decision_tree(tree_depth = tune(),
                         cost_complexity = tune(),
                         min_n=tune())%>%
  set_engine("rpart")%>%
  set_mode("regression")

## Workflow
regTree_wf <- workflow()%>%
  add_recipe(bike_recipe)%>%
  add_model(reg_tree)

## Tuning Grid
regTree_tuning_grid <- grid_regular(tree_depth(),
                                    cost_complexity(),
                                    min_n(),
                                    levels = 5)
## Tune the model
tree_folds_fit <- regTree_wf %>%
  tune_grid(resamples = folds,
            grid=regTree_tuning_grid,
            metrics=metric_set(rmse),
            control=untunedModel)


####################
##stack the models##
####################

bike_stack <- stacks() %>%
  add_candidates(linreg_folds) %>%
  add_candidates(preg_folds_fit) %>%
  add_candidates(tree_folds_fit) 
as_tibble(bike_stack)


stack_mod <- bike_stack %>%
blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members()

collet_parameters(stack_mod,"tree_folds_fit")


## Get Predictions for test set AND format for Kaggle
stacked_preds <- predict(stack_mod, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred))%>%
  bind_cols(., bikeTest) %>% # Bind predictions with test data
  select(datetime, .pred) %>% # Just keep datetime and predictions
  rename(count = .pred) %>% # Rename pred to count (for submission to Kaggle)
  mutate(count = pmax(0, count)) %>% # Pointwise max of (0, prediction)
  mutate(datetime = as.character(format(datetime))) # Needed for the right format for Kaggle

## Write prediction file to CSV
vroom_write(x = test_preds, file = "./result.csv", delim = ",")

