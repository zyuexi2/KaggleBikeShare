library(tidyverse) 
library(vroom) 
library(tidymodels)
library(recipes)

bike <- vroom("/Users/cicizeng/Desktop/STA348/KaggleBikeShare/train.csv")


myCleanData <- bike %>%
  mutate(neweather= ifelse(weather == 4,3,weather)) 

my_recipe <- recipe(~., data = myCleanData) %>%
  step_date(datetime, features="dow") 
data_with_dow <- prep(my_recipe) %>% bake(new_data = NULL)


my_recipe <- recipe(~., data = myCleanData) %>%
  step_dummy(all_nominal_predictors()) 
data_with_dummies <- prep(my_recipe) %>% bake(new_data = NULL)
