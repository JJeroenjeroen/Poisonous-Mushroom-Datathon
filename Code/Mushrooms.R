#####################################################
# Date:      28-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Poisonous mushroom datathon            #
# Version:   1.0                                    #    
#####################################################

#The goal of this Datathon was to create a model that can tell whether a mushroom is edible is not 
#depending on a picture of the mushroom. However, if it predicts that a poisonous mushroom is edible,
#someone could die. This must be avoided ofcourse...

rm(list = ls())
library(dplyr)
library(tidyr)
library(caret)

setwd("C:/Users/Jeroen/Desktop/Ubiqum/Mushroom Hackathon/Dataset")

#read mushroom trainindata
mushrooms <- readRDS("train.rds")

#turn all character variables into factors
mushrooms <- as.data.frame(unclass(mushrooms))
mushrooms <- mushrooms[complete.cases(mushrooms),]



#remove attributes that are not visible on a photograph or with only 1 level
mushrooms <- mushrooms %>%  select(-c("odor", "habitat",  
                                      "stalk.surface.above.ring", 
                                      "stalk.surface.below.ring", 
                                      "veil.type","spore.print.color",
                                      "gill.attachment", "gill.spacing", 
                                      "gill.size", "gill.color",
                                      "stalk.root", "stalk.surface.above.ring",
                                      "veil.type", "ring.number",
                                      "ring.type", "veil.color",
                                      "stalk.shape",
                                      "cap.surface"))


#remove duplicate values
mushrooms <- unique(mushrooms)

#add more observations depending on cap shape
mushrooms_stalk <- mushrooms %>% filter(stalk.color.below.ring == "w" & class == "p")
mushrooms <- bind_rows(mushrooms, mushrooms_stalk, mushrooms_stalk, mushrooms_stalk)

#make a data partition
#how big should the data partition be?
no_rows_partition <- 200

#create the data partition of the full trainingset  
set.seed(123)
train_partition <- createDataPartition(y = mushrooms[,"class"],
                                       p = (no_rows_partition/nrow(mushrooms)),
                                       list = FALSE)

#create trainset
mushrooms_train <- mushrooms[train_partition,]

#create testset
mushrooms_test <- mushrooms[-train_partition,]


#set coss validation parameters
control_method <-"repeatedcv"
control_folds <- 10
control_repeats <- 1
control_search <- "grid"
fitControl <- trainControl(method = control_method,
                           number = control_folds,
                           repeats = control_repeats,
                           search = control_search)

#set training parameters
train_method <- "C5.0"
train_metric <- "Sensitivity"
train_tuneLength <- 15

#train Random Forest Regression model 
set.seed(123)
C5Fit <- train(class ~ .,
                data = mushrooms_train,
                method = train_method,
                metric = train_metric,
                tuneLength = train_tuneLength,
                trControl = fitControl)


#See the most important predictors
predictions <- predict(C5Fit, newdata = mushrooms_test)

#generate a variable that provides specificity of the model
matrix <- confusionMatrix(predictions, mushrooms_test$class)
specificity <- matrix[["byClass"]][["Specificity"]]

#make a dataframe for mispredicted poisonous mushrooms specifically
mushrooms_test$predictions <- predictions
mushrooms_errors <- mushrooms_test %>% filter(predictions == "e" & class == "p")



#while loop that adds the mispredicted toxic mushrooms to the set and 
#models a decision tree untill poison mushrooms are not predicted to be edible
while (specificity != 1){
  
  
  #add errors of previous model to the full dataset
  mushrooms_errors <- mushrooms_errors %>% select(-"predictions")
  mushrooms <- bind_rows(mushrooms, mushrooms_errors)
  
  #make a data partition
  no_rows_partition <- 200
  
  #create the data partition of the full trainingset  
  set.seed(123)
  train_partition <- createDataPartition(y = mushrooms[,"class"],
                                         p = (no_rows_partition/nrow(mushrooms)),
                                         list = FALSE)
  
  #create trainset
  mushrooms_train <- mushrooms[train_partition,]
  
  #create testset
  mushrooms_test <- mushrooms[-train_partition,]
  
  
  #set coss validation parameters
  control_method <-"repeatedcv"
  control_folds <- 10
  control_repeats <- 1
  control_search <- "grid"
  fitControl <- trainControl(method = control_method,
                             number = control_folds,
                             repeats = control_repeats,
                             search = control_search)
  
  #set training parameters
  train_method <- "C5.0"
  train_metric <- "Sensitivity"
  train_tuneLength <- 15
  
  #train decision tree 
  set.seed(123)
  C5Fit <- train(class ~ .,
                  data = mushrooms_train,
                  method = train_method,
                  metric = train_metric,
                  tuneLength = train_tuneLength,
                  trControl = fitControl)
  
  
  
  save(C5Fit, file = "Jeroen_mushroom5.rda")
  
  #See the most important predictors
  predictions <- predict(C5Fit, newdata = mushrooms_test)
  
  #generate a variable that provides specificity of the model
  matrix <- confusionMatrix(predictions, mushrooms_test$class)
  specificity <- matrix[["byClass"]][["Specificity"]]
  
  #make a dataframe for mispredicted poisonous mushrooms specifically
  mushrooms_test$predictions <- predictions
  mushrooms_errors <- mushrooms_test %>% filter(predictions == "e" & class == "p")
  
}

#save model
save(C5Fit, file = "Jeroen_mushrooms.rda")