#clear memory
rm(list=ls())

#read the data
train <- read.csv("../Data/train.csv", stringsAsFactors = T)
test <- read.csv("../Data/test.csv", stringsAsFactors = T)

dim(train)
dim(test)

str(train)
str(test)

head(train)

# DATA CLEANING, WRANGLING, TIDYING

#Reformat data, convert data type 
library("tidyverse")
library(dplyr)

#Reformat Date Class
train$last_scraped = as.Date(train$last_scraped)
train$host_since = as.Date(train$host_since)
train$calendar_last_scraped = as.Date(train$calendar_last_scraped)
train$first_review = as.Date(train$first_review)
train$last_review = as.Date(train$last_review)

test$last_scraped = as.Date(test$last_scraped)
test$host_since = as.Date(test$host_since)
test$calendar_last_scraped = as.Date(test$calendar_last_scraped)
test$first_review = as.Date(test$first_review)
test$last_review = as.Date(test$last_review)

#Count the days 
train$last_scraped_days = as.numeric(as.Date("2021-04-11") - train$last_scraped)
train$host_since_days = as.numeric(as.Date("2021-04-11") - train$host_since)
train$calendar_last_scraped_days = as.numeric(as.Date("2021-04-11") - train$calendar_last_scraped)
train$first_review_days = as.numeric(as.Date("2021-04-11") - train$first_review)
train$last_review_days = as.numeric(as.Date("2021-04-11") - train$last_review)

test$last_scraped_days = as.numeric(as.Date("2021-04-11") - test$last_scraped)
test$host_since_days = as.numeric(as.Date("2021-04-11") - test$host_since)
test$calendar_last_scraped_days = as.numeric(as.Date("2021-04-11") - test$calendar_last_scraped)
test$first_review_days = as.numeric(as.Date("2021-04-11") - test$first_review)
test$last_review_days = as.numeric(as.Date("2021-04-11") - test$last_review)


#Character Class
train$summary = as.character(train$summary)
train$space = as.character(train$space)
train$description = as.character(train$description)
train$neighborhood_overview = as.character(train$neighborhood_overview)
train$transit = as.character(train$transit)
train$access = as.character(train$access)
train$house_rules = as.character(train$house_rules)
train$host_location = as.character(train$host_location)
train$host_about = as.character(train$host_about)
train$host_verifications = as.character(train$host_verifications)
train$amenities = as.character(train$amenities)
train$neighbourhood_cleansed = as.character(train$neighbourhood_cleansed)

test$summary = as.character(test$summary)
test$space = as.character(test$space)
test$description = as.character(test$description)
test$neighborhood_overview = as.character(test$neighborhood_overview)
test$transit = as.character(test$transit)
test$access = as.character(test$access)
test$house_rules = as.character(test$house_rules)
test$host_location = as.character(test$host_location)
test$host_about = as.character(test$host_about)
test$host_verifications = as.character(test$host_verifications)
test$amenities = as.character(test$amenities)
test$neighbourhood_cleansed = as.character(test$neighbourhood_cleansed)

#exclude data
train <- train[train$country_code != 'UY',]
test <- test[test$country_code != 'UY',]

#check blank data and impute missing value
blank_data = colSums(is.na(train))
blank_data[blank_data > 0]

#train
for (i in 1:ncol(train)) {
  if (is.numeric(train[,i])) {
    train[is.na(train[,i]), i] = mean(train[,i], na.rm = TRUE)
  }
  if (is.factor(train[,i])) {
    train[,i] = addNA(train[,i])
  }
}
#test
for (i in 1:ncol(test)) {
  if (is.numeric(test[,i])) {
    test[is.na(test[,i]), i] = mean(test[,i], na.rm = TRUE)
  }
  if (is.factor(test[,i])) {
    test[,i] = addNA(test[,i])
  }
}

# colSums(is.na(test))
sum(is.na(train$summary))
sum(is.na(test$summary))


#Word count for charater data types
library(ngram)
library(dplyr)
train = train %>%
  rowwise() %>%
  mutate(wc_summary = wordcount(summary),
         wc_space = wordcount(space),
         wc_description = wordcount(description),
         wc_neighborhood_overview = wordcount(neighborhood_overview),
         wc_transit = wordcount(transit),
         wc_access = wordcount(access),
         wc_house_rules = wordcount(house_rules),
         wc_host_location = wordcount(host_location),
         wc_host_about = wordcount(host_about),
         wc_host_verifications = wordcount(host_verifications),
         wc_amenities = wordcount(amenities),
         wc_neighbourhood_cleansed = wordcount(neighbourhood_cleansed)
  )

test = test %>%
  rowwise() %>%
  mutate(wc_summary = wordcount(summary),
         wc_space = wordcount(space),
         wc_description = wordcount(description),
         wc_neighborhood_overview = wordcount(neighborhood_overview),
         wc_transit = wordcount(transit),
         wc_access = wordcount(access),
         wc_house_rules = wordcount(house_rules),
         wc_host_location = wordcount(host_location),
         wc_host_about = wordcount(host_about),
         wc_host_verifications = wordcount(host_verifications),
         wc_amenities = wordcount(amenities),
         wc_neighbourhood_cleansed = wordcount(neighbourhood_cleansed)
  )


#Check different types of amenities to be included in the model analysis using Regular Expressions functions
# Then we will choose the common searching amenities to included in rental

# head(train$amenities)
# head(test$amenities)

library(stringr)
library(qdapTools)
library(eply)
# install.packages("eply")

train$amenities = gsub("[^[:alnum:]]", "", train$amenities)
train$amenities = gsub('([[:upper:]])', ' \\1', train$amenities)
train$amenities =  strsplit(train$amenities," ")
train = cbind(train, mtabulate(train$amenities))

test$amenities = gsub("[^[:alnum:]]", "", test$amenities)
test$amenities = gsub('([[:upper:]])', ' \\1', test$amenities)
test$amenities =  strsplit(test$amenities," ")
test = cbind(test, mtabulate(test$amenities))


# neighbourhood_cleansed : multiple factoral levels
train_nc = train %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(n = n(), meanPrice = mean(price)) %>%  
  arrange(desc(n))

train = merge(train, train_nc, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))

## Just get 50 level
train$neighbourhood_cleansed = as.character(train$neighbourhood_cleansed) 
train = train %>%
  mutate(level_nc = ifelse(n > 150, neighbourhood_cleansed, "other"))
train$neighbourhood_cleansed = as.factor(train$neighbourhood_cleansed)
train$level_nc = as.factor(train$level_nc)

test_nc = test %>%
  group_by(neighbourhood_cleansed = neighbourhood_cleansed) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

test = merge(test, test_nc, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))

test$neighbourhood_cleansed = as.character(test$neighbourhood_cleansed)
test = test %>%
  mutate(level_nc = ifelse(n > 30, neighbourhood_cleansed, "other"))
test$level_nc = as.factor(test$level_nc)

## create price_mean_c for score_data 
train2 = data.frame(neighbourhood_cleansed = train_nc$neighbourhood_cleansed,
                    meanPrice = train_nc$meanPrice)

test = merge(test, train2, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"), all.x = TRUE) 

## find NA value
#sum(is.na(test$meanPrice))
test[is.na(test$meanPrice),]$meanPrice = mean(test$meanPrice, na.rm = TRUE)

# neighbourhood_group_cleansed: multiple factoral levels
train3 = train %>%
  group_by(neighbourhood_group_cleansed = neighbourhood_group_cleansed) %>%
  summarize(meanPriceGC = mean(price))

train = merge(train, train3, by = c("neighbourhood_group_cleansed", "neighbourhood_group_cleansed"))
test = merge(test, train3, by = c("neighbourhood_group_cleansed", "neighbourhood_group_cleansed"), all.x = TRUE)

## check NA value
sum(is.na(test$meanPriceGC))

# DATA MODELLING

library(gbm)
set.seed(1708)

boostModelFinal = gbm(price ~ meanPrice+meanPriceGC + level_nc + bedrooms + room_type + property_type + bathrooms + beds 
                      + accommodates + cleaning_fee + monthly_price + security_deposit + minimum_nights + maximum_nights  + neighbourhood_group_cleansed 
                      + host_is_superhost + availability_30 + availability_60 + availability_90 + availability_365 
                      + review_scores_rating + number_of_reviews + last_review_days + first_review_days + review_scores_cleanliness + review_scores_accuracy
                      + wc_transit + wc_summary+ wc_description+ wc_host_about + wc_neighborhood_overview #word count
                      + host_listings_count + host_since_days + reviews_per_month + host_has_profile_pic
                      + extra_people + guests_included + cancellation_policy 
                      + Airconditioning + Dryer + Elevator + Familykidfriendly + Freestreetparking #amenities
                      + Hairdryer + Iron + Oven + Refrigerator + Shampoo + Selfcheckin  #amenities
               ,data = train, distribution = "gaussian", 
               n.trees = 30000,   
               interaction.depth = 5,
               shrinkage = 0.005,
               n.minobsinnode = 5)

summary(boostModelFinal)

## predict train dataset
predboostModelFinal = predict(boostModelFinal, n.trees = 30000)
RMSE = sqrt(mean((predboostModelFinal-train$price)^2)); RMSE
# [1] 37.50427

## predict scoring dataset
predboostModelFinal_test = predict(boostModelFinal, n.trees = 30000, newdata = test)
RMSE = sqrt(mean((predboostModelFinal_test-train$price)^2)); RMSE
# [1] 139.3448

#FILE SUBMISSION
submission <- data.frame(id = test$id, price = predboostModelFinal_test)
write.csv(x = submission, file = "Model Boost Final Predictions 2.csv", row.names = FALSE)

subMix = read.csv('Model Boost Final Predictions 2.csv')
sum(is.na(subMix))

