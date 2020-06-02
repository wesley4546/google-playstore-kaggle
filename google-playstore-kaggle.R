#googleplaystore.csv 

library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)


#reading data

googleplaystore <- read_csv(here::here("data","raw","googleplaystore.csv"))

googleplaystore <- 
  googleplaystore %>% 
  clean_names() %>% 
  filter(!category == '1.9') %>% 
  na.omit()

category_eda <-
  googleplaystore %>% 
  mutate(category = as_factor(category)) %>% 
  add_count(category) %>% 
  mutate(category = fct_reorder(category, n))
 
category_eda %>% 
  ggplot(aes(x = category, fill = content_rating)) +
  geom_bar() +
  coord_flip()

# googleplaystore %>% 
#   ggplot(aes(rating)) +
#   geom_histogram() +
#   facet_grid(~category)


# apriori -----------------------------------------------------------------
library(arules)

market_data <- 
  googleplaystore %>% 
  select(-c(app, current_ver, last_updated, android_ver, size, price, reviews)) %>% 
  mutate(category = factor(category)) %>% 
  mutate(installs = factor(installs)) %>% 
  mutate(content_rating = factor(content_rating)) %>% 
  mutate(genres = factor(genres)) %>% 
  mutate(rating = factor(
    case_when(rating < 2.0 ~ '1' ,
              rating < 3.0 ~ '2' ,
              rating < 4.0 ~ '3' ,
              rating < 5.0 ~ '4' ,
              TRUE ~ '5'
    ),
    levels = c('1','2','3','4','5'))) %>% 
  mutate(type = factor(type)) %>% 
  filter(!type == "NaN")




playstore_apriori <- apriori(market_data)

test <- tibble(inspect(playstore_apriori))



# KSVM ---------------------------------------------------------------------
library(e1071)
library(caTools)
library(kernlab)

svm_data <- 
  googleplaystore %>% 
  select(-c(app, last_updated, current_ver, android_ver)) %>% 
  mutate(size = as.numeric(str_remove(size, "M"))) %>% 
  mutate(price = as.numeric(str_remove(price, "\\$"))) %>% 
  mutate(installs = factor(installs)) %>% 
  mutate(content_rating = factor(content_rating)) %>% 
  mutate(genres = factor(genres)) %>% 
  filter(!rating == "NaN")
  

model_data <-
  googleplaystore %>% 
  select(-c(app, last_updated, current_ver, android_ver)) %>%
  mutate(type = as.integer(if_else(type == "Free", 0, 1))) %>% 
  mutate(price = as.numeric(str_remove(price, "\\$"))) %>% 
  mutate(size = as.numeric(str_remove(size, "M"))) %>% 
  mutate(installs = as.integer(str_remove_all(str_remove(installs, "\\+"), ","))) %>% 
  mutate(content_rating = as.integer(factor(
    case_when(
      content_rating == "Everyone" ~ 1,
      content_rating == "Teen" ~ 2,
      content_rating == "Everyone 10+" ~ 3,
      content_rating == "Mature 17+" ~ 4,
      content_rating == "Adults only 18+" ~ 5,
      content_rating == "Unrated" ~ 6
    ),
    levels = c('1','2','3','4','5','6')
  )))



dummy <- as.data.frame(model.matrix(~ category -1 , data = model_data))

df <- cbind(model_data, dummy) %>% select(-c(category)) %>% na.omit()


split <- sample.split(df$rating, SplitRatio = .7)

train_play <- subset(df, split == TRUE)
test_play <- subset(df, split == FALSE)


ksvm_model <- ksvm(rating ~ ., data = train_play)

ksvm_predict <- predict(ksvm_model, test_play)

confmatrix_ksvm <- table(ksvm_predict, test_play$genres)

totalcorrect <- confmatrix_ksvm[1,1] + confmatrix_ksvm[2,2]

total <- nrow(test_play)

percentage <- totalcorrect / total



