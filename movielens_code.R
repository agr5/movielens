# ##########################################################
# # Create edx set, validation set (final hold-out test set)
# ##########################################################
# 
# # Note: this process could take a couple of minutes
# 
# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# # if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



# We can save the files to a csv, so the loading times are faster next time
# write.csv(edx,'./edx.csv')
# write.csv(validation,'./validation.csv')
# edx<-read.csv('./edx.csv')
# validation<-read.csv('./validation.csv')


edx<-edx%>%mutate(guk = paste(userId,genres))
validation<-validation%>%mutate(guk = paste(userId,genres))

# Support functions
RMSE <- function(actuals,forecast){
  sqrt(mean((actuals-forecast)**2))
}

# What about creating user clustering groups with similar tastes
# it sounds awfully complex

data_model<-edx
val_data_model<-validation

# Given we might end up with missing values
# We will have a model which produces a "base rating"
# And then we will include the other effects
base_rating <- mean(data_model$rating)

data_model <- data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

val_data_model <- val_data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)


head(data_model)

# movies_coefs
# First we will calculate the usual score given to each movie
movies_coefs <-  data_model %>% as_tibble() %>% group_by(movieId) %>% 
  summarise(mf = mean(resid)) %>% 
  select(movieId, mf)


#data_model<-data_model%>%mutate(br=base_rating)
data_model<-merge(data_model, movies_coefs,
                  by.x = c("movieId"), 
                  by.y = c("movieId"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf, resid=rating-fcast_rating)


val_data_model<-val_data_model%>%mutate(br=base_rating)
val_data_model<-merge(val_data_model, movies_coefs,
                      by.x = c("movieId"), 
                      by.y = c("movieId"), 
                      all.x = TRUE, 
                      all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf, resid=rating-fcast_rating)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)



# user_coefs
# Second we will calculate the usual score given by each user
user_coefs <-  data_model %>% group_by(userId) %>% 
  summarise(uf = mean(resid)) %>% 
  select(userId, uf)


data_model<-merge(data_model, user_coefs,
                  by.x = c("userId"), 
                  by.y = c("userId"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)


val_data_model<-merge(val_data_model, user_coefs,
                      by.x = c("userId"), 
                      by.y = c("userId"), 
                      all.x = TRUE, 
                      all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)


# genre_coefs
# First we will calculate the usual score given by each user to each genre
guk_coefs <-  data_model %>% group_by(guk) %>% 
  summarise(gf = mean(resid)) %>% 
  select(guk, gf)


data_model<-merge(data_model, guk_coefs,
                  by.x = c("guk"), 
                  by.y = c("guk"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)

val_data_model<-merge(val_data_model, guk_coefs,
                      by.x = c("guk"), 
                      by.y = c("guk"), 
                      all.x = TRUE, 
                      all.y = FALSE) 

# Fill Missing Values
val_data_model$gf[is.na(val_data_model$gf)]<-0
val_data_model<-val_data_model %>% mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)

# The change in RMSE over the validation set, shows a clear case of "overfitting", where we
# are correcting very well for our training set, but the adjustment is not proving correct
# with the validation set.

# In order to understand what is happening, we will split out edx training set, into a two subsets
# Train and Test, and subset this two splits to try to understand what is happening with the validation
# set, following the spirit of the exercise which is "not to use the validation set only, than to measure
# RMSE results".

set.seed(42, sample.kind="Rounding")
test_index <- createDataPartition(edx$movieId, p=0.3, list = FALSE)
edx_train<-edx[-test_index,] # test dataset
edx_test<-edx[test_index,] # test dataset

# If we were to retrain our model but this time using this subsets of training/test, we could check the largest
# differences instances, and driver conclusions from there.


data_model<-edx_train
val_data_model<-edx_test

# Given we might end up with missing values
# We will have a model which produces a "base rating"
# And then we will include the other effects
base_rating <- mean(data_model$rating)

data_model <- data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

val_data_model <- val_data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)


# movies_coefs
# First we will calculate the usual score given to each movie
movies_coefs <-  data_model %>% as_tibble() %>% group_by(movieId) %>% 
  summarise(mf = mean(resid)) %>% 
  select(movieId, mf)


#data_model<-data_model%>%mutate(br=base_rating)
data_model<-merge(data_model, movies_coefs,
                  by.x = c("movieId"), 
                  by.y = c("movieId"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf, resid=rating-fcast_rating)


val_data_model<-val_data_model%>%mutate(br=base_rating)
val_data_model<-merge(val_data_model, movies_coefs,
                      by.x = c("movieId"), 
                      by.y = c("movieId"), 
                      all.x = TRUE, 
                      all.y = FALSE)

val_data_model$mf[is.na(val_data_model$mf)]<-0
val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf, resid=rating-fcast_rating)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)



# user_coefs
# Second we will calculate the usual score given by each user
user_coefs <-  data_model %>% group_by(userId) %>% 
  summarise(uf = mean(resid)) %>% 
  select(userId, uf)


data_model<-merge(data_model, user_coefs,
                  by.x = c("userId"), 
                  by.y = c("userId"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)


val_data_model<-merge(val_data_model, user_coefs,
                      by.x = c("userId"), 
                      by.y = c("userId"), 
                      all.x = TRUE, 
                      all.y = FALSE)

val_data_model$uf[is.na(val_data_model$uf)]<-0
val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)


# genre_coefs
# First we will calculate the usual score given by each user to each genre
guk_coefs <-  data_model %>% group_by(guk) %>% 
  summarise(gf = mean(resid)) %>% 
  select(guk, gf)


data_model<-merge(data_model, guk_coefs,
                  by.x = c("guk"), 
                  by.y = c("guk"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)

val_data_model<-merge(val_data_model, guk_coefs,
                      by.x = c("guk"), 
                      by.y = c("guk"), 
                      all.x = TRUE, 
                      all.y = FALSE) 

# Fill Missing Values
val_data_model$gf[is.na(val_data_model$gf)]<-0
val_data_model<-val_data_model %>% mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)

RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)

val_data_model %>% 
  arrange(desc(resid))%>%
  head()

val_data_model %>% filter(guk=="37651 Comedy|Drama") %>%
  head()

data_model %>% filter(guk=="37651 Comedy|Drama") %>%
  head()


## Regularization

### User Reg Parameter
data_model<-edx_train
val_data_model<-edx_test

base_rating <- mean(data_model$rating)

data_model <- data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

val_data_model <- val_data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

# This will remain constant
movies_coefs <-  data_model %>% as_tibble() %>% group_by(movieId) %>% 
  summarise(mf = mean(resid)) %>% 
  select(movieId, mf)


# Loop
user_reg_params = c(1:10)
validation_rmse<-list()

for (user_reg_param in user_reg_params) {
  
  ### User Reg Parameter
  data_model<-edx_train
  val_data_model<-edx_test
  
  base_rating <- mean(data_model$rating)
  
  data_model <- data_model %>% mutate(
    br=base_rating,
    fcast_rating=br,
    resid=rating-fcast_rating
  )
  
  val_data_model <- val_data_model %>% mutate(
    br=base_rating,
    fcast_rating=br,
    resid=rating-fcast_rating
  )
  
  # This will remain constant
  movies_coefs <-  data_model %>% as_tibble() %>% group_by(movieId) %>% 
    summarise(mf = mean(resid)) %>% 
    select(movieId, mf)
  
  data_model<-merge(data_model, movies_coefs,
                    by.x = c("movieId"), 
                    by.y = c("movieId"), 
                    all.x = TRUE, 
                    all.y = FALSE) %>% 
    mutate(fcast_rating=br+mf, resid=rating-fcast_rating)
  
  
  val_data_model<-val_data_model%>%mutate(br=base_rating)
  val_data_model<-merge(val_data_model, movies_coefs,
                        by.x = c("movieId"), 
                        by.y = c("movieId"), 
                        all.x = TRUE, 
                        all.y = FALSE)
  
  val_data_model$mf[is.na(val_data_model$mf)]<-0
  val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf, resid=rating-fcast_rating)
  
  
  user_coefs <-  data_model %>% group_by(userId) %>% 
    summarise(suf = sum(resid), n=n(), uf = suf/(user_reg_param + n)) %>% 
    select(userId, uf)
  
  data_model<-merge(data_model, user_coefs,
                    by.x = c("userId"), 
                    by.y = c("userId"), 
                    all.x = TRUE, 
                    all.y = FALSE) %>% 
    mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)
  
  
  val_data_model<-merge(val_data_model, user_coefs,
                        by.x = c("userId"), 
                        by.y = c("userId"), 
                        all.x = TRUE, 
                        all.y = FALSE)
  
  val_data_model$uf[is.na(val_data_model$uf)]<-0
  val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)
  
  # RMSE(data_model$rating, data_model$fcast_rating)
  # RMSE(val_data_model$rating, val_data_model$fcast_rating)
  validation_rmse[[user_reg_param]]<-RMSE(val_data_model$rating, val_data_model$fcast_rating)
  
}


plot(user_reg_params,validation_rmse)




# Loop guk params
guk_reg_params = c(1:10)
validation_rmse_guk<-list()

for (guk_reg_param in guk_reg_params) {
  
  ### GUK Reg Parameter
  data_model<-edx_train
  val_data_model<-edx_test
  
  base_rating <- mean(data_model$rating)
  
  data_model <- data_model %>% mutate(
    br=base_rating,
    fcast_rating=br,
    resid=rating-fcast_rating
  )
  
  val_data_model <- val_data_model %>% mutate(
    br=base_rating,
    fcast_rating=br,
    resid=rating-fcast_rating
  )
  
  # This will remain constant
  movies_coefs <-  data_model %>% as_tibble() %>% group_by(movieId) %>% 
    summarise(mf = mean(resid)) %>% 
    select(movieId, mf)
  
  data_model<-merge(data_model, movies_coefs,
                    by.x = c("movieId"), 
                    by.y = c("movieId"), 
                    all.x = TRUE, 
                    all.y = FALSE) %>% 
    mutate(fcast_rating=br+mf, resid=rating-fcast_rating)
  
  
  val_data_model<-val_data_model%>%mutate(br=base_rating)
  val_data_model<-merge(val_data_model, movies_coefs,
                        by.x = c("movieId"), 
                        by.y = c("movieId"), 
                        all.x = TRUE, 
                        all.y = FALSE)
  
  val_data_model$mf[is.na(val_data_model$mf)]<-0
  val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf, resid=rating-fcast_rating)
  
  user_reg_param <- 5
  user_coefs <-  data_model %>% group_by(userId) %>% 
    summarise(suf = sum(resid), n=n(), uf = suf/(user_reg_param + n)) %>% 
    select(userId, uf)
  
  data_model<-merge(data_model, user_coefs,
                    by.x = c("userId"), 
                    by.y = c("userId"), 
                    all.x = TRUE, 
                    all.y = FALSE) %>% 
    mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)
  
  
  val_data_model<-merge(val_data_model, user_coefs,
                        by.x = c("userId"), 
                        by.y = c("userId"), 
                        all.x = TRUE, 
                        all.y = FALSE)
  
  val_data_model$uf[is.na(val_data_model$uf)]<-0
  val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)
  
  
  guk_coefs <-  data_model %>% group_by(guk) %>% 
    summarise(sgf = sum(resid), n=n(), gf = sgf/(guk_reg_param + n)) %>% 
    select(guk, gf)
  
  
  data_model<-merge(data_model, guk_coefs,
                    by.x = c("guk"), 
                    by.y = c("guk"), 
                    all.x = TRUE, 
                    all.y = FALSE) %>% 
    mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)
  
  val_data_model<-merge(val_data_model, guk_coefs,
                        by.x = c("guk"), 
                        by.y = c("guk"), 
                        all.x = TRUE, 
                        all.y = FALSE) 
  
  # Fill Missing Values
  val_data_model$gf[is.na(val_data_model$gf)]<-0
  val_data_model<-val_data_model %>% mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)
  
  
  # RMSE(data_model$rating, data_model$fcast_rating)
  # RMSE(val_data_model$rating, val_data_model$fcast_rating)
  validation_rmse_guk[[guk_reg_param]]<-RMSE(val_data_model$rating, val_data_model$fcast_rating)
  
}


plot(guk_reg_params,validation_rmse_guk)





# Final Model
data_model<-edx
val_data_model<-validation

base_rating <- mean(data_model$rating)

data_model <- data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

val_data_model <- val_data_model %>% mutate(
  br=base_rating,
  fcast_rating=br,
  resid=rating-fcast_rating
)

# This will remain constant
movies_coefs <-  data_model %>% as_tibble() %>% group_by(movieId) %>% 
  summarise(mf = mean(resid)) %>% 
  select(movieId, mf)

data_model<-merge(data_model, movies_coefs,
                  by.x = c("movieId"), 
                  by.y = c("movieId"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf, resid=rating-fcast_rating)


val_data_model<-val_data_model%>%mutate(br=base_rating)
val_data_model<-merge(val_data_model, movies_coefs,
                      by.x = c("movieId"), 
                      by.y = c("movieId"), 
                      all.x = TRUE, 
                      all.y = FALSE)

val_data_model$mf[is.na(val_data_model$mf)]<-0
val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf, resid=rating-fcast_rating)

user_reg_param <- 5
user_coefs <-  data_model %>% group_by(userId) %>% 
  summarise(suf = sum(resid), n=n(), uf = suf/(user_reg_param + n)) %>% 
  select(userId, uf)

data_model<-merge(data_model, user_coefs,
                  by.x = c("userId"), 
                  by.y = c("userId"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)


val_data_model<-merge(val_data_model, user_coefs,
                      by.x = c("userId"), 
                      by.y = c("userId"), 
                      all.x = TRUE, 
                      all.y = FALSE)

val_data_model$uf[is.na(val_data_model$uf)]<-0
val_data_model<-val_data_model%>%mutate(fcast_rating=br+mf+uf, resid=rating-fcast_rating)


guk_reg_param <-5
guk_coefs <-  data_model %>% group_by(guk) %>% 
  summarise(sgf = sum(resid), n=n(), gf = sgf/(guk_reg_param + n)) %>% 
  select(guk, gf)


data_model<-merge(data_model, guk_coefs,
                  by.x = c("guk"), 
                  by.y = c("guk"), 
                  all.x = TRUE, 
                  all.y = FALSE) %>% 
  mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)

val_data_model<-merge(val_data_model, guk_coefs,
                      by.x = c("guk"), 
                      by.y = c("guk"), 
                      all.x = TRUE, 
                      all.y = FALSE) 

# Fill Missing Values
val_data_model$gf[is.na(val_data_model$gf)]<-0
val_data_model<-val_data_model %>% mutate(fcast_rating=br+mf+uf+gf, resid=rating-fcast_rating)


RMSE(data_model$rating, data_model$fcast_rating)
RMSE(val_data_model$rating, val_data_model$fcast_rating)



