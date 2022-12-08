spotify <- spotify_duplicate_songs_removed

library(fastDummies)
library('sjPlot')
library(rsample)
library(tidyverse)
library(ggplot2)

spotify <- dummy_cols(spotify, select_columns = "artist")
spotify <- dummy_cols(spotify, select_columns = "explicit")
spotify <- dummy_cols(spotify, select_columns = "time_signature")

spotify_split <- initial_split(spotify, prop = 0.8)
spotify_train <- training(spotify_split)
spotify_test <- testing(spotify_split)

#with artists
mod <- lm(popularity ~ total_followers + energy + liveness + loudness 
          + instrumentalness + tempo + valence + danceability + speechiness 
          + acousticness + duration_ms + artist + explicit + time_signature, data = spotify_train)

#without artists
mod2 <- lm(popularity ~ total_followers + energy + liveness + loudness 
          + instrumentalness + tempo + valence + danceability + speechiness 
          + acousticness + duration_ms + explicit + time_signature, data = spotify_train)

#with artist
preds_train <- predict(mod, newdata = spotify_train)
preds_test <- predict(mod, newdata = spotify_test)

#without artist
preds_train2 <- predict(mod2, newdata = spotify_train)
preds_test2 <- predict(mod2, newdata = spotify_test)

#with artist
results_train <- tibble('preds' = preds_train, 'true' = spotify_train$popularity, 'type' = "train")
results_test <- tibble('preds' = preds_test, 'true' = spotify_test$popularity, 'type' = "test")
results_df <- bind_rows(results_train, results_test)

#without artist
results_train2 <- tibble('preds' = preds_train, 'true' = spotify_train$popularity, 'type' = "train")
results_test2 <- tibble('preds' = preds_test, 'true' = spotify_test$popularity, 'type' = "test")
results_df2 <- bind_rows(results_train2, results_test2)

ggplot(results_df, aes(x = true, y = preds)) + geom_point(aes(color = type)) + geom_abline(color = "red") +
  facet_wrap(~type) + theme_minimal(base_size = 16) + theme(legend.position = "bottom")
plot_model(mod, type = 'pred', terms = 'popularity', show.data = TRUE)

ggplot(results_df2, aes(x = true, y = preds)) + geom_point(aes(color = type)) + geom_abline(color = "red") +
  facet_wrap(~type) + theme_minimal(base_size = 16) + theme(legend.position = "bottom")
plot_model(mod, type = 'pred', terms = 'popularity', show.data = TRUE)

getrmse <- function(true, predictions){
  sqrt(mean((true - predictions)^2))
}

getrmse(spotify_train$popularity, preds_train2)

getrmse(spotify_test$popularity, preds_test2)


summary(mod)
tab_model(mod)
tab_model(mod2)
plot_model(mod2)
