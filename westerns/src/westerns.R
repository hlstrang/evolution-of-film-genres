#The evolution of genres over time
#Data : TMDb + IMDb via Kagglehub

library(tidyverse)
library(stringr)
library(lubridate)
library(randomForest)
library(caret)
library(ggrepel)
library(slider)
library(factoextra)
setwd("Documents/programming/tmdb_proj_0526")
rm(list = ls())

df <- read_csv("TMDB  IMDB Movies Dataset.csv")

##Westerns

#Filtering
all_films <- df %>%
  filter(!is.na(release_date)) %>%
  mutate(year = year(as.Date(release_date))) %>%
  filter(runtime != 0 & runtime > 40) 

total_films_per_year <- all_films %>%
  group_by(year) %>%
  summarise(total_films = n())

western_df <- all_films %>%
  filter(str_detect(genres, "Western")) %>%
  filter(!is.na(release_date)) %>%
  mutate(year = year(as.Date(release_date)),
         decade = as.factor((year %/% 10) * 10),
         combinedRating = (averageRating + vote_average)/2,
         profit = revenue - budget) %>%
  mutate(rating_category = cut(combinedRating, 
                               breaks = c(0, 4, 7, 10), 
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE))

western_df <- western_df %>%
  mutate(
    lead_actor = str_extract(cast, "^[^,]+") 
  ) %>%
  arrange(lead_actor, release_date)

western_df <- western_df %>%
  group_by(lead_actor) %>%
  mutate(
    star_power_score = slide_dbl(profit, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(star_power_score = if_else(is.na(star_power_score), median(profit, na.rm = TRUE), star_power_score))

western_df <- western_df %>%
  mutate(
    lead_director = str_extract(directors, "^[^,]+") 
  ) %>%
  group_by(lead_director) %>%
  mutate(
    director_power_score = slide_dbl(profit, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(director_power_score = if_else(is.na(director_power_score), median(profit, na.rm = TRUE), director_power_score))

western_counts_per_year <- western_df %>%
  group_by(year) %>%
  summarise(western_count = n())

western_proportion_per_year <- western_counts_per_year %>%
  left_join(total_films_per_year, by = "year") %>%
  mutate(proportion = western_count / total_films)

#Visualising
western_df %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Western Films Through the Ages", x = "Year", y = "Number of Films") +
  theme_minimal()

ggplot(western_proportion_per_year, aes(x = year, y = proportion)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Proportion of Western Films by Year (Normalised)",
    x = "Year",
    y = "Proportion of Total Films"
  ) +
  theme_minimal()

ggplot(western_df, aes(x=decade, y= combinedRating))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(aes(color = rating_category), 
              alpha = 0.5, 
              width = 0.3) +
  labs(x = "Decade",
       y="Average Rating", 
       title = "Ratings of Western Films over Time",
       color = "Rating Category") +
  theme_minimal()

#Random forest for predicting ratings
set.seed(42)

avergae_popularity_per_year <- western_df %>%
  group_by(year) %>%
  summarise(average_popularity = mean(popularity))

western_df <- western_df %>%
  left_join(avergae_popularity_per_year, by = "year") %>%
  mutate(relative_popularity = popularity / average_popularity)

rating_rf <- western_df %>%
  mutate(
    original_language = as.factor(original_language),
    release_date = as.Date(release_date), 
    release_month = as.factor(month(release_date, label = TRUE)),
    season = case_when(
      release_month %in% c("May", "Jun", "Jul") ~ "Summer",
      release_month %in% c("Nov", "Dec")      ~ "Winter",
      release_month %in% c("Mar", "Apr")      ~ "Spring",
      TRUE                                    ~ "Off_Peak"
    ),season = as.factor(season),
    numVotes = log10(numVotes)) %>%
  select(title, runtime, popularity, original_language, 
         director_power_score, star_power_score, season,
         combinedRating, numVotes, relative_popularity, lead_actor,
         lead_director) %>%
  drop_na()

trainIndex <- sample(1:nrow(rating_rf), 0.8 * nrow(rating_rf))
trainData <- rating_rf[trainIndex, ]
testData <- rating_rf[-trainIndex, ]
rating_rf_model <- randomForest(combinedRating ~ . - title - lead_actor - lead_director, data = trainData)
print(rating_rf_model)

predictions <- predict(rating_rf_model, testData)
error <- predictions - testData$combinedRating

rmse <- sqrt(mean(error^2))
print(paste("Average error: ", rmse))

ggplot(data.frame(actual = testData$combinedRating, predicted = predictions), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual Rating vs. Predicted Rating",
       x = "Actual Rating", y = "Predicted Rating") +
  theme_minimal()

importance(rating_rf_model)
varImpPlot(rating_rf_model)

rating_rf_classified <- rating_rf %>%
  mutate(rating_level = as.factor(if_else(combinedRating > median(combinedRating), "High", "Low"))) %>%
  select(-combinedRating)
trainIndex <- sample(1:nrow(rating_rf_classified), 0.8 * nrow(rating_rf_classified))
trainData <- rating_rf_classified[trainIndex, ]
testData <- rating_rf_classified[-trainIndex, ]
rating_classified_model <- randomForest(rating_level ~ . - title - lead_actor - lead_director, data = trainData, proximity = TRUE)
print(rating_classified_model)

predictions <- predict(rating_classified_model, testData)
confusionMatrix(predictions, testData$rating_level)

importance(rating_classified_model)
varImpPlot(rating_classified_model)

MDSplot(rating_classified_model, trainData$rating_level)
legend("topright", 
       legend = levels(trainData$rating_level), 
       fill = c("red", "blue"))

features <- rating_rf %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ scale(.x) %>% as.vector))

wss <- sapply(1:10, function(k) {
  kmeans(features, centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "WSS")

fviz_nbclust(features, kmeans, method='silhouette')

k <- 2
kmeans_model <- kmeans(features, centers = k, nstart = 10)

rating_rf_clusters <- rating_rf %>%
  mutate(
    cluster = as.factor(kmeans_model$cluster),
    rating_class = case_when(
      cluster == 1 ~ "High",
      cluster == 2 ~ "Low",
      .default = "Unknown" 
    )
  )

ggplot(rating_rf_clusters, aes(x = rating_class, y = combinedRating, color = rating_class)) +
  geom_boxplot() +
  geom_jitter() +
  labs(title = "Combined Rating by Cluster", x = "Rating Class", y = "Combined Rating") +
  theme_minimal()

trainIndex <- sample(1:nrow(rating_rf_clusters), 0.8 * nrow(rating_rf_clusters))
trainData <- rating_rf_clusters[trainIndex, ]
testData <- rating_rf_clusters[-trainIndex, ]

trainData$rating_class <- as.factor(trainData$rating_class)
testData$rating_class <- as.factor(testData$rating_class)

rating_cluster_model <- randomForest(
  rating_class ~ . - title - combinedRating - cluster - lead_actor - lead_director, 
  data = trainData,
  proximity = TRUE
)

print(rating_cluster_model)
predictions <- predict(rating_cluster_model, testData)
confusionMatrix(predictions, testData$rating_class)
varImpPlot(rating_cluster_model)

MDSplot(rating_cluster_model, trainData$rating_class)
legend("topright", 
       legend = levels(trainData$rating_class), 
       fill = c("red", "blue"))

rf_prox <- randomForest(
  rating_class ~ . - title - combinedRating - cluster,
  data = trainData,
  proximity = TRUE
)$proximity

mds_coords <- cmdscale(1 - rf_prox, k = 2)

coords_map <- trainData %>%
  mutate(Dim1 = mds_coords[,1], Dim2 = mds_coords[,2])

ggplot(coords_map, aes(x = Dim1, y = Dim2, color = rating_class)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  theme_minimal() +
  labs(title = "MDS Plot: Rating Classes from Clustering", x = "Dimension 1", y = "Dimension 2",
       color = "Rating Class")

features <- trainData %>%
  select(where(is.numeric), - combinedRating, - cluster)

cor_matrix <- cor(features, mds_coords)
colnames(cor_matrix) <- c("Dim1", "Dim2")

multiplier <- 0.6
arrow_data <- as.data.frame(cor_matrix * multiplier) %>%
  mutate(feature = rownames(cor_matrix))

ggplot(coords_map, aes(x = Dim1, y = Dim2)) +
  geom_jitter(aes(color = rating_class), alpha = 0.5, size = 2, width = 0.01, height = 0.01) +
  scale_color_manual(values = c("High" = "#E41A1C", "Low" = "#377EB8")) +
  geom_segment(data = arrow_data, aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "grey20", size = 1, alpha = 0.8) +
  geom_text_repel(data = arrow_data, aes(x = Dim1, y = Dim2, label = feature),
                  color = "black", fontface = "bold", size = 4,
                  box.padding = 0.5, segment.color = NA) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.2) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "MDS Rating Projection: Distances reflect Random Forest similarity",
    x = "Dimension 1 (Dominant Patterns)",
    y = "Dimension 2 (Secondary Variance)",
    color = "Rating Class"
  )


#Random forest for predicting profit
set.seed(42)

profit_rf <- western_df %>%
  filter(profit != 0,
         budget != 0) %>%
  mutate(
    original_language = as.factor(original_language),
    release_date = as.Date(release_date), 
    release_month = as.factor(month(release_date, label = TRUE)),
    season = case_when(
      release_month %in% c("May", "Jun", "Jul") ~ "Summer",
      release_month %in% c("Nov", "Dec")      ~ "Winter",
      release_month %in% c("Mar", "Apr")      ~ "Spring",
      TRUE                                    ~ "Off_Peak"
    ),season = as.factor(season),
    numVotes = log10(numVotes)) %>%
  dplyr::select(title, profit, budget, runtime, popularity, original_language, 
                director_power_score, star_power_score, season,
                numVotes, lead_actor, lead_director, relative_popularity) %>%
  drop_na()

trainIndex <- sample(1:nrow(profit_rf), 0.8 * nrow(profit_rf))
trainData <- profit_rf[trainIndex, ]
testData <- profit_rf[-trainIndex, ]
profit_rf_model <- randomForest(profit ~ . - title - lead_actor - lead_director,
                                data = trainData)
print(profit_rf_model)

predictions <- predict(profit_rf_model, testData)
error <- predictions - testData$profit

rmse <- sqrt(mean(error^2))
print(paste("Average error in dollars: ", rmse))

ggplot(data.frame(actual = testData$profit, predicted = predictions), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual Profit vs. Predicted Profit",
       subtitle = "Dots closer to the red line are more accurate",
       x = "Actual Profit", y = "Predicted Profit") +
  theme_minimal()

varImpPlot(profit_rf_model)

profit_rf_classified <- profit_rf %>%
  mutate(profit_level = as.factor(if_else(profit > median(profit), "High", "Low"))) %>%
  select(-profit)
trainIndex <- sample(1:nrow(profit_rf_classified), 0.8 * nrow(profit_rf_classified))
trainData <- profit_rf_classified[trainIndex, ]
testData <- profit_rf_classified[-trainIndex, ]
profit_classified_model <- randomForest(profit_level ~ . - title - lead_actor - lead_director, data = trainData, proximity = TRUE)
print(profit_classified_model)

predictions <- predict(profit_classified_model, testData)
confusionMatrix(predictions, testData$profit_level)

varImpPlot(profit_classified_model)

MDSplot(profit_classified_model, trainData$profit_level)
legend("topright", 
       legend = levels(trainData$profit_level), 
       fill = c("red", "blue"))

features <- profit_rf %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ scale(.x) %>% as.vector))

wss <- sapply(1:10, function(k) {
  kmeans(features, centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "WSS")

fviz_nbclust(features, kmeans, method='silhouette')

k <- 2
kmeans_model <- kmeans(features, centers = k, nstart = 10)

profit_rf_clusters <- profit_rf %>%
  mutate(
    cluster = as.factor(kmeans_model$cluster),
    profit_class = case_when(
      cluster == 1 ~ "High",
      cluster == 2 ~ "Low",
      .default = "Unknown" 
    )
  )

ggplot(profit_rf_clusters, aes(x = profit_class, y = profit, color = profit_class)) +
  geom_boxplot() +
  geom_jitter() +
  labs(title = "Combined Rating by Cluster", x = "Rating Class", y = "Combined Rating") +
  theme_minimal()

trainIndex <- sample(1:nrow(profit_rf_clusters), 0.8 * nrow(profit_rf_clusters))
trainData <- profit_rf_clusters[trainIndex, ]
testData <- profit_rf_clusters[-trainIndex, ]

trainData$profit_class <- as.factor(trainData$profit_class)
testData$profit_class <- as.factor(testData$profit_class)

profit_cluster_model <- randomForest(
  profit_class ~ . - title - profit - cluster - lead_actor - lead_director, 
  data = trainData,
  proximity = TRUE
)

print(profit_cluster_model)
predictions <- predict(profit_cluster_model, testData)
confusionMatrix(predictions, testData$profit_class)
varImpPlot(profit_cluster_model)

MDSplot(profit_cluster_model, trainData$profit_class)
legend("topleft", 
       legend = levels(trainData$profit_class), 
       fill = c("red", "blue"))

rf_prox <- randomForest(
  profit_class ~ . - title - profit - cluster
  - lead_actor - lead_director,
  data = trainData,
  proximity = TRUE
)$proximity

mds_coords <- cmdscale(1 - rf_prox, k = 2)

coords_map <- trainData %>%
  mutate(Dim1 = mds_coords[,1], Dim2 = mds_coords[,2])

features <- trainData %>%
  select(where(is.numeric), - profit, - cluster)

cor_matrix <- cor(features, mds_coords)
colnames(cor_matrix) <- c("Dim1", "Dim2")

multiplier <- 0.6
arrow_data <- as.data.frame(cor_matrix * multiplier) %>%
  mutate(feature = rownames(cor_matrix))

ggplot(coords_map, aes(x = Dim1, y = Dim2)) +
  geom_jitter(aes(color = profit_class), alpha = 0.5, size = 2, width = 0.01, height = 0.01) +
  scale_color_manual(values = c("High" = "#E41A1C", "Low" = "#377EB8")) +
  geom_segment(data = arrow_data, aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "grey20", size = 1, alpha = 0.8) +
  geom_text_repel(data = arrow_data, aes(x = Dim1, y = Dim2, label = feature),
                  color = "black", fontface = "bold", size = 4,
                  box.padding = 0.5, segment.color = NA) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.2) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "MDS Profit Projection: Distances reflect Random Forest similarity",
    x = "Dimension 1 (Dominant Patterns)",
    y = "Dimension 2 (Secondary Variance)",
    color = "Profit Class"
  )

#Linear models 
profit_rf$original_language <- relevel(as.factor(profit_rf$original_language), ref = "en")
model1 <- glm(profit ~ runtime + budget + popularity + star_power_score + director_power_score
              + numVotes + relative_popularity + original_language, data = profit_rf)
summary(model1)

profit_long <- profit_rf %>%
  select(profit, popularity, star_power_score) %>%
  pivot_longer(
    cols = c(popularity, star_power_score),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "star_power_score" ~ "Star Average Career Profit",
    Metric == "popularity" ~ "Popularity"
  ))
ggplot(profit_long, aes(x = Value, y = profit)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = "Profit ($)")

rating_rf$original_language <- relevel(as.factor(rating_rf$original_language), ref = "en")
model2 <- glm(combinedRating ~ runtime + star_power_score + director_power_score + 
                popularity + season + relative_popularity + numVotes + original_language
              , data = rating_rf)
summary(model2)

rating_long <- rating_rf %>%
  select(combinedRating, runtime, popularity, relative_popularity, numVotes) %>%
  pivot_longer(
    cols = c(runtime, popularity, relative_popularity, numVotes),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "runtime" ~ "Runtime (mins)",
    Metric == "popularity" ~ "Popularity",
    Metric == "relative_popularity" ~ "Relative Popularity",
    Metric == "numVotes" ~ "Number of IMDb Votes (log10)"
  ))
ggplot(rating_long, aes(x = Value, y = combinedRating)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = "Average Rating")

model2_df <- as.data.frame(summary(model2)$coefficients)
colnames(model2_df) <- c("Estimate", "StdError", "tValue", "PValue")

sig_languages <- model2_df[grepl("original_language", rownames(model2_df)) & model2_df$PValue < 0.05, ]
clean_names <- gsub("original_language", "", rownames(sig_languages))

sig_languages %>%
  rownames_to_column("Language") %>%
  ggplot(aes(x = reorder(Language, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Significant Language Effects on Rating", x = "Language", y = "Coefficient Estimate")

sig_lang_codes <- gsub("original_language", "", 
                       rownames(model2_df)[grepl("original_language", 
                                                 rownames(model2_df)) & 
                                             model2_df$PValue < 0.05])
target_languages <- c(sig_lang_codes, "en")
lang_df <- rating_rf %>%
  filter(original_language %in% target_languages) %>%
  group_by(original_language) %>%
  filter(n() > 10) %>%
  ungroup() %>%
  mutate(original_language = recode(original_language,
                                    "de" = "german", 
                                    "en" = "english",
                                    "it" = "italian",
                                    "es" = "spanish",
                                    "nl" = "dutch",
                                    "pt" = "portugese"))

ggplot(lang_df, aes(x = original_language, y = combinedRating, colour = original_language)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.3) +
  labs(title = "Significant Language Effects on Rating", x ="Language",
       y = "Average Rating", color = "Language") +
  theme_minimal()
