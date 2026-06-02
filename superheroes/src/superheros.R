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
library(quantmod)
setwd("Documents/programming/tmdb_proj_0526")
rm(list = ls())

df <- read_csv("TMDB  IMDB Movies Dataset.csv")

##Superheros

#Filtering
getSymbols("CPIAUCSL", src = "FRED")
cpi_data <- as.data.frame(CPIAUCSL) %>%
  rename(cpi = CPIAUCSL) %>%
  tibble::rownames_to_column("date") %>%  
  mutate(
    date = as.Date(date), 
    year = lubridate::year(date) 
  ) %>%
  select(year, cpi)

cpi_data <- cpi_data %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE)) 

all_films <- df %>%
  filter(!is.na(release_date)) %>%
  mutate(year = year(as.Date(release_date))) %>%
  filter(runtime != 0 & runtime > 40) %>%
  filter(year >= 1960 & year <= 2023)

total_films_per_year <- all_films %>%
  group_by(year) %>%
  summarise(total_films = n())

superhero_df <- df %>%
  filter(str_detect(keywords, "superhero|marvel|dc comics|comic book")) %>%
  filter(!is.na(release_date)) %>%
  mutate(year = year(as.Date(release_date)),
         decade = as.factor((year %/% 10) * 10),
         combinedRating = (averageRating + vote_average)/2) %>%
  filter(runtime != 0 & runtime > 40) %>%
  filter(year >= 1960 & year <= 2023) %>%
  filter(popularity < 2000) %>%
  mutate(rating_category = cut(combinedRating, 
                               breaks = c(0, 4, 7, 10), 
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE)) %>%
  left_join(cpi_data, by = "year")

base_year <- 2023
base_cpi <- cpi_data %>%
  filter(year == base_year) %>%
  pull(cpi) %>%
  as.numeric()

superhero_df <- superhero_df %>%
  mutate(
    budget_2023 = if_else(!is.na(budget) & !is.na(cpi),
                          (budget / cpi) * base_cpi, NA),
    revenue_2023 = if_else(!is.na(revenue) & !is.na(cpi),
                           (revenue / cpi) * base_cpi, NA),
    profit_2023 = revenue_2023 - budget_2023
  )

superhero_df <- superhero_df %>%
  mutate(
    lead_actor = str_extract(cast, "^[^,]+") 
  ) %>%
  arrange(lead_actor, release_date)

superhero_df <- superhero_df %>%
  group_by(lead_actor) %>%
  mutate(
    star_power_score = slide_dbl(profit_2023, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(star_power_score = if_else(is.na(star_power_score), median(profit_2023, na.rm = TRUE), star_power_score))

superhero_df <- superhero_df %>%
  mutate(
    lead_director = str_extract(directors, "^[^,]+") 
  ) %>%
  group_by(lead_director) %>%
  mutate(
    director_power_score = slide_dbl(profit_2023, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(director_power_score = if_else(is.na(director_power_score), median(profit_2023, na.rm = TRUE), director_power_score))

superhero_counts_per_year <- superhero_df %>%
  group_by(year) %>%
  summarise(superhero_count = n())

superhero_proportion_per_year <- superhero_counts_per_year %>%
  left_join(total_films_per_year, by = "year") %>%
  mutate(proportion = superhero_count / total_films)

action_films_per_year <- all_films %>%
  filter(str_detect(genres, "Action")) %>%
  group_by(year) %>%
  summarise(total_action = n())

superhero_action_counts <- superhero_df %>%
  filter(str_detect(genres, "Action")) %>%
  group_by(year) %>%
  summarise(superhero_action_count = n())

superhero_action_proportion <- superhero_action_counts %>%
  left_join(action_films_per_year, by = "year") %>%
  mutate(proportion = superhero_action_count / total_action)

#Visualising
superhero_df %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "The Rise of Superhero Cinema", x = "Year", y = "Number of Films") +
  theme_minimal()

ggplot(superhero_proportion_per_year, aes(x = year, y = proportion)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Proportion of Superhero Films by Year (Normalised)",
    x = "Year",
    y = "Proportion of Total Films"
  ) +
  theme_minimal()

ggplot(superhero_action_proportion, aes(x = year, y = proportion)) +
  geom_line(color = "#e63946", size = 1) +
  geom_point(color = "#1d3557") +
  geom_smooth(method = "loess", se = FALSE, color = "gray", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Superhero Films as a Proportion of Action Films (Normalised)",
    subtitle = "Percentage of action films that feature superhero themes",
    x = "Year of Release",
    y = "% of Action Movies"
  ) +
  theme_minimal()

ggplot(superhero_df, aes(x=decade, y= combinedRating))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(aes(color = rating_category), 
              alpha = 0.5, 
              width = 0.3) +
  labs(x = "Decade",
       y="Average Rating", 
       title = "Ratings of Superhero Films over Time",
       color = "Rating Category") +
  theme_minimal()

profit_per_year <- superhero_df %>%
  filter(profit_2023 != 0) %>%
  group_by(year) %>%
  summarise(
    avg_profit = mean(profit_2023, na.rm = TRUE),
    num_films = n()
  )

ggplot(profit_per_year, aes(x = year, y = avg_profit)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "gray", linetype = "dashed") +
  labs(
    title = "Average Profit per Superhero Film by Year",
    x = "Year",
    y = "Average Profit ($)"
  ) +
  theme_minimal()

#Random forest for predicting profit
set.seed(42)

superhero_rf <- superhero_df %>%
  filter(profit_2023 != 0,
         budget_2023 != 0) %>%
  mutate(
    is_franchise = if_else(str_detect(title, "[:0-9]| (II|III|IV|V|VI|VII|VIII|IX|X)($| )"), 1, 0),
    is_franchise = as.factor(is_franchise),
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
  dplyr::select(title, profit_2023, budget_2023, runtime, popularity, 
         is_franchise, original_language, numVotes,
         director_power_score, star_power_score, season) %>%
  drop_na()

trainIndex <- sample(1:nrow(superhero_rf), 0.8 * nrow(superhero_rf))
trainData <- superhero_rf[trainIndex, ]
testData <- superhero_rf[-trainIndex, ]
superhero_rf_model <- randomForest(profit_2023 ~ . - title, data = trainData)
print(superhero_rf_model)

predictions <- predict(superhero_rf_model, testData)
error <- predictions - testData$profit_2023

rmse <- sqrt(mean(error^2))
print(paste("Average error in dollars: ", rmse))

ggplot(data.frame(actual = testData$profit_2023, predicted = predictions), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual Profit vs. Predicted Profit",
       subtitle = "Dots closer to the red line are more accurate",
       x = "Actual Profit", y = "Predicted Profit") +
  theme_minimal()

importance(superhero_rf_model)
varImpPlot(superhero_rf_model)

superhero_rf_classified <- superhero_rf %>%
  mutate(profit_level = as.factor(if_else(profit_2023 > median(profit_2023), "High", "Low"))) %>%
  select(-profit_2023)
trainIndex <- sample(1:nrow(superhero_rf_classified), 0.8 * nrow(superhero_rf_classified))
trainData <- superhero_rf_classified[trainIndex, ]
testData <- superhero_rf_classified[-trainIndex, ]
superhero_classified_model <- randomForest(profit_level ~ . - title, data = trainData, proximity = TRUE)
print(superhero_classified_model)

predictions <- predict(superhero_classified_model, testData)
confusionMatrix(predictions, testData$profit_level)

importance(superhero_classified_model)
varImpPlot(superhero_classified_model)

MDSplot(superhero_classified_model, trainData$profit_level)
legend("topleft", 
       legend = levels(trainData$profit_level), 
       fill = c("red", "blue"))

rf_prox <- randomForest(profit_level ~ . - title, data = trainData, proximity = TRUE)$proximity
mds_coords <- cmdscale(1 - rf_prox, k = 2)
outlier_map <- trainData %>%
  mutate(Dim1 = mds_coords[,1], 
         Dim2 = mds_coords[,2])

plot_data <- outlier_map %>%
  mutate(
    is_anomaly = if_else(
      (profit_level == "High" & Dim1 < -0.1) | 
      (profit_level == "Low" & Dim1 > 0.3),
      as.character(title), 
      NA_character_
    )
  )
ggplot(plot_data, aes(x = Dim1, y = Dim2, color = profit_level)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_label_repel(
    aes(label = is_anomaly),
    color = "black",
    fill = "white",          
    fontface = "bold",
    size = 3,
    force = 10,             
    nudge_y = 0.05,          
    segment.color = "black",
    segment.size = 0.5,     
    segment.alpha = 0.8,    
    min.segment.length = 0   
  ) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  theme_minimal() +
  labs(title = "MDS Plot: Superhero Profit Outliers",
       x = "Dimension 1", y = "Dimension 2", 
       color = "Profit Level")

#Random forest model for predicting rating
set.seed(42)

rating_rf <- superhero_df %>%
  mutate(
    is_franchise = if_else(str_detect(title, "[:0-9]| (II|III|IV|V|VI|VII|VIII|IX|X)($| )"), 1, 0),
    is_franchise = as.factor(is_franchise),
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
  select(title, runtime, popularity, 
                is_franchise, original_language, season,
                combinedRating, numVotes) %>%
  drop_na()

trainIndex <- sample(1:nrow(rating_rf), 0.8 * nrow(rating_rf))
trainData <- rating_rf[trainIndex, ]
testData <- rating_rf[-trainIndex, ]
rating_rf_model <- randomForest(combinedRating ~ . - title, data = trainData)
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
rating_classified_model <- randomForest(rating_level ~ . - title, data = trainData, proximity = TRUE)
print(rating_classified_model)

predictions <- predict(rating_classified_model, testData)
confusionMatrix(predictions, testData$rating_level)

importance(rating_classified_model)
varImpPlot(rating_classified_model)

MDSplot(rating_classified_model, trainData$rating_level)
legend("topleft", 
       legend = levels(trainData$rating_level), 
       fill = c("red", "blue"))

features <- rating_rf %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ scale(.x) %>% as.vector))

wss <- sapply(1:10, function(k) {
  kmeans(features, centers = k, nstart = 10)$tot.withinss
})

fviz_nbclust(features, kmeans, method='silhouette')

k <- 2 
kmeans_model <- kmeans(features, centers = k, nstart = 10)

rating_rf_clusters <- rating_rf %>%
  mutate(
    cluster = as.factor(kmeans_model$cluster),
    rating_class = if_else(cluster == 1, "Low", "High")  
  )

ggplot(rating_rf_clusters, aes(x = rating_class, y = combinedRating, fill = rating_class)) +
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
  rating_class ~ . - title - combinedRating - cluster, 
  data = trainData,
  proximity = TRUE
)

print(rating_cluster_model)
predictions <- predict(rating_cluster_model, testData)
confusionMatrix(predictions, testData$rating_class)
varImpPlot(rating_cluster_model)

rf_prox <- randomForest(
  rating_class ~ . - title - combinedRating - cluster,
  data = trainData,
  proximity = TRUE
)$proximity

mds_coords <- cmdscale(1 - rf_prox, k = 2)

coords_map <- trainData %>%
  mutate(Dim1 = mds_coords[,1], Dim2 = mds_coords[,2])

plot_data <- coords_map %>%
  mutate(
    is_anomaly = if_else(
      (rating_class == "High" & Dim1 > 0.0) | 
        (rating_class == "Low" & Dim1 < -0.35),
      as.character(title), 
      NA_character_
    )
  )

ggplot(plot_data, aes(x = Dim1, y = Dim2, color = rating_class)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_label_repel(
    aes(label = is_anomaly),
    color = "black",
    fill = "white",          
    fontface = "bold",
    size = 3,
    force = 10,             
    nudge_y = 0.05,          
    segment.color = "black",
    segment.size = 0.5,     
    segment.alpha = 0.8,    
    min.segment.length = 0   
  ) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  theme_minimal() +
  labs(title = "MDS Plot: Rating Classes from Clustering", x = "Dimension 1", y = "Dimension 2",
       color = "Rating Class")

#Linear models 
model1 <- glm(profit_2023 ~ runtime + budget_2023 + popularity + star_power_score + 
                director_power_score + season + numVotes +
                original_language + is_franchise, data = superhero_rf)
summary(model1)

profit_long <- superhero_rf %>%
  select(profit_2023, budget_2023, star_power_score, numVotes) %>%
  pivot_longer(
    cols = c(budget_2023, star_power_score, numVotes),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "star_power_score" ~ "Star Average Career Profit",
    Metric == "budget_2023" ~ "Budget Adjusted for Inflation($)",
    Metric == "numVotes" ~ "Number of IMDb Votes (log10)"
  ))
ggplot(profit_long, aes(x = Value, y = profit_2023)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = "Profit Adjusted for Inflation ($)")

rating_rf$original_language <- relevel(as.factor(rating_rf$original_language), ref = "en")

model2 <- glm(combinedRating ~ runtime  + popularity
              + season + is_franchise + original_language + numVotes, data = rating_rf)
summary(model2)

rating_long <- rating_rf %>%
  select(combinedRating, popularity, numVotes, runtime) %>%
  pivot_longer(
    cols = c(popularity, numVotes, runtime),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "popularity" ~ "Popularity",
    Metric == "numVotes" ~ "Number of IMDb Votes (Log10)",
    Metric == "runtime" ~ "Runtime (mins)"
  ))

ggplot(rating_long, aes(x = Value, y = combinedRating)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = "Average Rating")

ggplot(rating_rf, aes(x = as.factor(is_franchise), y = combinedRating, color = as.factor(is_franchise))) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Franchise vs. Non-Franchise Ratings", x = "Is Franchise (0=No, 1=Yes)",
       color = "Is Franchise?")

model2_df <- as.data.frame(summary(model2)$coefficients)
colnames(model2_df) <- c("Estimate", "StdError", "tValue", "PValue")

language_map <- c(
  "hu" = "Hungarian",
  "ja" = "Japanese",
  "ml" = "Malayalam",
  "ms" = "Malay",
  "es" = "Spanish",
  "ur" = "Urdu"
)

sig_languages <- model2_df[grepl("original_language", rownames(model2_df)) & model2_df$PValue < 0.05, ]
clean_names <- gsub("original_language", "", rownames(sig_languages))
rownames(sig_languages) <- ifelse(clean_names %in% names(language_map), 
                                  language_map[clean_names], 
                                  clean_names)

sig_languages %>%
  rownames_to_column("Language") %>%
  ggplot(aes(x = reorder(Language, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Significant Language Effects on Rating", x = "Language", y = "Coefficient Estimate")

lang_df <- rating_rf %>%
  filter(original_language %in% c("hu", "ja", "ml", "ur", "lb", "fr", "en")) %>%
  group_by(original_language) %>%
  filter(n() > 10) %>%
  ungroup() %>%
  mutate(original_language = recode(original_language,
                                      "fr" = "french", 
                                    "en" = "english",
                                    "ja" = "japanese"))

ggplot(lang_df, aes(x = original_language, y = combinedRating, colour = original_language)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7, width = 0.3) +
  labs(title = "Significant Language Effects on Rating", x ="Language",
       y = "Average Rating", color = "Language") +
  theme_minimal()
