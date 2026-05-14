#The evolution of genres over time
#Data : TMDb + IMDb via Kagglehub

library(tidyverse)
library(stringr)
library(lubridate)
library(randomForest)
library(caret)
library(ggrepel)
library(slider)
setwd("Documents/programming/tmdb_proj_0526")
rm(list = ls())

df <- read_csv("TMDB  IMDB Movies Dataset.csv")

##Superheros

#Filtering
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
         combinedRating = (averageRating + vote_average)/2,
         profit = revenue - budget) %>%
  filter(runtime != 0 & runtime > 40) %>%
  filter(year >= 1960 & year <= 2023) %>%
  filter(popularity < 2000) %>%
  mutate(rating_category = cut(combinedRating, 
                               breaks = c(0, 4, 7, 10), 
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE))

superhero_df <- superhero_df %>%
  mutate(
    lead_actor = str_extract(cast, "^[^,]+") 
  ) %>%
  arrange(lead_actor, release_date)

superhero_df <- superhero_df %>%
  group_by(lead_actor) %>%
  mutate(
    star_power_score = slide_dbl(profit, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(star_power_score = if_else(is.na(star_power_score), median(profit, na.rm = TRUE), star_power_score))

superhero_df <- superhero_df %>%
  mutate(
    lead_director = str_extract(directors, "^[^,]+") 
  ) %>%
  group_by(lead_director) %>%
  mutate(
    director_power_score = slide_dbl(profit, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(director_power_score = if_else(is.na(director_power_score), median(profit, na.rm = TRUE), director_power_score))

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
  filter(profit != 0) %>%
  group_by(year) %>%
  summarise(
    avg_profit = mean(profit, na.rm = TRUE),
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
  filter(profit != 0,
         budget != 0) %>%
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
    ),season = as.factor(season)) %>%
  dplyr::select(title, profit, budget, runtime, popularity, 
         is_franchise, original_language, 
         director_power_score, star_power_score, release_month, season) %>%
  drop_na()

trainIndex <- sample(1:nrow(superhero_rf), 0.8 * nrow(superhero_rf))
trainData <- superhero_rf[trainIndex, ]
testData <- superhero_rf[-trainIndex, ]
superhero_rf_model <- randomForest(profit ~ . - title, data = trainData)
print(superhero_rf_model)

predictions <- predict(superhero_rf_model, testData)
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

importance(superhero_rf_model)
varImpPlot(superhero_rf_model)

superhero_rf_classified <- superhero_rf %>%
  mutate(profit_level = as.factor(if_else(profit > median(profit), "High", "Low"))) %>%
  select(-profit)
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
legend("topright", 
       legend = levels(trainData$profit_level), 
       fill = c("red", "blue"))

rf_prox <- randomForest(profit_level ~ ., data = trainData, proximity = TRUE)$proximity
mds_coords <- cmdscale(1 - rf_prox, k = 2)
outlier_map <- trainData %>%
  mutate(Dim1 = mds_coords[,1], 
         Dim2 = mds_coords[,2])
misplaced_movies <- outlier_map %>%
  filter(
    (profit_level == "High" & Dim1 > 0.2) |
      (profit_level == "Low" & Dim1 < -0.3)
  )
print(misplaced_movies %>% select(title, profit_level, budget, popularity, Dim1))

plot_data <- outlier_map %>%
  mutate(
    is_anomaly = if_else(
      (profit_level == "High" & Dim1 > 0.2) | 
      (profit_level == "Low" & Dim1 < -0.3),
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
       subtitle = "Labeled points represent 'misplaced' movies (High-profit stats with Low-profit results, and vice versa)",
       x = "Dimension 1", y = "Dimension 2", 
       color = "Profit Level")

#Linear models 
profit_model <- superhero_df %>%
  filter(profit != 0,
         budget != 0)
model1 <- glm(profit ~ combinedRating + runtime + budget + popularity + star_power_score + director_power_score, data = profit_model)
summary(model1)

profit_long <- profit_model %>%
  select(profit, combinedRating, budget, star_power_score) %>%
  pivot_longer(
    cols = c(combinedRating, budget, star_power_score),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "combinedRating" ~ "Combined Average Rating",
    Metric == "star_power_score" ~ "Star Average Career Profit",
    Metric == "budget" ~ "Budget ($)"
  ))
ggplot(profit_long, aes(x = Value, y = profit)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = "Profit ($)")

model2 <- glm(combinedRating ~ runtime + budget + star_power_score + director_power_score + popularity +decade, data = superhero_df)
summary(model2)

ggplot(superhero_df, aes(x = popularity, y = combinedRating, colour = decade)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ decade) +
  theme_bw() +
  labs(title = "Popularity Effects on Rating in Each Decade",
       x ="Popularity",
       y = "Combined Average Rating")

ggplot(superhero_df, aes(x = runtime, y = combinedRating, colour = decade)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ decade) +
  theme_bw() +
  labs(title = "Runtime Effects on Rating in Each Decade",
       x ="Runtime",
       y = "Combined Average Rating")

model3 <- glm(budget ~ runtime + star_power_score + director_power_score + popularity, data = profit_model)
summary(model3)

budget_long <- profit_model %>%
  select(budget, runtime, star_power_score, director_power_score, popularity) %>%
  pivot_longer(
    cols = c(runtime, star_power_score, director_power_score, popularity),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "runtime" ~ "Runtime (mins)",
    Metric == "star_power_score" ~ "Star Average Career Profit",
    Metric == "director_power_score" ~ "Director Average Career Profit",
    Metric == "popularity" ~ "Popularity"
  ))
ggplot(budget_long, aes(x = Value, y = budget)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = "Budget")
