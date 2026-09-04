#The evolution of genres over time
#Data : TMDb + IMDb via Kagglehub

library(tidyverse)
library(slider)
library(factoextra)
library(FactoMineR)
library(geomtextpath)
library(broom)
library(quantmod)
library(patchwork)
library(DHARMa)
library(mgcv)
library(mclust)
library(GGally)
setwd("Documents/programming/tmdb_proj_0526")
rm(list = ls())

remakes <- read_csv("remakes_dataset.csv")
remakes <- remakes %>%
  group_by(pair_group_id) %>%
  filter(n() > 1) %>%
  ungroup()

##Remakes vs. Originals
getSymbols("CPIAUCSL", src = "FRED")
cpi_data <- as.data.frame(CPIAUCSL)
colnames(cpi_data) <- "cpi"

cpi_data <- cpi_data %>%
  tibble::rownames_to_column("date") %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date)
  ) %>%
  dplyr::select(year, cpi)

cpi_data <- cpi_data %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE))

remakes <- remakes %>%
  mutate(decade = as.factor((year %/% 10) * 10),
         combinedRating = (averageRating + vote_average)/2) %>%
  mutate(
    rating_category = ifelse(combinedRating >= median(combinedRating), "High", "Low"),
    rating_category = factor(rating_category, levels = c("Low", "High"))
  ) %>%
  left_join(cpi_data, by = "year")

base_year <- 2024
base_cpi <- cpi_data %>%
  filter(year == base_year) %>%
  pull(cpi) %>%
  as.numeric()

remakes <- remakes %>%
  mutate(
    budget_2024 = if_else(!is.na(budget) & !is.na(cpi),
                          (budget / cpi) * base_cpi, NA),
    revenue_2024 = if_else(!is.na(revenue) & !is.na(cpi),
                           (revenue / cpi) * base_cpi, NA),
    profit_2024 = revenue_2024 - budget_2024
  )

remakes <- remakes %>%
  mutate(lead_actor = str_extract(cast, "^[^,]+")) %>%
  arrange(lead_actor, release_date)

remakes <- remakes %>%
  filter(!is.na(lead_actor)) %>%
  group_by(lead_actor) %>%
  mutate(
    star_power_score = slide_dbl(profit_2024, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  bind_rows(filter(remakes, is.na(lead_actor))) %>%
  mutate(star_power_score = if_else(is.na(star_power_score), median(profit_2024, na.rm = TRUE), star_power_score))

remakes <- remakes %>%
  mutate(lead_director = str_extract(directors, "^[^,]+"))

remakes <- remakes %>%
  filter(!is.na(lead_director)) %>%
  group_by(lead_director) %>%
  mutate(
    director_power_score = slide_dbl(profit_2024, mean, .before = 5, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  bind_rows(filter(remakes, is.na(lead_director))) %>%
  mutate(director_power_score = if_else(is.na(director_power_score), median(profit_2024, na.rm = TRUE), director_power_score))

final_remakes <- remakes %>%
  group_by(pair_group_id) %>%
  mutate(
    orig_year = min(year),
    orig_rating = combinedRating[year == orig_year][1]
  ) %>%
  mutate(type = ifelse(year == min(year), "Original", "Remake")) %>%
  mutate(
    avg_remake_rating = mean(combinedRating[type == "Remake"], na.rm = TRUE),
    group_performance = ifelse(avg_remake_rating > orig_rating, "Remake is Better", "Remake is Worse")
  ) %>%
  ungroup() %>%
  arrange(pair_group_id, year)

final_remakes <- final_remakes %>%
  arrange(pair_group_id, year) %>%
  group_by(pair_group_id) %>%
  mutate(
    remake_number = row_number(),
    previous_remakes = remake_number - 1
  ) %>%
  ungroup()

remakes_only <- final_remakes %>%
  filter(type == "Remake")

ggplot(remakes_only, aes(x = avg_remake_rating, y = orig_rating)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  theme_basic()+
  labs(x = "Average Remake Rating", y = "Original Rating")

remakes_only <- remakes_only %>%
  mutate(rating_diff = avg_remake_rating - orig_rating)

ggplot(remakes_only, aes(x = rating_diff, y = orig_rating)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  theme_basic()+
  labs(x = "Difference in Rating between Average Remake and Original Rating",
       y = "Original Rating")

model1 <- glm(rating_diff ~ orig_rating + avg_remake_rating, data = remakes_only)
summary(model1)

ggplot(remakes_only, aes(x = rating_diff, y = avg_remake_rating)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  theme_basic()+
  labs(x = "Difference in Rating between Original and Average Remake Rating",
       y = "Average Remake Rating")

remakes_only <- remakes_only %>%
  mutate(year_diff = year - orig_year)

ggplot(remakes_only, aes(x = year_diff, y = combinedRating)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  theme_basic()+
  labs(x = "Number of Years Between Original and Remake",
       y = "Rating of Remake")

model2 <- glm(combinedRating ~ year_diff, data = remakes_only)
summary(model2)

ggplot(remakes_only, aes(x = year_diff)) +
  geom_histogram()+
  geom_labelvline(
    label = round(mean(remakes_only$year_diff), 2),
    xintercept = mean(remakes_only$year_diff),
    color = "forestgreen",
    angle=45
  ) +
  theme_basic()

ggplot(remakes_only, aes(x = year_diff, y = combinedRating)) +
  geom_point(alpha = 0.2, color = "grey") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = TRUE) +
  labs(
    x = "Years Difference (year_diff)",
    y = "Remake Rating (combinedRating)"
  ) +
  theme_basic()

top_remakes <- remakes_only %>%
  arrange(desc(combinedRating)) %>%
  distinct(pair_group_id, .keep_all = TRUE) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")"))

ggplot(top_remakes, aes(x = title_with_year)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes vs. their Original Rating"
  )

max_remakes_df <- remakes_only %>%
  group_by(pair_group_id) %>%
  summarise(highest_remake_rating = max(combinedRating, na.rm = TRUE), .groups = "drop")

top_originals <- final_remakes %>%
  filter(type == "Original") %>%
  left_join(max_remakes_df, by = "pair_group_id") %>%
  arrange(desc(combinedRating)) %>%
  distinct(pair_group_id, .keep_all = TRUE) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")"))

ggplot(top_originals, aes(x = title_with_year)) +
  geom_col(aes(y = combinedRating, fill = "Original Rating")) +
  geom_line(aes(y = highest_remake_rating, group = 1, color = "Highest Remake Rating"), size = 1.2) +
  geom_point(aes(y = highest_remake_rating, color = "Highest Remake Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Original Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Highest Remake Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Originals vs. Their Highest Rated Remake"
  )

remakes_only <- remakes_only %>%
  mutate(era_category = case_when(
    orig_year < 1960 ~ "Pre-1960",
    orig_year >= 1960 & orig_year < 1995 ~ "1960-1994",
    orig_year >= 1995 ~ "1995+"
  )) %>%
  mutate(era_category = factor(era_category, levels = c(
    "Pre-1960", "1960-1994", "1995+"
  ))) %>%
  mutate(logPop = log1p(popularity),
         logNumVotes = log1p(numVotes))

set.seed(42)
pca_data <- remakes_only %>%
  dplyr::select(runtime, logPop, logNumVotes, orig_rating, orig_year, year_diff, remake_number) %>%
  drop_na()

pca_data <- scale(pca_data)
head(pca_data)

pca_result_fm <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(pca_result_fm,
                geom.ind = "point",
                col.var = "blue",
                col.ind = remakes_only$rating_category,
                palette = c("#00AFBB", "#E7B800"),
                addEllipses = TRUE,
                ellipse.level = 0.95)

pca_data_eras <- remakes_only %>%
  dplyr::select(runtime, logPop, logNumVotes, orig_rating, year_diff) %>%
  drop_na()

pca_data_eras <- scale(pca_data_eras)
head(pca_data_eras)

pca_result_eras <- PCA(pca_data_eras, scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(pca_result_eras,
                geom.ind = "point",
                col.var = "blue",
                col.ind = remakes_only$era_category,
                palette = c("#00AFBB", "#E7B800", "tomato"),
                addEllipses = TRUE,
                ellipse.level = 0.95)

fviz_eig(pca_result_fm, addlabels = TRUE)
pca_res <- prcomp(pca_data)

gmm <- Mclust(pca_res$x[, 1:5])
summary(gmm)
clusters_gmm <- gmm$classification

meta <- remakes_only %>%
  dplyr::select(era_category, title, year_diff, logPop,logNumVotes, runtime, orig_rating,
                remake_number)

coords <- pca_result_fm$ind$coord %>%
  as.data.frame() %>%
  mutate(row_id = row_number())

coords2 <- bind_cols(coords, meta)

loadings <- as.data.frame(pca_result_fm$var$coord[, 1:2])
loadings$var <- rownames(loadings)

arrow_scale <- 3

coords2 %>%
  mutate(cluster = clusters_gmm) %>%
  ggplot(aes(Dim.1, Dim.2, colour = factor(cluster), label = title)) +
  geom_point(size = 2) +
  geom_segment(
    data = loadings,
    inherit.aes = FALSE,
    aes(x = 0, y = 0,
        xend = Dim.1 * arrow_scale,
        yend = Dim.2 * arrow_scale),
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black"
  ) +
  geom_text(data = loadings,
            aes(x = Dim.1 * arrow_scale * 1.1,
                y = Dim.2 * arrow_scale * 1.1,
                label = var),
            colour = "black",
            size = 4) +
  stat_ellipse(type = "norm", level = 0.8) +
  theme_basic() +
  labs(colour = "Cluster")

cluster_input <- remakes_only %>%
  dplyr::select(runtime, logPop, logNumVotes,
                orig_rating, year_diff, remake_number)

rows_used <- complete.cases(cluster_input)

cluster_data <- remakes_only[rows_used, ]
cluster_data$cluster <- clusters_gmm

remakes_with_clusters <- remakes_only %>%
  mutate(row_id = row_number()) %>%
  left_join(
    cluster_data %>%
      mutate(row_id = row_number()) %>%
      dplyr::select(row_id, cluster),
    by = "row_id"
  ) %>%
  mutate(cluster = as.factor(cluster))

remakes_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(c(logPop, logNumVotes, year_diff, orig_rating, runtime, remake_number), mean), n())

ggplot(remakes_with_clusters, aes(x = cluster, y = year_diff, colour = cluster))+
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic()

ggplot(remakes_with_clusters, aes(x = cluster, y = orig_rating, colour = cluster))+
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic()

ggplot(remakes_with_clusters, aes(x = cluster, y = logPop, colour = cluster))+
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic()

ggplot(remakes_with_clusters, aes(x = cluster, y = logNumVotes, colour = cluster))+
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic()

ggplot(remakes_with_clusters, aes(x = cluster, y = runtime , colour = cluster))+
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic()


remakes_with_clusters %>%
  dplyr::count(cluster, era_category) %>%
  ggplot(aes(x = cluster, y = n, fill = era_category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(y = "Proportion", fill = "Era")

remakes_with_clusters %>%
  dplyr::select(cluster, logPop, logNumVotes, year_diff, orig_rating, runtime) %>%
  mutate(cluster = factor(cluster)) %>%
  GGally::ggparcoord(
    columns = 2:6,
    groupColumn = 1,
    scale = "std",
    alphaLines = 0.4
  ) +
  theme_basic()

remakes_with_clusters <- remakes_with_clusters %>%
  mutate(cluster_label = case_when(
    cluster == 1 ~ "mainstream remakes of high rated originals",
    cluster == 2 ~ "mid quality, mid popularity",
    cluster == 3 ~ "high rated originals, very short gap",
    cluster == 4 ~ "very long, multiple remakes of high rated originals",
    cluster == 5 ~ "obscure remakes, short gap",
    cluster == 6 ~ "long remakes with long gaps",
    cluster == 7 ~ "modern remakes of very old originals",
    cluster == 8 ~ "multiple remakes of old, low rated originals",
    cluster == 9 ~ "obscure, short remakes",
    TRUE ~ "Unknown"
  ))


remakes_with_clusters %>%
  dplyr::select(cluster, logPop, logNumVotes, year_diff, orig_rating, runtime) %>%
  mutate(cluster = factor(cluster)) %>%
  ggpairs(aes(colour = cluster, alpha = 0.6))

cluster_means <- remakes_with_clusters %>%
  group_by(cluster, cluster_label) %>%
  summarise(across(c(logPop, logNumVotes, year_diff, orig_rating, runtime, remake_number), mean)) %>%
  ungroup()

scaled_means <- cluster_means %>%
  mutate(across(c(logPop, logNumVotes, year_diff, orig_rating, runtime, remake_number), scale))

plot_data <- scaled_means %>%
  pivot_longer(
    cols = c(logPop, logNumVotes, year_diff, orig_rating, runtime, remake_number),
    names_to = "variable",
    values_to = "value"
  )

ggplot(plot_data, aes(variable, value, fill = cluster_label)) +
  geom_col(position = "dodge") +
  theme_basic() +
  labs(fill = "Cluster label", x = "Variable", y = "Scaled mean")

## cluster 1
cluster_one <- remakes_with_clusters %>%
  filter(cluster == 1) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")")) %>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 30))

ggplot(cluster_one, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster One",
    subtitle = "Cluster one: mainstream remakes of high rated originals"
  )

## cluster 2
cluster_two <- remakes_with_clusters %>%
  filter(cluster == 2) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")"))%>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_two, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Two",
    subtitle = "Cluster two: mid quality, mid popularity"
  )

## cluster 3
cluster_three <- remakes_with_clusters %>%
  filter(cluster == 3) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")")) %>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_three, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Three",
    subtitle = "Cluster three: high rated originals, very short gap"
  )

## cluster 4
cluster_four <- remakes_with_clusters %>%
  filter(cluster == 4) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")"))
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_four, aes(x = title_with_year)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Four",
    subtitle = "Cluster four: very long, multiple remakes of high rated originals"
  )

## cluster 5
cluster_five <- remakes_with_clusters %>%
  filter(cluster == 5) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")")) %>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 15))

ggplot(cluster_five, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Five",
    subtitle = "Cluster five: obscure remakes, short gap"
  )

## cluster 6
cluster_six <- remakes_with_clusters %>%
  filter(cluster == 6) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")")) %>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_six, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Six",
    subtitle = "Cluster six: long remakes with moderate gaps"
  )

## cluster 7
cluster_seven <- remakes_with_clusters %>%
  filter(cluster == 7) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")")) %>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_seven, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Seven",
    subtitle = "Cluster seven: short remakes of very old originals"
  )

## cluster 8
cluster_eight <- remakes_with_clusters %>%
  filter(cluster == 8) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")"))
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_eight, aes(x = title_with_year)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Eight",
    subtitle = "Cluster eight: multiple remakes of old, low rated originals"
  )

## cluster 9
cluster_nine <- remakes_with_clusters %>%
  filter(cluster == 9) %>%
  arrange(desc(combinedRating)) %>%
  slice_max(order_by = combinedRating, n = 10) %>%
  mutate(title_with_year = paste0(title, " (", year, ")")) %>%
  mutate(title_wrapped = stringr::str_wrap(title_with_year, width = 17))

ggplot(cluster_nine, aes(x = title_wrapped)) +
  geom_col(aes(y = combinedRating, fill = "Remake Rating")) +
  geom_line(aes(y = orig_rating, group = 1, color = "Original Rating"), linewidth = 1.2) +
  geom_point(aes(y = orig_rating, color = "Original Rating"), size = 3) +
  scale_fill_manual(name = "", values = c("Remake Rating" = "steelblue")) +
  scale_color_manual(name = "", values = c("Original Rating" = "darkorange")) +
  theme_basic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  labs(
    x = "Film (Year)",
    y = "Average Rating",
    title = "Highest Rated Remakes in Cluster Nine",
    subtitle = "Cluster nine: old, short remakes"
  )

## with financial data
financial_data <- remakes_only %>%
  filter(profit_2024 != 0,
         budget_2024 != 0) %>%
  arrange(pair_group_id, year) %>%
  mutate(profit_category = ifelse(profit_2024 >= median(profit_2024), "High", "Low"))

pca_data <- financial_data %>%
  dplyr::select(runtime, logPop, logNumVotes, orig_rating, orig_year, year_diff,
                remake_number,budget_2024) %>%
  drop_na()

pca_data <- scale(pca_data)
head(pca_data)

pca_result_fm <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(pca_result_fm,
                geom.ind = "point",
                col.var = "blue",
                col.ind = financial_data$rating_category,
                palette = c("#00AFBB", "#E7B800"),
                addEllipses = TRUE,
                ellipse.level = 0.95)

pca_data_eras <- financial_data %>%
  dplyr::select(runtime, logPop, logNumVotes, orig_rating, year_diff,
                remake_number, budget_2024) %>%
  drop_na()

pca_data_eras <- scale(pca_data_eras)
head(pca_data_eras)

pca_result_eras <- PCA(pca_data_eras, scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(pca_result_eras,
                geom.ind = "point",
                col.var = "blue",
                col.ind = financial_data$era_category,
                palette = c("#00AFBB", "#E7B800", "tomato"),
                addEllipses = TRUE,
                ellipse.level = 0.95)

pca_data_profit <- financial_data %>%
  dplyr::select(runtime, logPop, logNumVotes, orig_rating, year_diff,
                remake_number, budget_2024, orig_year) %>%
  drop_na()

pca_data_profit <- scale(pca_data_profit)
head(pca_data_profit)

pca_result_profit <- PCA(pca_data_profit, scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(pca_result_profit,
                geom.ind = "point",
                col.var = "blue",
                col.ind = financial_data$profit_category,
                palette = c("#00AFBB", "#E7B800", "tomato"),
                addEllipses = TRUE,
                ellipse.level = 0.95)

fviz_eig(pca_result_profit, addlabels = TRUE)
pca_res <- prcomp(pca_data_profit)

## linear models
remakes_only <- remakes_only %>%
  mutate(
    top_genre = str_extract(genres, "[A-Za-z\\-]+"),
    top_genre = as.factor(top_genre)
  ) %>%
  filter(logPop < 4) %>%
  mutate(remake_number = as.factor(remake_number))

mfull <- glm(combinedRating ~ (runtime + logPop + logNumVotes + orig_year +
               orig_rating + year_diff + top_genre + remake_number)^2, data = remakes_only)
summary(mfull)
mred1 <- step(mfull)
mred1 %>% drop1(test = "LRT") %>%
  as_tibble(rownames = "effect") %>%
  arrange(AIC)
mred2 <- update(mred1, ~.- logPop:orig_rating - logPop:remake_number - logPop:year_diff - logNumVotes:remake_number - runtime:orig_rating)
mred2 %>% drop1(test = "LRT") %>%
  as_tibble(rownames = "effect") %>%
  arrange(AIC)
mred3 <- update(mred2, ~.-orig_rating)
mred3 %>% drop1(test = "LRT") %>%
  as_tibble(rownames = "effect") %>%
  arrange(AIC)
anova(mred3)
1-deviance(mred1)/mred1$null.deviance
plot(simulateResiduals(mred1))
1-deviance(mred2)/mred2$null.deviance
plot(simulateResiduals(mred2))

significant_terms <- tidy(mred3) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

print(significant_terms, n = 22)

genre_data <- remakes_only %>%
  filter(top_genre %in% c("TV", "Horror", "Music", "Science"))

ggplot(genre_data, aes(x = top_genre, y = combinedRating, colour = top_genre))+
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic()+
  labs(x = "Top Genre of Remake",
       y = "Rating of Remake",
       color = "Top Genre")

genre_pop <- remakes_only %>%
  filter(top_genre %in% c("Horror", "Music", "TV", "Science"))

ggplot(genre_pop, aes(x = logPop, y = combinedRating, colour = top_genre)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_basic()+
  labs(x = "Popularity (log1p)",
       y = "Rating of Remake")

ggplot(remakes_only, aes(x = runtime, y = combinedRating, colour = era_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_basic() +
  labs(x = "Runtime",
       y = "Rating of Remake",
       colour = "Original Film Year Category")

ggplot(remakes_only, aes(x = logNumVotes, y = combinedRating, colour = era_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_basic() +
  labs(x = "Log of Number of Voters",
       y = "Rating of Remake",
       colour = "Original Film Year Category")

ggplot(remakes_only, aes(x = logPop, y = combinedRating, colour = era_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_basic() +
  labs(x = "Popularity (log1p)",
       y = "Remake Rating",
       colour = "Original Film Year Category")

binary_model_data <- remakes_only %>%
  mutate(binary_performance = ifelse(group_performance == "Remake is Better", 1, 0))

mfull <- glm(binary_performance ~ runtime + logPop + logNumVotes + orig_year +
                                     orig_rating + year_diff + top_genre,
             data = binary_model_data,
             family = "binomial")

mred1 <- step(mfull)
mred1 %>% drop1(test = "LRT") %>%
  as_tibble(rownames = "effect") %>%
  arrange(AIC)
anova(mred1)
1-deviance(mred1)/mred1$null.deviance
plot(simulateResiduals(mred1))

significant_terms <- tidy(mred1) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

print(significant_terms)

ggplot(binary_model_data, aes(x = orig_rating, y = binary_performance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE, colour = "forestgreen") +
  theme_basic()+
  labs(x = "Rating of Original Film",
       y = "Performance of Remake (1 = better, 0 = worse)")

ggplot(binary_model_data, aes(x = orig_year, y = binary_performance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE, colour = "forestgreen") +
  theme_basic()+
  labs(x = "Year of Original Film",
       y = "Performance of Remake (1 = better, 0 = worse)")

ggplot(binary_model_data, aes(x = year_diff, y = binary_performance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE, colour = "forestgreen") +
  theme_basic()+
  labs(x = "Year diff",
       y = "Performance of Remake (1 = better, 0 = worse)")

genre_binary <- binary_model_data %>%
  filter(top_genre %in% c("Animation", "Drama"))

ggplot(genre_binary, aes(x = top_genre, fill = as.factor(binary_performance))) +
  geom_bar(position = "fill") +
  theme_basic() +
  labs(x = "Top Genre", fill = "Performance of Remake (1 = better, 0 = worse)")

ggplot(binary_model_data, aes(x = logNumVotes, y = binary_performance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE, colour = "forestgreen") +
  theme_basic()+
  labs(x = "Number of Votes (log1p)",
       y = "Performance of Remake (1 = better, 0 = worse)")

ggplot(binary_model_data, aes(x = runtime, y = binary_performance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE, colour = "forestgreen") +
  theme_basic()+
  labs(x = "Runtime",
       y = "Performance of Remake (1 = better, 0 = worse)")

## Hypothesis testing
remakes_per_year <- remakes_only %>%
  group_by(year) %>%
  summarise(remake_count = n())

all_films <- read_csv("TMDB  IMDB Movies Dataset.csv")
all_films <- all_films %>%
  filter(!is.na(release_date)) %>%
  mutate(year = year(as.Date(release_date))) %>%
  filter(runtime > 40)

total_per_year <- all_films %>%
  group_by(year) %>%
  summarise(total_count = n())

normalized_data <- total_per_year %>%
  left_join(remakes_per_year, by = "year") %>%
  mutate(remake_count = replace_na(remake_count, 0)) %>%
  mutate(remake_proportion = remake_count / total_count)

remakes_only <- remakes_only %>%
  left_join(normalized_data, by = "year")

normalized_data %>%
  filter(year >= 1924,
         year <= 2024) %>%
  ggplot(aes(x = year, y = remake_proportion)) +
  geom_point() +
  geom_line() +
  theme_basic() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Normalised Remake Trend (Last 100 Years)",
       x = "Year", y = "Proportion of Movies that are Remakes")

normalized_data %>%
  filter(year >= 2000,
         year <= 2024) %>%
  ggplot(aes(x = year, y = remake_proportion)) +
  geom_point() +
  geom_line() +
  theme_basic() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Normalised Remake Trend (2000-2024)",
       x = "Year", y = "Proportion of Movies that are Remakes")

recent_trend <- normalized_data %>%
  filter(year >= 2000,
         year <= 2024)

hundred_years <- normalized_data %>%
  filter(year >= 1924,
         year <= 2024)

model <- lm(remake_proportion ~ year, data = recent_trend)
summary(model)

model2 <- lm(remake_proportion ~ year, data = hundred_years)
summary(model2)

recent_remakes <- remakes_only %>%
  filter(year >= 2000,
         year <= 2024) %>%
  mutate(era = ifelse(year < 2011, "pre-2011", "post-2011"))

model3 <- glm(combinedRating ~ era, data = recent_remakes)
summary(model3)

ggplot(recent_remakes, aes(x = era, y = combinedRating, colour = era)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic() +
  labs(x = "Time Period", y = "Rating of Remake", colour = "Time Period")

model4 <- glm(rating_diff ~ era, data = recent_remakes)
summary(model4)

model5 <- glm(logNumVotes ~ era, data = recent_remakes)
summary(model5)

profit_model_data <- recent_remakes %>%
  filter(profit_2024 != 0)

model6 <- glm(profit_2024 ~ era, data = profit_model_data)
summary(model6)

model7 <- glm(year_diff ~ era, data = recent_remakes)
summary(model7)

manova_data <- recent_remakes %>%
  mutate(era = as.factor(era))

manova_model <- manova(
  cbind(combinedRating, year_diff, logNumVotes, logPop, orig_year, orig_rating, runtime) ~ era,
  data = manova_data
)
summary(manova_model)
summary.aov(manova_model)

ggplot(recent_remakes, aes(x = era, y = orig_year, colour = era)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.7) +
  theme_basic() +
  labs(x = "Time Period", y = "Year of Original Film", colour = "Time Period")

all_decades <- sort(unique(manova_data$orig_decade))
era_means <- manova_data %>%
  group_by(era) %>%
  summarise(median_orig_year = median(orig_year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    base_decade = floor(median_orig_year / 10) * 10,
    bar_index = match(base_decade, all_decades),
    remainder = (median_orig_year %% 10) / 10,
    x_position = bar_index + remainder
  )

ggplot(manova_data, aes(x = factor(orig_decade), fill = era)) +
  geom_bar(position = "dodge") +
  geom_vline(data = era_means, aes(xintercept = x_position),
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = era_means, aes(x = x_position, y = Inf,
                                  label = paste(round(median_orig_year, 0))),
            color = "red", angle = 90, vjust = -0.5, hjust = 1.5, fontface = "bold") +
  scale_fill_manual(values = c("pre-2011" = "steelblue", "post-2011" = "darkorange")) +
  facet_wrap(~era, ncol = 1) +
  labs(
    x = "Decade of the Original Film",
    y = "Number of Remakes",
    fill = "Era"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
