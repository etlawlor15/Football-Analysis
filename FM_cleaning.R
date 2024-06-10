library(tidyverse)
library(tidymodels)
library(janitor)
library(lubridate)
library(corrr)
library(ggforce)
library(bestNormalize)
library(learntidymodels)
library(embed)

fm_player_data_raw <- read_csv("Data/fm_player_data.csv", 
                           col_types = cols(...1 = col_skip())) 

#Clean column names
fm_player_data <- fm_player_data_raw %>% 
  clean_names() %>% 
  rename(dob = do_b)

#Clean dob and find age
fm_player_data <- fm_player_data %>%
  mutate(dob = mdy(str_replace_all(dob, pattern = " \\s*\\([^\\)]+\\)", replacement = ""))) %>% 
  mutate(age = floor(as.numeric(as.duration(today()-dob), "years"))) %>% 
  select(player_id, name, dob, age, everything())

#Lengthen positions and widen sides
fm_player_data <- fm_player_data %>%
  separate_longer_delim(position, delim = ", ") %>% 
  separate_wider_delim(position, delim = " ", names = c("position", "side"), too_few = "align_start") %>%
  mutate(side = str_remove_all(string = side, pattern = "[()]"))

#Clean height and weight
fm_player_data$weight <- as.numeric(str_replace_all(fm_player_data$weight, pattern = "lbs", replacement = ""))
fm_player_data$height <- str_extract_all(fm_player_data$height, "\\d+", simplify = TRUE)

fm_player_data <- fm_player_data %>%
  mutate(height_in = 12*as.numeric(height[,1]) + as.numeric(height[,2])) %>% 
  select(-height)
  

#Clean transfer value
fm_player_data <- fm_player_data %>% 
  mutate(transfer_ranges = case_when(
    transfer_value == "Not for Sale" ~ str_replace_all(transfer_value, "Not for Sale", NA_character_),
    str_detect(transfer_value, "$") == TRUE ~ str_extract_all(transfer_value, "\\d+", simplify = TRUE)
    TRUE ~ transfer_value
    )
  )

hold <- separate_wider_delim(transfer_value, delim = " - ", names = c("transfer_value_min", "transfer_value_max"), too_few = "align_start")


fm_corr <- fm_player_data %>%
  select(-player_id) %>%
  correlate() %>% 
  rearrange() %>%
  shave() 

fm_corr %>%
  rplot(print_cor=TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

df_split <- initial_split(fm_player_data, strata = potential_ability, prop = 3/4)

df_train <- training(df_split)
df_test  <- testing(df_split)

set.seed(1702)
df_val <- validation_split(df_train, strata = potential_ability, prop = 4/5)
df_val$splits[[1]]

df_rec <-
  recipe(potential_ability ~ ., data = analysis(df_val$splits[[1]])) %>%
  step_rm(c("player_id", "name", "dob", "contract_start", "contract_end", "transfer_value", "all_time_apps", "all_time_gls", "eu_national", "current_ability")) %>% 
  step_string2factor(c("position", "side", "pref_foot","left_foot", "right_foot", "division", "nationality")) %>% 
  step_normalize(all_numeric_predictors())

# 3. prepare the recipe
df_rec_trained <- prep(df_rec)

show_variables <- 
  df_rec %>% 
  prep(log_changes = TRUE)


df_validation <- df_val$splits %>% pluck(1) %>% assessment()

# 4. bake the recipe
df_val_processed <- bake(df_rec_trained, new_data = df_validation)

plot_validation_results <- function(recipe, dat = assessment(df_val$splits[[1]])) {
  recipe %>%
    # Estimate any additional steps
    prep() %>%
    # Process the data (the validation set by default)
    bake(new_data = dat) %>%
    # Create the scatterplot matrix
    ggplot(aes(x = .panel_x, y = .panel_y, col = potential_ability, fill = potential_ability)) +
    geom_point(alpha = 0.4, size = 0.5) +
    geom_autodensity(alpha = .3) +
    facet_matrix(vars(-potential_ability), layer.diag = 2) 
}

df_rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  plot_validation_results() + 
  ggtitle("Principal Component Analysis")


df_rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>% 
  prep() %>% 
  plot_top_loadings(component_number <= 4, n = 5) + 
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Principal Component Analysis")
