# Script 3/3 for Paschen (2025): Acoustic disambiguation of homophones is exceptional. Journal of Linguistics. https://doi.org/10.1017/S0022226725100777

### 0) Load libraries
library(tidyverse)
library(here)
library(randomForest)

# Loading workspaces
load(here("01_Preprocessing.RData"))
load(here("02_Models.RData"))



### 12) Flipped model predicting 'morph' from duration

# Factors for the Random Forest models
factors <- c("mb_duration", "position_in_ipu", "wd_size", "speech_rate", "speaker", "wd_freq", "segmental_context")
predictor <- "gl"
results_all_gini_flipped <- data.frame(lang = character(), morph = character(), factor = character(), contribution = numeric(), r_sq = numeric())

# Loop to run Random Forests for each homophone set, per language
set.seed(44)
for(l in languages) {
  
  morphs_raw <- unique(homophone_data$mb_raw[homophone_data$lang == l])
  for(m in morphs_raw) {
    print(paste(l,m)) # debugging
    data_subset <- homophone_data %>% filter(lang == l, mb_raw == m) %>% select(c(all_of(factors)), all_of(predictor))
    data_subset$gl <- as.factor(data_subset$gl)
    
    if(nrow(data_subset) > 0){
      
      if (nrow(data_subset) > 1 && length(unique(data_subset$gl)) > 1) {

        rf <- randomForest(gl ~ ., 
                             data = data_subset, 
                             ntree = 500, 
                             mtry = 3, 
                             importance = TRUE)

        # Gini Coefficient
        contributions_gini <- rf$importance[, "MeanDecreaseGini"] / sum(rf$importance[, "MeanDecreaseGini"])

        # OOB (out-of-bag) error rate
        oob_error_rate <- rf$err.rate[nrow(rf$err.rate), "OOB"]
          
        # Save results
        for (f in factors) {
          contribution <- contributions_gini[f]
          results_all_gini_flipped <- rbind(
          results_all_gini_flipped, 
          data.frame(lang = l, 
                     morph = m, 
                     factor = f, 
                     contribution = contributions_gini[f], 
                     oob_error = oob_error_rate)
            )
          }
        }
      }
    }
  }

# Rename columns for better readability
results_all_gini_flipped <- results_all_gini_flipped %>%
  mutate(factor = case_when(
    factor == "mb_duration"  ~ "morph_duration",
    factor == "position_in_ipu" ~ "position",
    factor == "wd_freq" ~ "word_frequency",
    factor == "wd_size" ~ "word_size",
    TRUE ~ factor
  ))

# Remove models that exceed 25% OOB errors
results_all_gini_flipped_unfiltered <- results_all_gini_flipped
results_all_gini_flipped <- filter(results_all_gini_flipped_unfiltered, oob_error < 0.25)

# Figure: Factor x Contribution (over all languages)
contr_factor_flipped <- ggplot(results_all_gini_flipped, aes(x = reorder(factor, contribution, FUN = mean), y = contribution)) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "purple", size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "purple") +
  #  geom_hline(yintercept = 0.142, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0, 0.4)) +
  labs(x = "Factor", y = "Importance") +
  theme_gray() +
  theme(text = element_text(size = 32))
ggsave(filename = "img/appendix_contr_factor_flipped_37_44.png", plot = contr_factor_flipped, width = 10, height = 8, units = "in", dpi = 300)



### 13) Minimum model (removing the "speaker" variable)

# Factors for the Random Forest models
factors <- c("gl", "position_in_ipu", "wd_size", "speech_rate", "wd_freq", "segmental_context")
predictor <- "mb_duration"
results_all_purity_min <- data.frame(lang = character(), morph = character(), factor = character(), contribution = numeric(), r_sq = numeric())

# Loop to run Random Forests for each homophone set, per language
set.seed(44)
for(l in languages) {
  
  morphs_raw <- unique(homophone_data$mb_raw[homophone_data$lang == l])
  for(m in morphs_raw) {
    print(paste(l,m)) # debugging
    data_subset <- homophone_data %>% filter(lang == l, mb_raw == m) %>% select(c(all_of(factors)), all_of(predictor))
    if(nrow(data_subset) > 0){
      
      # Run model
      rf <- randomForest(mb_duration ~ ., ntree = 500, mtry = 3, data = data_subset, importance = TRUE)
      
      # Model statistics
      r_sq = max(rf$rsq)
      contributions_purity <- rf$importance[, "IncNodePurity"] / sum(rf$importance[, "IncNodePurity"])
      for (f in factors) {
        results_all_purity_min <- rbind(results_all_purity_min, data.frame(lang = l, morph = m, factor = f, contribution = contributions_purity[[f]], r_sq = r_sq))
      }
    }
  }
}

# Rename columns for better readability
results_all_purity_min <- results_all_purity_min %>%
  mutate(factor = case_when(
    factor == "gl"  ~ "morph",
    factor == "position_in_ipu" ~ "position",
    factor == "wd_freq" ~ "word_frequency",
    factor == "wd_size" ~ "word_size",
    TRUE ~ factor
  ))

# Remove models below a 25% performance threshold
results_all_purity_min_unfiltered <- results_all_purity_min
results_all_purity_min <- filter(results_all_purity_min_unfiltered, r_sq > 0.25)

# Figure: Factor x Contribution (over all languages)
contr_factor_min <- ggplot(results_all_purity_min, aes(x = reorder(factor, contribution, FUN = mean), y = contribution)) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "purple", size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "purple") +
  #  geom_hline(yintercept = 0.142, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0, 0.4)) +
  labs(x = "Factor", y = "Importance") +
  theme_gray() +
  theme(text = element_text(size = 32))
ggsave(filename = "img/appendix_contr_factor_min_37_44.png", plot = contr_factor_min, width = 10, height = 8, units = "in", dpi = 300)



### 14) Model with maximum information on segmental context, separating variables for left and right context and using actual segments rather than segment groups

# Factors for the Random Forest models
factors <- c("gl", "position_in_ipu", "wd_size", "speech_rate", "speaker", "wd_freq", "segmental_context_raw_r", "segmental_context_raw_l")
predictor <- "mb_duration"
results_all_purity_max <- data.frame(lang = character(), morph = character(), factor = character(), contribution = numeric(), r_sq = numeric())

# Loop to run Random Forests for each homophone set, per language
set.seed(44)
for(l in languages) {
  
  morphs_raw <- unique(homophone_data$mb_raw[homophone_data$lang == l])
  for(m in morphs_raw) {
    print(paste(l,m)) # debugging
    data_subset <- homophone_data %>% filter(lang == l, mb_raw == m) %>% select(c(all_of(factors)), all_of(predictor))
    if(nrow(data_subset) > 0){
      
      # Run model
      rf <- randomForest(mb_duration ~ ., ntree = 500, mtry = 3, data = data_subset, importance = TRUE)
      
      # Model statistics
      r_sq = max(rf$rsq)
      contributions_purity <- rf$importance[, "IncNodePurity"] / sum(rf$importance[, "IncNodePurity"])
      for (f in factors) {
        results_all_purity_max <- rbind(results_all_purity_max, data.frame(lang = l, morph = m, factor = f, contribution = contributions_purity[[f]], r_sq = r_sq))
      }
    }
  }
}

# Rename columns for better readability
results_all_purity_max <- results_all_purity_max %>%
  mutate(factor = case_when(
    factor == "gl"  ~ "morph",
    factor == "position_in_ipu" ~ "position",
    factor == "wd_freq" ~ "word_frequency",
    factor == "wd_size" ~ "word_size",
    factor == "segmental_context_raw_l" ~ "left_context",
    factor == "segmental_context_raw_r" ~ "right_context",
    TRUE ~ factor
  ))

# Remove models below a 25% performance threshold
results_all_purity_max_unfiltered <- results_all_purity_max
results_all_purity_max <- filter(results_all_purity_max_unfiltered, r_sq > 0.25)

# Figure: Factor x Contribution (over all languages)
contr_factor_max <- ggplot(results_all_purity_max, aes(x = reorder(factor, contribution, FUN = mean), y = contribution)) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "purple", size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "purple") +
  #  geom_hline(yintercept = 0.142, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0, 0.4)) +
  labs(x = "Factor", y = "Importance") +
  theme_gray() +
  theme(text = element_text(size = 32))
ggsave(filename = "img/appendix_contr_factor_max_37_44.png", plot = contr_factor_max, width = 10, height = 8, units = "in", dpi = 300)



### 15) Model with morph_type as an additional factor

# Factors for the Random Forest models
factors <- c("gl", "position_in_ipu", "wd_size", "speech_rate", "speaker", "wd_freq", "segmental_context", "mt")
predictor <- "mb_duration"
results_all_purity_mt <- data.frame(lang = character(), morph = character(), factor = character(), contribution = numeric(), r_sq = numeric())

# Loop to run Random Forests for each homophone set, per language
set.seed(44)
for(l in languages) {
  
  morphs_raw <- unique(homophone_data$mb_raw[homophone_data$lang == l])
  for(m in morphs_raw) {
    print(paste(l,m)) # debugging
    data_subset <- homophone_data %>% filter(lang == l, mb_raw == m) %>% select(c(all_of(factors)), all_of(predictor))
    if(nrow(data_subset) > 0){
      
      # Run model
      rf <- randomForest(mb_duration ~ ., ntree = 500, mtry = 3, data = data_subset, importance = TRUE)
      
      # Model statistics
      r_sq = max(rf$rsq)
      contributions_purity <- rf$importance[, "IncNodePurity"] / sum(rf$importance[, "IncNodePurity"])
      for (f in factors) {
        results_all_purity_mt <- rbind(results_all_purity_mt, data.frame(lang = l, morph = m, factor = f, contribution = contributions_purity[[f]], r_sq = r_sq))
      }
    }
  }
}

# Rename columns for better readability
results_all_purity_mt <- results_all_purity_mt %>%
  mutate(factor = case_when(
    factor == "gl"  ~ "morph",
    factor == "position_in_ipu" ~ "position",
    factor == "wd_freq" ~ "word_frequency",
    factor == "wd_size" ~ "word_size",
    factor == "mt" ~ "morph_type",
    TRUE ~ factor
  ))

# Remove models below a 25% performance threshold
results_all_purity_mt_unfiltered <- results_all_purity_mt
results_all_purity_mt <- filter(results_all_purity_mt_unfiltered, r_sq > 0.25)

# Figure: Factor x Contribution (over all languages)
contr_factor_mt <- ggplot(results_all_purity_mt, aes(x = reorder(factor, contribution, FUN = mean), y = contribution)) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "purple", size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "purple") +
  #  geom_hline(yintercept = 0.142, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0, 0.4)) +
  labs(x = "Factor", y = "Importance") +
  theme_gray() +
  theme(text = element_text(size = 32))
ggsave(filename = "img/appendix_contr_factor_mt_37_44.png", plot = contr_factor_mt, width = 10, height = 8, units = "in", dpi = 300)




### 16) Relation between dataset size and importance of 'morph'

# Data preparation
n_words_per_lang_humanreadable <- n_words_per_lang 
n_words_per_lang_humanreadable$lang <- mapping_vector[n_words_per_lang_humanreadable$lang]
results_morph_purity_with_token_counts <- results_morph_purity %>%
  left_join(n_words_per_lang_humanreadable, by = "lang")

# Figure
dataset_size_contrmorph <- ggplot(data= results_morph_purity_with_token_counts, aes(x = word_tokens, y = contribution)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", linewidth = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.175)) +
  labs(x = "Word tokens", y = "Importance of morph") +
  theme_gray() +
  theme(text = element_text(size = 32))
ggsave(filename = "img/appendix_datasetsize_contrmorph_37_44.png", plot = dataset_size_contrmorph, width = 10, height = 10, units = "in", dpi = 300)

# Linear regression
r_datasetsize <- cor(results_morph_purity_with_token_counts$contribution, results_morph_purity_with_token_counts$word_tokens)
model2 <- lm(contribution ~ word_tokens, data = results_morph_purity_with_token_counts)
p_datasetsize <- summary(model2)$coefficients[2, 4]



### 17) Housekeeping

# Save workspace
save.image(here("03_Everything.RData"))
