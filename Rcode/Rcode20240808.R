# -*- coding: utf-8 -*-
# @Author: bcdon
# @Date:   2024-06-20 15:25:27
# @Last Modified by: dbc
# @Last Modified time: 2024-08-15 10:57:11

# Clear memory
cat("\014")
rm(list = ls())

# Load packages
library(broom)
library(car)
library(coin)
library(dplyr)
library(ggpubr)
library(lmPerm)
library(patchwork)
library(purrr)
library(rcompanion)
library(readr)
library(readxl)
library(rlist)
library(rstatix)
library(stringr)
library(tidyr)
library(tidyverse)
library(vegan)

# citation("lmPerm")

# Set work directory
setwd("D:/我的坚果云/方向华老师项目")
getwd()

# Load data
plot.data <- read_csv("result.local_comm04.csv")
str(plot.data)

# Change the factor names
plot.data <- plot.data %>%
mutate(distance = str_replace_all(distance, "m", "")) %>%
mutate(invasion = factor(invasion, levels = c("non_ap", "ap")),
distance = as.factor(distance))

str(plot.data)

# Data range
range(plot.data$local_coverage_perc)

# Variables
variables <- c(
  "local_aboveground_mass",
  "local_no.plants",
  "local_coverage_perc",
  "richness",
  "evenness",
  "simpson_index",
  # "local_vegetative_mass",
  # "local_sexual_organ_mass",
  "its_observed_features",
  "its_pielou_e",
  "its_simpson",
  "X16s_observed_features",
  "X16s_pielou_e",
  "X16s_simpson"
  )

# Split data into four subsets based on invasion_status02
# Remove Alternanthera subset
plot.data01 <- plot.data %>% filter(invasion_status02 != "Alternanthera")
split_data <- split(plot.data01, plot.data01$invasion_status02)

# Calculate replicates
plot.data01 %>%
  group_by(invasion_status02, invasion, distance) %>%
  summarise(
    n = n())

# Extract replicates based on invasion status level
# # A tibble: 16 × 4
# # Groups:   invasion_status02, invasion [8]
#    invasion_status02 invasion distance     n
#    <chr>             <fct>    <fct>    <int>
#  1 community         non_ap   1           10
#  2 community         non_ap   5           10
#  3 community         ap       1           10
#  4 community         ap       5           10
#  5 invasive          non_ap   1            7
#  6 invasive          non_ap   5            6
#  7 invasive          ap       1            9
#  8 invasive          ap       5            8
#  9 native            non_ap   1            9
# 10 native            non_ap   5           10
# 11 native            ap       1           10
# 12 native            ap       5           10
# 13 non_invasive      non_ap   1            6
# 14 non_invasive      non_ap   5            8
# 15 non_invasive      ap       1            9
# 16 non_invasive      ap       5            8

# Calculate measures loss or gain between invasion levels
result_invasion <- plot.data01 %>%
  group_by(invasion_status02, invasion) %>%
  summarise(n = n(),
            mean_coverage = mean(local_coverage_perc, na.rm = TRUE),
            mean_richness = mean(richness, na.rm = TRUE),
            mean_evenness = mean(evenness, na.rm = TRUE),
            mean_simpson_index = mean(simpson_index, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(invasion_status02) %>%
  summarise(
    coverage_percent = (mean_coverage[invasion == "ap"] - mean_coverage[invasion == "non_ap"]) / mean_coverage[invasion == "non_ap"] * 100,
    richness_percent = (mean_richness[invasion == "ap"] - mean_richness[invasion == "non_ap"]) / mean_richness[invasion == "non_ap"] * 100,
    evenness_percent = (mean_evenness[invasion == "ap"] - mean_evenness[invasion == "non_ap"]) / mean_evenness[invasion == "non_ap"] * 100,
    simpson_index_percent = (mean_simpson_index[invasion == "ap"] - mean_simpson_index[invasion == "non_ap"]) / mean_simpson_index[invasion == "non_ap"] * 100
    )

result_invasion
print(result_invasion, width = Inf)
# # A tibble: 4 × 5
#   invasion_status02 coverage_percent richness_percent evenness_percent
#   <chr>                        <dbl>            <dbl>            <dbl>
# 1 community                    -32.4            50              20.0
# 2 invasive                     -70.1            -8.24           16.7
# 3 native                       -31.2            43.1            24.3
# 4 non_invasive                 -58.4            55.6            -0.293
#   simpson_index_percent
#                   <dbl>
# 1                29.1
# 2                16.0
# 3                34.1
# 4                -0.461

# Calculate measures loss or gain between distance levels
# local_aboveground_mass
# local_no.plants
result_distance <- plot.data01 %>%
  group_by(invasion_status02, invasion, distance) %>%
  summarise(n = n(),
            mean_aboveground_mass = mean(local_aboveground_mass, na.rm = TRUE),
            mean_no.plants = mean(local_no.plants, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(invasion_status02, invasion) %>%
  summarise(
    aboveground_mass_percent = (mean_aboveground_mass[distance == 5] - mean_aboveground_mass[distance == 1]) / mean_aboveground_mass[distance == 1] * 100,
    no.plants_percent = (mean_no.plants[distance == 5] - mean_no.plants[distance == 1]) / mean_no.plants[distance == 1] * 100
    )

result_distance
print(result_distance, width = Inf)
# # A tibble: 8 × 4
# # Groups:   invasion_status02 [4]
#   invasion_status02 invasion aboveground_mass_percent no.plants_percent
#   <chr>             <fct>                       <dbl>             <dbl>
# 1 community         non_ap                      12.1             -43.7
# 2 community         ap                         -21.8               2.01
# 3 invasive          non_ap                     -78.6             -72.4
# 4 invasive          ap                          58.1             -26.9
# 5 native            non_ap                      -3.16            -57.0
# 6 native            ap                         -19.8              21.6
# 7 non_invasive      non_ap                     108.                4.17
# 8 non_invasive      ap                         -51.0             -24.1

# Calculate mean and se of measures based on invasion and distance levels
split_data$community %>%
select(soil_moist, totalC, totalN, totalP, totalK, distance) %>%
group_by(distance) %>%
get_summary_stats(type = "mean_se") %>%
arrange(variable)

## =========================================================================
# ANCOVA: ANOVA with covariates
# ==========================================================================
# Function
anova_results <- map(split_data, ~ map(variables, function(var) {
  formula <- as.formula(paste(var, " ~ soil_moist + totalC + totalN + totalP + totalK + invasion + distance + invasion:distance"))

  # Permutation test
  set.seed(2024)
  perm_anova <- aovp(formula, data = .x, np = 999)

  # anova_result <- as.data.frame(perm_anova$tab)
  anova_result <- perm_anova %>% tidy()
  anova_result$variable <- var
  anova_result$invasion_status02 <- unique(.x$invasion_status02)
  anova_result

}))

# Combine results into a data frame
anova_results_df <- bind_rows(anova_results)
View(anova_results_df)

# Save results to CSV file
write_csv(anova_results_df, "20240809.anova_results_df.csv")

## =========================================================================
# ANOVA: models with covariates
# ==========================================================================
# Function
anova_results.0cov <- map(split_data, ~ map(variables, function(var) {
  formula <- as.formula(paste(var, " ~ invasion + distance + invasion:distance"))

  # Permutation test
  set.seed(2024)
  perm_anova <- aovp(formula, data = .x, np = 999)

  # anova_result <- as.data.frame(perm_anova$tab)
  anova_result <- perm_anova %>% tidy()
  anova_result$variable <- var
  anova_result$invasion_status02 <- unique(.x$invasion_status02)
  anova_result

}))

# Combine results into a data frame
anova_results.0cov_df <- bind_rows(anova_results.0cov)
View(anova_results.0cov_df)

# Save results to CSV file
write_csv(anova_results.0cov_df, "20240809.anova_results.0cov_df.csv")

# ==========================================================================
# Permutation ANOVA and pairwise comparison
# ==========================================================================
# Main analysis function
aovp_posthoc <- function(data, response, factors, np = 999) {

  # Build formula
  formula <- as.formula(paste(response, "~", paste(factors[1], factors[2], paste0(factors[1], ":", factors[2]), sep = "+")))

  # Perform permutation ANOVA
  set.seed(2024)
  perm_anova <- aovp(formula, data = data, np = np)
  anova_result <- perm_anova %>% tidy() %>%
                              mutate(across(everything(), as.character)) %>%
                              mutate(variable = response,
                                   invasion_status02 = unique(data$invasion_status02))

  # Post hoc comparison for each main effect
  posthoc_results <- map(factors, function(factor) {
    posthoc_formula <- as.formula(paste(response, "~", factor))
    pairwisePermutationTest(posthoc_formula, data = data, method = "fdr",  distribution = approximate(nresample = np))
  })
  names(posthoc_results) <- factors

  # If there is an interaction effect, perform simple main effect analysis
  if (length(factors) > 1) {
    interaction_term <- paste(factors, collapse = ":")

    if (interaction_term %in% anova_result$term) {
      # if (anova_result$'Pr(Prob)'[anova_result$term == interaction_term] < 0.05) {
      # Create interaction effect combinations
      data$interaction <- interaction(data[[factors[1]]], data[[factors[2]]])

      # Post hoc comparison for interaction effect
      posthoc_interaction <- pairwisePermutationTest(as.formula(paste(response, "~ interaction")),
                                                       data = data, method = "fdr",  distribution = approximate(nresample = np))

      posthoc_results$interaction <- posthoc_interaction

      posthoc_results_df <- posthoc_results %>%
                            bind_rows(.id = "source") %>%
                            mutate(across(everything(), as.character)) %>%
                            mutate(variable = response,
                                   invasion_status02 = unique(data$invasion_status02))
      # }
    }
  }

  list(anova = anova_result, posthoc = posthoc_results_df)
}


# Perform analysis for each subset
anova_results.posthoc <- map(split_data, function(subset_data) {
  map(variables, function(var) {
    result <- aovp_posthoc(subset_data, var, c("invasion", "distance"))
    result
  })
})

# Extract the parameters from results
extract_data <- function(anova_results.posthoc) {
  result_list <- map(names(anova_results.posthoc), function(subset_name) {
    subset <- anova_results.posthoc[[subset_name]]

    anova_df <- subset %>% map_dfr(~ .x$anova, .id = "list_id")
    posthoc_df <- subset %>% map_dfr(~ .x$posthoc, .id = "list_id")

      # Post-hoc results
      anova_df <- anova_df %>%
        mutate(result_type = "anova_df")

      posthoc_df <- posthoc_df %>%
        mutate(result_type = "posthoc_df")

      # Combine all results
      all_results <- bind_rows(anova_df, posthoc_df)
      all_results

    }
    )

  bind_rows(result_list)
}

# Use function to extract data
anova_results.posthoc_df <- extract_data(anova_results.posthoc)

# print(anova_results.posthoc_df)

# Save results to CSV file
write_csv(anova_results.posthoc_df, "20240809.anova_results.posthoc_df.0cov.csv")

# ==========================================================================
# Plotting function
# ==========================================================================
# Define variables to fit
variables <- c(
  "local_aboveground_mass",
  "local_no.plants",
  "local_coverage_perc",
  "richness",
  "evenness",
  "simpson_index",
  # "local_vegetative_mass",
  # "local_sexual_organ_mass",
  "its_observed_features",
  "its_pielou_e",
  "its_simpson",
  "X16s_observed_features",
  "X16s_pielou_e",
  "X16s_simpson"
  )


# Prepare data for plotting
plot_data_long <- plot.data01 %>%
  filter(invasion_status02 != "Alternanthera") %>%
  pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "value")

split.data_long <- split(plot_data_long, plot_data_long$invasion_status02)

# Use nested map() function to create plot list for each invasion_status02 group
plot_list <- map(split.data_long, ~ map(variables, function(var) {

      ggplot(.x %>% filter(variable == var & !is.na(value)), aes(x = distance, y = value, color = invasion)) +
        # geom_boxplot(outlier.shape = NA, alpha = 0.5, position = "dodge") +
        # geom_point(position = position_jitterdodge(), size = 2, alpha = 0.5) +
        # geom_violin(position = position_dodge(width = 0.5), alpha = 0.7) +
        stat_summary(fun.data = "mean_se", geom = "errorbar",
                 width = 0.2, position = position_dodge(width = 0.5), size = 1) +
        stat_summary(fun = "mean", geom = "point",
                 position = position_dodge(width = 0.5), size = 2) +
        # Add dashed lines connecting mean points
        stat_summary(fun = "mean", geom = "line", aes(group = invasion),
               linetype = "dashed", position = position_dodge(width = 0.5), size = 1) +
        labs(x = "Distance to river edge (m)", y = "Value", color = "Invasion status", title = paste(var, "-", unique(.x$invasion_status02))) +
        theme_classic() +
        theme(panel.background = element_rect(fill = NA),
              legend.position = "none",
              legend.background = element_blank(),
              axis.ticks = element_line(colour = "black", linetype = "solid", linewidth = 1),
              axis.text = element_text(family = "serif", colour = "black", size = 14),
              legend.title = element_text(family = "serif", colour = "black", size = 14),
              legend.text = element_text(family = "serif", colour = "black", size = 14),
              axis.title = element_text(family = "serif", colour = "black", size = 14),
              plot.title = element_text(family = "serif", colour = "black", size = 14)) +
        scale_color_manual(values = c("non_ap" = "#1B9E77", "ap" = "#D95F02"),
                          labels = c("non_ap" = "Not invaded", "ap" = "Invaded"))
    }) %>%
      set_names(variables)  # Set names for each subplot, corresponding to variable names

)

plot_list01 <- plot_list
print(plot_list)

# Save plots
# Use patchwork to combine plots
wrapped_plot <- wrap_plots(plot_list$community, ncol = 3)
wrapped_plot

# Community
plot_list01$community$local_aboveground_mass <- plot_list01$community$local_aboveground_mass + labs(title = "", y = "Above-ground mass (g)") + scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) + theme(legend.position = c(0.4, 0.8))
plot_list01$community$local_coverage_perc <- plot_list01$community$local_coverage_perc + labs(title = "", y = "Coverage (%)") + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by =20))
plot_list01$community$local_no.plants <- plot_list01$community$local_no.plants + labs(title = "", y = "Number of plants") + scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100))
plot_list01$community$evenness <- plot_list01$community$evenness + labs(title = "", y = "Evenness") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
plot_list01$community$richness <- plot_list01$community$richness + labs(title = "", y = "Richness") + scale_y_continuous(limits = c(0, 20), breaks = seq(0,20, by = 5))
plot_list01$community$simpson_index <- plot_list01$community$simpson_index + labs(title = "", y = "Simpson's index") + scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.2))

plot_list01$community$local_coverage_perc <- plot_list01$community$local_coverage_perc + annotate("text", x = 1.5, y = 10, label = "I: <0.001", family = "serif", colour = "black", size = 5)
plot_list01$community$richness <- plot_list01$community$richness + annotate("text", x = 1.5, y = 20, label = "I: 0.001", family = "serif", colour = "black", size = 5)
plot_list01$community$evenness <- plot_list01$community$evenness + annotate("text", x = 1.5, y = 1, label = "I: 0.016", family = "serif", colour = "black", size = 5)
plot_list01$community$simpson_index <- plot_list01$community$simpson_index + annotate("text", x = 1.5, y = 1, label = "I: 0.005", family = "serif", colour = "black", size = 5)

plot_list01$community$local_aboveground_mass
plot_list01$community$local_coverage_perc
plot_list01$community$local_no.plants
plot_list01$community$evenness
plot_list01$community$richness
plot_list01$community$simpson_index

# Microbes
plot_list01$community$its_observed_features <- plot_list01$community$its_observed_features + labs(title = "Soil fungal community", y = "Observed features") + scale_y_continuous(limits = c(0, 2500), breaks = seq(0,2500, by = 500))
plot_list01$community$its_pielou_e <- plot_list01$community$its_pielou_e + labs(title = "", y = "Evenness") + scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1)) + theme(legend.position = c(0.4, 0.8))
plot_list01$community$its_simpson <- plot_list01$community$its_simpson + labs(title = "", y = "Simpson's index") + scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.05))
plot_list01$community$X16s_observed_features <- plot_list01$community$X16s_observed_features + labs(title = "Soil bacterial community", y = "Observed features") + scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000))
plot_list01$community$X16s_pielou_e <- plot_list01$community$X16s_pielou_e + labs(title = "", y = "Evenness") + scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1))
plot_list01$community$X16s_simpson <- plot_list01$community$X16s_simpson + labs(title = "", y = "Simpson's index") + scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.05))

plot_list01$community$its_observed_features
plot_list01$community$its_pielou_e
plot_list01$community$its_simpson
plot_list01$community$X16s_observed_features
plot_list01$community$X16s_pielou_e
plot_list01$community$X16s_simpson


# Invasive
plot_list01$invasive$local_aboveground_mass <- plot_list01$invasive$local_aboveground_mass + labs(title = "", y = "Above-ground mass (g)") + scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) + theme(legend.position = c(0.4, 0.8))
plot_list01$invasive$local_coverage_perc <- plot_list01$invasive$local_coverage_perc + labs(title = "", y = "Coverage (%)") + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by =20))
plot_list01$invasive$local_no.plants <- plot_list01$invasive$local_no.plants + labs(title = "", y = "Number of plants") + scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100))
plot_list01$invasive$evenness <- plot_list01$invasive$evenness + labs(title = "", y = "Evenness") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
plot_list01$invasive$richness <- plot_list01$invasive$richness + labs(title = "", y = "Richness") + scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
plot_list01$invasive$simpson_index <- plot_list01$invasive$simpson_index + labs(title = "", y = "Simpson's index") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

split.data_long$invasive %>%
filter(variable == "local_aboveground_mass") %>%
group_by(invasion, distance) %>%
summarise(max = mean(value)) %>%
ungroup()
# # A tibble: 4 × 3
#   invasion distance   max
#   <fct>    <fct>    <dbl>
# 1 non_ap   1        16.5
# 2 non_ap   5         3.52
# 3 ap       1         6.71
# 4 ap       5        10.6

plot_list01$invasive$local_aboveground_mass <- plot_list01$invasive$local_aboveground_mass +
annotate("text", x = 1.5, y = 25, label = "I %*% D: 0.044", family = "serif", colour = "black", size = 5, parse = TRUE) +
annotate("text", x = 0.7, y = 16.5, label = "*", family = "serif", colour = "#1B9E77", size = 5) +
annotate("text", x = 2.3, y = 10.6, label = "ns", family = "serif", colour = "#D95F02", size = 5)

plot_list01$invasive$local_coverage_perc <- plot_list01$invasive$local_coverage_perc + annotate("text", x = 1.5, y = 100, label = "I: 0.001", family = "serif", colour = "black", size = 5)

plot_list01$invasive$local_aboveground_mass
plot_list01$invasive$local_coverage_perc
plot_list01$invasive$local_no.plants
plot_list01$invasive$evenness
plot_list01$invasive$richness
plot_list01$invasive$simpson_index

# Native
plot_list01$native$local_aboveground_mass <- plot_list01$native$local_aboveground_mass + labs(title = "", y = "Above-ground mass (g)") + scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) + theme(legend.position = c(0.4, 0.8))
plot_list01$native$local_coverage_perc <- plot_list01$native$local_coverage_perc + labs(title = "", y = "Coverage (%)") + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))
plot_list01$native$local_no.plants <- plot_list01$native$local_no.plants + labs(title = "", y = "Number of plants") + scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100))
plot_list01$native$evenness <- plot_list01$native$evenness + labs(title = "", y = "Evenness") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
plot_list01$native$richness <- plot_list01$native$richness + labs(title = "", y = "Richness") + scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5))
plot_list01$native$simpson_index <- plot_list01$native$simpson_index + labs(title = "", y = "Simpson's index") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

split.data_long$native %>%
filter(variable == "local_no.plants") %>%
group_by(invasion, distance) %>%
summarise(mean = mean(value)) %>%
ungroup()
# # A tibble: 4 × 3
#   invasion distance  mean
#   <fct>    <fct>    <dbl>
# 1 non_ap   1        138.
# 2 non_ap   5         59.3
# 3 ap       1        106.
# 4 ap       5        128.

plot_list01$native$local_no.plants <- plot_list01$native$local_no.plants +
annotate("text", x = 1.5, y = 400, label = "I %*% D: 0.023", family = "serif", colour = "black", size = 5, parse = TRUE) +
annotate("text", x = 0.7, y = 138, label = "*", family = "serif", colour = "#1B9E77", size = 5) +
annotate("text", x = 2.3, y = 128, label = "ns", family = "serif", colour = "#D95F02", size = 5)

plot_list01$native$local_coverage_perc <- plot_list01$native$local_coverage_perc + annotate("text", x = 1.5, y = 10, label = "I: 0.014", family = "serif", colour = "black", size = 5)
plot_list01$native$richness <- plot_list01$native$richness + annotate("text", x = 1.5, y = 20, label = "I: 0.028", family = "serif", colour = "black", size = 5)
plot_list01$native$simpson_index <- plot_list01$native$simpson_index + annotate("text", x = 1.5, y = 1, label = "I: 0.040", family = "serif", colour = "black", size = 5)

plot_list01$native$local_aboveground_mass
plot_list01$native$local_coverage_perc
plot_list01$native$local_no.plants
plot_list01$native$evenness
plot_list01$native$richness
plot_list01$native$simpson_index

# Non-invasive
plot_list01$non_invasive$local_aboveground_mass <- plot_list01$non_invasive$local_aboveground_mass + labs(title = "", y = "Above-ground mass (g)") + scale_y_continuous(limits = c(0,200), breaks = seq(0, 200, by = 50)) + theme(legend.position = c(0.4, 0.8))
plot_list01$non_invasive$local_coverage_perc <- plot_list01$non_invasive$local_coverage_perc + labs(title = "", y = "Coverage (%)") + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))
plot_list01$non_invasive$local_no.plants <- plot_list01$non_invasive$local_no.plants + labs(title = "", y = "Number of plants") + scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100))
plot_list01$non_invasive$evenness <- plot_list01$non_invasive$evenness + labs(title = "", y = "Evenness") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
plot_list01$non_invasive$richness <- plot_list01$non_invasive$richness + labs(title = "", y = "Richness") + scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
plot_list01$non_invasive$simpson_index <- plot_list01$non_invasive$simpson_index + labs(title = "", y = "Simpson's index") + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

plot_list01$non_invasive$local_coverage_perc <- plot_list01$non_invasive$local_coverage_perc + annotate("text", x = 1.5, y = 100, label = "I: 0.017", family = "serif", colour = "black", size = 5)
plot_list01$non_invasive$richness <- plot_list01$non_invasive$richness + annotate("text", x = 1.5, y = 5, label = "I: 0.044", family = "serif", colour = "black", size = 5)

plot_list01$non_invasive$local_aboveground_mass
plot_list01$non_invasive$local_coverage_perc
plot_list01$non_invasive$local_no.plants
plot_list01$non_invasive$evenness
plot_list01$non_invasive$richness
plot_list01$non_invasive$simpson_index

# ==========================================================================
# Generate plots
# ==========================================================================

layout <- "
AD
BE
CF
"

plots.community <-
plot_list01$community$local_aboveground_mass +
plot_list01$community$local_no.plants +
plot_list01$community$local_coverage_perc +
plot_list01$community$richness +
plot_list01$community$evenness +
plot_list01$community$simpson_index +
# plot_layout(ncol = 2) +
plot_layout(ncol = 2, design = layout) +
plot_annotation(tag_levels = "A")

plots.invasive <-
plot_list01$invasive$local_aboveground_mass +
plot_list01$invasive$local_no.plants +
plot_list01$invasive$local_coverage_perc +
plot_list01$invasive$richness +
plot_list01$invasive$evenness +
plot_list01$invasive$simpson_index +
plot_layout(ncol = 2, design = layout) +
plot_annotation(tag_levels = "A")

plots.non_invasive <-
plot_list01$non_invasive$local_aboveground_mass +
plot_list01$non_invasive$local_no.plants +
plot_list01$non_invasive$local_coverage_perc +
plot_list01$non_invasive$richness +
plot_list01$non_invasive$evenness +
plot_list01$non_invasive$simpson_index +
plot_layout(ncol = 2, design = layout) +
plot_annotation(tag_levels = "A")

plots.native <-
plot_list01$native$local_aboveground_mass +
plot_list01$native$local_no.plants +
plot_list01$native$local_coverage_perc +
plot_list01$native$richness +
plot_list01$native$evenness +
plot_list01$native$simpson_index +
plot_layout(ncol = 2, design = layout) +
plot_annotation(tag_levels = "A")

plots.community.soil <-
plot_list01$community$its_observed_features +
plot_list01$community$its_pielou_e +
plot_list01$community$its_simpson +
plot_list01$community$X16s_observed_features +
plot_list01$community$X16s_pielou_e +
plot_list01$community$X16s_simpson +
plot_layout(ncol = 2, design = layout) +
plot_annotation(tag_levels = "A")

# Save combined plots for community
ggexport(plots.community, filename = "./20240809.plots.community.png",
         width = 2000,
         height = 2700,
         pointsize = 12,
         res = 300)

# Save combined plots for microbes
ggexport(plots.community.soil, filename = "./20240809.plots.community.soil.png",
         width = 2000,
         height = 2700,
         pointsize = 12,
         res = 300)

# Save combined plots for invasive
ggexport(plots.invasive, filename = "./20240809.plots.invasive.png",
         width = 2000,
         height = 2700,
         pointsize = 12,
         res = 300)

# Save combined plots for non_invasive
ggexport(plots.non_invasive, filename = "./20240809.plots.non_invasive.png",
         width = 2000,
         height = 2700,
         pointsize = 12,
         res = 300)

# Save combined plots for native
ggexport(plots.native, filename = "./20240809.plots.native.png",
         width = 2000,
         height = 2700,
         pointsize = 12,
         res = 300)