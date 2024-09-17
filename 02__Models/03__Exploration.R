# Making a data exploration file
rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggeasy)
library(patchwork)
library(easystats)

data_filtered <- readxl::read_xlsx(file.path("01__Data/02__data_filtered.xlsx"))

# Fixing CFC-Immediate sub scaling
# https://scales.arabpsychology.com/s/two-factor-consideration-of-future-consequences-scale-cfc-14/
data_filtered <- data_filtered %>%
  mutate(
    Total_CFCI = rowSums(across(c(CFC3:CFC5, CFC9:CFC12)), na.rm = TRUE)
  ) %>%
  mutate(age_group = cut(Age, breaks = c(18, 24, 45, 77), 
                         labels = c("18-24", "25-44", "45+"), 
                         include.lowest = TRUE),
         age_group = factor(age_group, levels = c("18-24", "25-44", "45+"))) %>%
  rename(GeneralHealth = GeneralHealthRating,
         SLE_prime = SLEPRIME,
         SLE = SubjectiveLifeExpectancy,
         Total_CFC = Total_CFC14Scores)

# Processing
data <- data_filtered %>%
  mutate(
    # Converting sex to factor
    Sex = ifelse(Sex == 1, "Male", "Female"),
    Sex = factor(Sex, levels = c("Male", "Female")),
    
    # Specifying health assessment
    GeneralHealth = case_when(
      GeneralHealth == 1 ~ "Excellent",
      GeneralHealth == 2 ~ "Very Good",
      GeneralHealth == 3 ~ "Good",
      GeneralHealth == 4 ~ "Fair",
      GeneralHealth == 5 ~ "Poor"),
    GeneralHealth = factor(GeneralHealth, levels = c("Excellent", "Very Good", 
                                                     "Good", "Fair", "Poor")),
    
    # Specifying chronic illness
    ChronicIllness = case_when(
      ChronicIllness == 1 ~ "Present",
      ChronicIllness == 2 ~ "Not Present",
      ChronicIllness == 3 ~ "Prefer not to say"),
    ChronicIllness = factor(ChronicIllness, levels = c("Present", "Not Present", 
                                                       "Prefer not to say"))
  )

# Visualizing data ------------------------------------------------------------- 
# Factor variables -------------------------------------------------------------
# Gender balance
fig_1 <- data %>%
  ggplot(aes(Sex)) +
  geom_bar(aes(fill = Sex), colour = "black", width = 0.75) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "Gender Balance", x = "", y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_classic() +
  easy_remove_legend() +
  easy_center_title()

# Age group balance
fig_2 <- data %>%
  filter(!is.na(age_group)) %>%
  ggplot(aes(age_group)) +
  geom_bar(aes(fill = age_group), colour = "black", width = 0.75) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "Age Group Distribution", x = "", y = "Count") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon")) +
  theme_classic() +
  easy_remove_legend() +
  easy_center_title()

# General Health Assessment
fig_3 <- data %>%
  ggplot(aes(GeneralHealth)) +
  geom_bar(aes(fill = Sex), colour = "black", width = 0.75, position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., group = Sex), 
            position = position_dodge(width = 0.75), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "General Health Assessment", x = "General Health", y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_classic() +
  easy_center_title() +
  easy_remove_legend_title() +
  easy_move_legend(to = "bottom")

# Chronic Illness
fig_4 <- data %>%
  ggplot(aes(ChronicIllness)) +
  geom_bar(aes(fill = ChronicIllness), colour = "black", width = 0.75) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon")) +
  labs(title = "Chronic Illness", x = NULL, y = "Count") +
  theme_classic() +
  easy_center_title() +
  easy_remove_legend()

patch_1 <- (fig_1 + fig_2) / (fig_3 + fig_4)

# Subjective Life Expectancy
# For people your age
fig_5 <- data %>%
  ggplot(aes(SLE_prime)) +
  geom_histogram(binwidth = 5, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(50, 105, by = 5)) +
  labs(title = "To what age do you think someone your age will live", x = "Subjective Life Expectency", y = "Count") +
  theme_minimal() +
  easy_center_title()

# For yourself
fig_6 <- data %>%
  ggplot(aes(SLE)) +
  geom_histogram(binwidth = 5, fill = "salmon", colour = "black") +
  scale_x_continuous(breaks = seq(50, 105, by = 5)) +
  labs(title = "To age do you think you'll live", x = "Subjective Life Expectency", y = "Count") +
  theme_minimal() +
  easy_center_title()

# Relationship between both
fig_7 <- data %>%
  ggplot(aes(x = SLE, y = SLE_prime)) +
  geom_jitter(colour = "#2E2E2E", alpha = 0.8) +
  scale_x_continuous(breaks = seq(50, 105, by = 5)) +
  scale_y_continuous(breaks = seq(50, 105, by = 5)) +
  labs(title = "Relationship between SLE and SLE Prime", 
       x = "To age do you think you'll live", y = "To what age do you think someone your age will live") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9)
  )


patch_2 <- (fig_5 + fig_6) / fig_7
  
# Continous plots --------------------------------------------------------------
# Age 
fig_8 <- data %>%
  dplyr::select(Sex, Age) %>%
  ggplot(aes(Age)) +
  geom_histogram(aes(fill = Sex), colour = "black") +
  scale_x_continuous(breaks = seq(15, 75, by = 5)) +
  labs(title = "Age distribution by sex") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  facet_wrap(~ Sex) +
  theme_minimal() +
  easy_center_title() +
  easy_remove_legend()

# Consideration of consequences 
# Immediate
fig_9 <- data %>%
  ggplot(aes(Total_CFCI)) +
  geom_histogram(binwidth = 3, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(7, 49, by = 7)) +
  labs(title = "Consideration of immediate consequences", x = "CFC-I", y = "count") +
  theme_minimal() +
  easy_center_title()

# Future 
fig_10 <- data %>%
  ggplot(aes(Total_CFCF)) +
  geom_histogram(binwidth = 3, fill = "salmon", colour = "black") +
  scale_x_continuous(breaks = seq(7, 49, by = 7)) +
  labs(title = "Consideration of future consequences", x = "CFC-F", y = "count") +
  theme_minimal() +
  easy_center_title()

# Procrastination 
fig_11 <- data %>%
  ggplot(aes(Total_PPS)) +
  geom_histogram(binwidth = 3, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  labs(title = "Total Procrastination", x = "Procrastination", y = "Count") +
  theme_minimal() +
  easy_center_title()

patch_3 <- (fig_8 + fig_11) / (fig_9 + fig_10)

# Correlations -----------------------------------------------------------------
correlation_results <- data %>%
  dplyr::select(Age, Total_PPS, Total_CFCF, Total_CFCI, 
         SLE_prime, SLE) %>%
  rename(
    PPS = Total_PPS,
    CFC_F = Total_CFCF,
    CFC_I = Total_CFCI,
    SLE_Prime = SLE_prime
  ) %>%
  correlation()

# Plotting
correlation_matrix <- summary(correlation_results) %>%
  plot() +
  theme_classic() +
  easy_center_title()

# Significant correlations
cor_1 <- ggstatsplot::ggscatterstats(
  data = data, x = Total_PPS, y = Total_CFCI, 
  xlab = "Total Procrastination", ylab = "CFC-I", 
  title = "Relationship between procrastination and CFC Immediate", 
  bf.message = FALSE  # Not doing Bayesian Stuff 
) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(7, 49, by = 7)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

cor_2 <- ggstatsplot::ggscatterstats(
  data = data, x = Age, y = Total_PPS,
  xlab = "Age", ylab = "Procrastination",
  title = "Relationship between procrastination and age",
  bf.message = FALSE,
) +
  scale_x_continuous(breaks = seq(15, 75, by = 10)) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Exporting --------------------------------------------------------------------
export_data <- "01__Data/"
export_graphics <- "02__Models/Results/01__EDA"

# Amended dataset
writexl::write_xlsx(
  x = data_filtered,
  path = file.path(export_data, "02__data_filtered.xlsx"))

# Plots
cowplot::save_plot(
  filename = file.path(export_graphics, "01__Factor_plots.png"),
  plot = patch_1, base_height = 7)

cowplot::save_plot(
  filename = file.path(export_graphics, "02__SLE_plots.png"),
  plot = patch_2, base_height = 7)

cowplot::save_plot(
  filename = file.path(export_graphics, "03__Continous_plots.png"),
  plot = patch_3, base_height = 7)

cowplot::save_plot(
  filename = file.path(export_graphics, "04a__Correlation_matix.png"),
  plot = correlation_matrix, base_height = 7)

cowplot::save_plot(
  filename = file.path(export_graphics, "04b__Correlation_1.png"),
  plot = cor_1, base_height = 8)

cowplot::save_plot(
  filename = file.path(export_graphics, "04c__Correlation_2.png"),
  plot = cor_2, base_height = 8)

