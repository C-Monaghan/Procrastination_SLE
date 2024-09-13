# Making a data exploration file
rm(list = ls())

library(dplyr)
library(ggplot2)
library(easystats)

data_raw <- readxl::read_xlsx(file.path("Data/02__Data.xlsx"))

# Processing
data <- data_raw %>%
  mutate(
    # Converting from numeric to factor
    GeneralHealth = factor(GeneralHealth),
    Healthissue = factor(Healthissue),
    
    # Converting sex to factor
    Sex = ifelse(Sex == 1, "Male", "Female"),
    Sex = factor(Sex, levels = c("Male", "Female")),
    
    # Specifying age groups
    Agegrp1 = case_when(
      Agegrp1 == 1 ~ "18 - 29",
      Agegrp1 == 2 ~ "30 - 44",
      Agegrp1 == 3 ~ "45+"),
    Agegrp1 = factor(Agegrp1, levels = c("18 - 29", "30 - 44", "45+")),
    
    # Specifying health assessment
    GeneralHealth = case_when(
      GeneralHealth == 1 ~ "Excellent",
      GeneralHealth == 2 ~ "Very Good",
      GeneralHealth == 3 ~ "Good",
      GeneralHealth == 4 ~ "Fair",
      GeneralHealth == 5 ~ "Poor"),
    GeneralHealth = factor(GeneralHealth, levels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))
  )

# Visualizing data ------------------------------------------------------------- 
# Gender balance
data %>%
  ggplot(aes(Sex)) +
  geom_bar(aes(fill = Sex), colour = "black", width = 0.75) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "Gender Balance", x = "", y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal() +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_center_title()

# Age group balance
data %>%
  ggplot(aes(Agegrp1)) +
  geom_bar(aes(fill = Agegrp1), colour = "black", width = 0.75) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "Age Group Distribution", x = "", y = "Count") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon")) +
  theme_minimal() +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_center_title()

# General Health Assessment
data %>%
  ggplot(aes(GeneralHealth)) +
  geom_bar(aes(fill = Sex), colour = "black", width = 0.75, position = "dodge") +
  labs(title = "General Health Assessment", x = "General Health", y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend_title() +
  ggeasy::easy_move_legend(to = "bottom")

# Subjective Life Expectancy
data %>%
  ggplot(aes(Subjectivelifeexpectancy)) +
  geom_bar(aes(fill = Agegrp1), colour = "black", position = "dodge") +
  scale_x_continuous(breaks = seq(50, 105, by = 5)) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon", "lightyellow", "lavender")) +
  labs(x = "Subjective Life Expectency", y = "Count") +
  facet_wrap(~ Sex)+
  theme_minimal() +
  ggeasy::easy_remove_legend_title() +
  ggeasy::easy_move_legend("bottom")

# Age distribution (as a whole)
data %>%
  ggplot(aes(Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(15, 75, by = 5)) +
  theme_minimal()

# Age distribution (by gender)
data %>%
  select(Sex, Age) %>%
  ggplot(aes(Age)) +
  geom_histogram(aes(fill = Sex), colour = "black") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = seq(15, 75, by = 5)) +
  theme_minimal() +
  ggeasy::easy_remove_legend()

# Procrastination
data %>%
  ggplot(aes(Total_PPS)) +
  geom_histogram(binwidth = 5, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  labs(x = "Total Procrastination", y = "Count") +
  theme_minimal()

data %>%
  ggplot(aes(SLEPrime)) +
  geom_histogram(binwidth = 5, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(70, 100, by = 5)) +
  labs(x = "Subjective Life Expectency (Prime)", y = "Count") +
  theme_minimal()

data %>%
  ggplot(aes(Subjectivelifeexpectancy)) +
  geom_histogram(binwidth = 5, fill = "salmon", colour = "black") +
  scale_x_continuous(breaks = seq(50, 105, by = 5)) +
  labs(x = "Subjective Life Expectency", y = "Count") +
  theme_minimal()


data %>%
  ggplot(aes(GeneralHealth)) +
  geom_bar(aes(fill = GeneralHealth), colour = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon", "lightyellow", "lavender")) +
  labs(title = "General Health Assessment", x = "", y = "Count") +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend()


data %>%
  ggplot(aes(Total_CFCImmediate)) +
  geom_histogram(binwidth = 2, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(7, 49, by = 7)) +
  labs(title = "Consideration of immediate consequences", x = "CFC-I", y = "count") +
  theme_minimal() +
  ggeasy::easy_center_title()

data %>%
  ggplot(aes(Total_CFCFuture)) +
  geom_histogram(binwidth = 2, fill = "salmon", colour = "black") +
  scale_x_continuous(breaks = seq(7, 49, by = 7)) +
  labs(title = "Consideration of future consequences", x = "CFC-F", y = "count") +
  theme_minimal() +
  ggeasy::easy_center_title()


correlation_results <- data %>%
  select(Total_PPS, Total_CFCFuture, Total_CFCImmediate, 
         SLEPrime, Subjectivelifeexpectancy) %>%
  rename(
    PPS = Total_PPS,
    CFC_F = Total_CFCFuture,
    CFC_I = Total_CFCImmediate,
    SLE_Prime = SLEPrime,
    SLE = Subjectivelifeexpectancy
  ) %>%
  correlation()

summary(correlation_results) %>%
  plot() +
  theme_classic() +
  ggeasy::easy_center_title()

data %>%
  ggplot(aes(x = Total_PPS, y = Total_CFCImmediate)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(7, 49, by = 7)) +
  labs(title = "Procrastination vs. CFC Immediate", x = "Procrastination", y = "CFC-I") +
  theme_minimal() +
  ggeasy::easy_center_title() +
  geom_smooth()

ggstatsplot::ggscatterstats(
  data = data, x = Total_PPS, y = Total_CFCImmediate, 
  xlab = "Total Procrastination", ylab = "CFC-I", 
  title = "Relationship between procrastination and CFC Immediate", 
  bf.message = FALSE  # Not doing Bayesian Stuff 
) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(7, 49, by = 7)) +
  ggeasy::easy_center_title()

ggstatsplot::ggscatterstats(
  data = data, x = Total_PPS, y = Age,
  xlab = "Procrastination", ylab = "Age",
  title = "Relationship between procrastination and age",
  bf.message = FALSE,
) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(15, 75, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

