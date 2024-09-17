# Latent Class Analysis --------------------------------------------------------
rm(list = ls())

library(dplyr)
library(ggplot2)

library(poLCA) # For LCA

# Functions --------------------------------------------------------------------
likert_collapse <- function(x) {
  ifelse(x %in% c(1, 2, 3), 1, 
         ifelse(x == 4, 2, 3))
}

# Reading in data
data <- readxl::read_xlsx(file.path("01__Data/02__data_filtered.xlsx"))

# Processing
data_polca <- data %>%
  mutate(across(c(CFC1:CFC14), likert_collapse)) %>% # Collapsing likert scale
  rename(
    # Future Items
    Future_1 = CFC1,
    Future_2 = CFC2,
    Future_3 = CFC6,
    Future_4 = CFC7,
    Future_5 = CFC8,
    Future_6 = CFC13,
    Future_7 = CFC14,
    
    # Immediate Items
    Immediate_1 = CFC3,
    Immediate_2 = CFC4,
    Immediate_3 = CFC5,
    Immediate_4 = CFC9,
    Immediate_5 = CFC10,
    Immediate_6 = CFC11,
    Immediate_7 = CFC12,
  ) %>%
  mutate(
    # Creating factor variables
    Sex = factor(Sex - 1, levels = c(0, 1)),
    GeneralHealth = as.factor(GeneralHealth),
    ChronicIllness = as.factor(ChronicIllness),
    age_group = case_when(
      age_group == "18-24" ~ 1,
      age_group == "25-44" ~ 2,
      age_group == "45+" ~ 3),
    age_group = as.factor(age_group)
  ) %>%
  # Only necessary variables
  dplyr::select(starts_with("Future_"), starts_with("Immediate_"), Sex, GeneralHealth, ChronicIllness, age_group) %>%
  # Removing 3 participants
  filter(Sex %in% c(0, 1)) %>%
  filter(ChronicIllness %in% c(1, 2))

# Setting up LCA ---------------------------------------------------------------
# Creating model formula
f <- cbind(
  Future_1, Future_2, Future_3, Future_4, Future_5, Future_6, Future_7,
  Immediate_1, Immediate_2, Immediate_3, Immediate_4, Immediate_5, Immediate_6, 
  Immediate_7) ~ 1

# Running 5 models with k-number of clusters and assessing based on BIC
for(k in 1:5){
  model <- poLCA(
    formula = f, data = data_polca, nclass = k,
    maxiter = 3000, nrep = 5, na.rm = TRUE,
    graphs = FALSE, verbose = FALSE)
  
  cat("Number of classes: ", k, " AIC = ", model$aic, " BIC = ", model$bic, "\n")
}

# 2 class model provides the best results
lca_model <- poLCA(
  formula = f, data = data_polca, nclass = 2,
  maxiter = 3000, nrep = 5, na.rm = FALSE, graphs = TRUE)

# Adding class predictions
data_classification <- data_polca %>%
  mutate(Class = lca_model$predclass - 1)
