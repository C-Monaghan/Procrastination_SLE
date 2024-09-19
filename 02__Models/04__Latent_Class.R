# Latent Class Analysis --------------------------------------------------------
rm(list = ls())

library(dplyr)
library(ggplot2)
library(patchwork)

library(poLCA) # For LCA
library(lavaan) # For SEM

# Functions --------------------------------------------------------------------
# Collapsing likert scale
likert_collapse <- function(x) {
  ifelse(x %in% c(1, 2, 3), 1, 
         ifelse(x == 4, 2, 3))
}

# Calculating entropy 
entropy <- function(posterior, k) {
  a <- sum(posterior * log(posterior), na.rm = TRUE) 
  b <- 140 * log(k)
  
  1 - (a / b)
}

# Making classification plot
create_classification_plot <- function(data, class_num, prefix, title) {
  data %>%
    filter(Class == class_num) %>%
    dplyr::select(starts_with(prefix)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "var",
                        values_to = "val") %>%
    mutate(
      val = case_when(
        val == 1 ~ "Low",
        val == 2 ~ "Neutral",
        val == 3 ~ "High"),
      val = factor(val, levels = c("Low", "Neutral", "High")),
      var = as.factor(var)
    ) %>%
    filter(!is.na(val)) %>%
    ggplot(aes(val)) +
    geom_bar(aes(fill = var), colour = "black", position = "dodge", width = 0.75) +
    labs(title = title, x = "", y = "") +
    ylim(0, 60) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3") +
    ggeasy::easy_center_title() +
    ggeasy::easy_remove_legend()
}

# Reading in data
data <- readxl::read_xlsx(file.path("01__Data/02__data_filtered.xlsx"))

# Processing
data <- data %>%
  # Removing 3 participants
  filter(ChronicIllness %in% c(1, 2) & Sex != 3) %>%
  # Collapsing likert scale
  mutate(across(c(CFC1:CFC14), likert_collapse)) %>%
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
  )
  
# Setting up data for LCA ------------------------------------------------------
data_polca <- data %>%
  # Creating factor variables
  mutate(
    # Sex coded as 0 & 1
    Sex = factor(Sex - 1, levels = c(0, 1)),
    
    # Chronic illness coded as 0 & 1
    ChronicIllness = factor(ChronicIllness - 1, levels = c(0, 1)),
    
    # General health coded from 1 - 5
    GeneralHealth = as.factor(GeneralHealth),
    
    # Age groups coded from 1 - 3
    age_group = case_when(
      age_group == "18-24" ~ 1,
      age_group == "25-44" ~ 2,
      age_group == "45+" ~ 3),
    age_group = as.factor(age_group)
  ) %>%
  # Only necessary variables
  dplyr::select(starts_with("Future_"), starts_with("Immediate_"), Sex, GeneralHealth, ChronicIllness, age_group)

# Setting up LCA ---------------------------------------------------------------
# Creating model formula
f <- cbind(
  Future_1, Future_2, Future_3, Future_4, Future_5, Future_6, Future_7,
  Immediate_1, Immediate_2, Immediate_3, Immediate_4, Immediate_5, Immediate_6, 
  Immediate_7) ~ Sex + GeneralHealth + ChronicIllness + age_group

# Running 5 models with k-number of clusters and assessing based on AIC & BIC
# for(k in 1:5){
#   model <- poLCA(
#     formula = f, data = data_polca, nclass = k,
#     maxiter = 3000, nrep = 100, na.rm = TRUE,
#     graphs = FALSE, verbose = FALSE)
#   
#   # Calculating entropy
#   entropy_val <- round(entropy(posterior = model$posterior, k = k), digits = 3)
#   
#   cat("For R =", k, "AIC =", model$aic, "BIC =", model$bic, "Entropy =", entropy_val,  "\n")
# }

# 2 class model provides the best results
lca_model <- poLCA(
  formula = f, data = data_polca, nclass = 2,
  maxiter = 3000, nrep = 100, na.rm = FALSE, graphs = TRUE)

# Adding class predictions
data_classification <- data %>%
  mutate(Class = lca_model$predclass - 1) # Adding predicted class (converting to 0 - 1 format)

# Plotting ---------------------------------------------------------------------
classification_plots <- list()

# Group 1 ----------------------------------------------------------------------
classification_plots[[1]] <- create_classification_plot(
  data_classification, 0, "Future_", 
  "Consideration of Future Consequences (Group 1)")

classification_plots[[2]] <- create_classification_plot(
  data_classification, 0, "Immediate_", 
  "Consideration of Immediate Consequences (Group 1)")

# Group 2 ----------------------------------------------------------------------
classification_plots[[3]] <- create_classification_plot(
  data_classification, 1, "Future_", 
  "Consideration of Future Consequences (Group 2)")

classification_plots[[4]] <- create_classification_plot(
  data_classification, 1, "Immediate_", 
  "Consideration of Immediate Consequences (Group 2)")

# Patchwork --------------------------------------------------------------------
(classification_plots[[1]] + classification_plots[[2]]) / (classification_plots[[3]] + classification_plots[[4]])

# SEM Analysis -----------------------------------------------------------------
# Preparing data with interaction effect
data_interaction <- data_classification %>%
  mutate(
    # Recalculating with new scoring system
    Total_CFCI = rowSums(across(c(Immediate_1:Immediate_7)), na.rm = TRUE),
    Total_CFC = rowSums(across(c(Future_1:Future_7)), na.rm = TRUE),
    
    # Scaling
    Total_CFCF = scale(Total_CFCF),
    Total_CFCI = scale(Total_CFCI),
    SLE = scale(SLE),
     
    # Creating interaction effect
    FCon_SLE = Total_CFCF * SLE,
    ICon_SLE = Total_CFCI * SLE
    )

# No moderation effect
model_1 <- '
  # LATENT VARIABLES
  FCon =~ Future_1 + Future_2 + Future_3 + Future_4 + Future_5 + Future_6 + Future_7;
  ICon =~ Immediate_1 + Immediate_2 + Immediate_3 + Immediate_4 + Immediate_5 + Immediate_6 + Immediate_7;
  Proc =~ PPS1 + PPS2 + PPS3 + PPS4 + PPS5 + PPS6 + PPS7 + PPS8 + PPS9 + PPS10 + PPS11 + PPS12;
  
  # RESIDUALS
  FCon ~~ ICon;
  
  # STRUCTURAL MODEL
  Proc ~ b1*FCon + b2*ICon + b3*SLE + c*Class;
'

# Moderation effect
model_2 <- '
  # LATENT VARIABLES
  FCon =~ Future_1 + Future_2 + Future_3 + Future_4 + Future_5 + Future_6 + Future_7;
  ICon =~ Immediate_1 + Immediate_2 + Immediate_3 + Immediate_4 + Immediate_5 + Immediate_6 + Immediate_7;
  Proc =~ PPS1 + PPS2 + PPS3 + PPS4 + PPS5 + PPS6 + PPS7 + PPS8 + PPS9 + PPS10 + PPS11 + PPS12; 
  
    # RESIDUALS
  FCon ~~ ICon;
  
  # STRUCTURAL MODEL
  Proc ~ b1*FCon + b2*ICon + b3*SLE + m1*FCon_SLE + m2*ICon_SLE + Class;
'

fit_1 <- sem(model = model_1, data = data_classification, estimator = "ML", missing = "fiml")
fit_2 <- sem(model = model_2, data = data_interaction, estimator = "ML", missing = "fiml")

summary(fit_1, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)
summary(fit_2, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)


fit_1_results <- broom::tidy(fit_1)

View(fit_1_results)

fit_1_results %>%
  filter(label != "")
