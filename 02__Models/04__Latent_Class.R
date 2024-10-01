# Latent Class Analysis --------------------------------------------------------
rm(list = ls())

set.seed(3462) # Reproducibility

library(dplyr)
library(ggplot2)
library(patchwork)

library(poLCA)          # For LCA
library(LCAplotter)     # For profile plots
library(lavaan)         # For SEM

# Functions --------------------------------------------------------------------
# Collapsing likert scale
likert_collapse <- function(x) {
  ifelse(x %in% c(1, 2, 3), 1, 
         ifelse(x == 4, 2, 3))
}

# Calculating entropy 
entropy <- function(posterior, k) {
  a <- - sum(posterior * log(posterior), na.rm = TRUE) 
  b <- 143 * log(k)
  
  1 - (a / b)
}

# Tidying SEM Output
tidy_output <- function(fit) {
  broom::tidy(fit) %>%
    filter(term %in% c("Proc ~ SRL", "Proc ~ Sex", "Proc ~ ChronicIllness")) %>%
    dplyr::select(!c(op, block)) %>%
    mutate(across(!c(term, group, p.value), \(x) round(x, digits = 2))) %>%
    mutate(p.value = round(p.value, digits = 3))
}

# Reading in data --------------------------------------------------------------
data <- readxl::read_xlsx(file.path("01__Data/02__data_filtered.xlsx"))

# Processing
data <- data %>%
  mutate(
    across(c(CFC1:CFC14), likert_collapse),   # Collapsing likert scale
    Sex = factor(Sex - 1, levels = c(0, 1)),  # Recoding sex
    ChronicIllness = factor(ChronicIllness - 1, levels = c(0, 1)), # Recoding CI
    GeneralHealth = ifelse(GeneralHealth %in% c(4, 5), 0, 1),
    GeneralHealth = as.factor(GeneralHealth),
    
    SRL = SLE - Age
    ) %>%
  rename(
    # Future Items
    F_1 = CFC1,
    F_2 = CFC2,
    F_3 = CFC6,
    F_4 = CFC7,
    F_5 = CFC8,
    F_6 = CFC13,
    F_7 = CFC14,
    
    # Immediate Items
    I_1 = CFC3,
    I_2 = CFC4,
    I_3 = CFC5,
    I_4 = CFC9,
    I_5 = CFC10,
    I_6 = CFC11,
    I_7 = CFC12,
  )

# Setting up data for LCA ------------------------------------------------------
data_polca <- data %>%
  # Only necessary variables
  dplyr::select(starts_with("F_"), starts_with("I_"))

# Setting up LCA ---------------------------------------------------------------
# Creating model formula
f <- cbind(
  F_1, F_2, F_3, F_4, F_5, F_6, F_7,
  I_1, I_2, I_3, I_4, I_5, I_6, 
  I_7) ~ 1

# Using a 2 class model
model <- poLCA(
  formula = f, data = data_polca, nclass = 2,
  maxiter = 3000, nrep = 100, na.rm = FALSE, 
  graphs = TRUE)

# Adding class predictions
data_classification <- data %>%
  mutate(
    Class = model$predclass - 1, # Adding predicted class (converting to 0 - 1 format)
    Class = ifelse(Class == 1, 0, 1))

# Visualizing classes ----------------------------------------------------------
# x axis labels
labels <- c(
  "CFC-F 1", "CFC-F 2", "CFC-F 3", "CFC-F 4", 
  "CFC-F 5", "CFC-F 6", "CFC-F 7",
  "CFC-I 1", "CFC-I 2", "CFC-I 3", "CFC-I 4", 
  "CFC-I 5", "CFC-I 6", "CFC-I 7")

# Creating a profile plot
data_classification %>%
  dplyr::select(starts_with("F_"), starts_with("I_"), Class) %>%
  tidyr::pivot_longer(
    cols = F_1:I_7,
    names_to = "CFC",
    values_to = "Response") %>%
  group_by(Class, CFC) %>%
  summarise(Response = mean(Response, na.rm = TRUE)) %>%
  mutate(Class = as.factor(Class)) %>%
  ggplot(aes(x = CFC, y = Response, colour = Class, group = Class)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = c("sienna1", "hotpink4"), labels = c("Class 1", "Class 2")) +
  scale_x_discrete(labels = labels) +
  labs(title = "Latent Profile Plot of Item Response Probabilities",
       subtitle = "Class 1 membership = 54.9%, Class 2 membership = 45.1%") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(size = 10, angle = 30, vjust = 0.7, hjust = 1),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  ggeasy::easy_move_legend("right") +
  ggeasy::easy_remove_legend_title() +
  ggeasy::easy_plot_legend_size(size = 11)

# SEM Analysis -----------------------------------------------------------------
# Data processing 
data_sem <- data_classification %>%
  mutate(
    SRL = scale(SRL, center = TRUE),
    ChronicIllness = factor(ifelse(ChronicIllness == 0, 1, 0), 
                            levels = c(0, 1))) %>%
  dplyr::select(SRL, Sex, ChronicIllness, Class, starts_with("PPS"))

# Specifying model -------------------------------------------------------------
model_1 <- '
  # LATENT VARIABLES
  Proc =~ PPS1 + PPS2 + PPS3 + PPS4 + PPS5 + PPS6 + PPS7 + PPS8 + PPS9 + PPS10 + PPS11 + PPS12;
  
  # RESIDUALS
  # (Monaghan et al., 2024)
  PPS1 ~~ PPS2;
  PPS10 ~~ PPS11;
  
  # Modification Indices
  PPS9 ~~ PPS10;
  PPS2 ~~ PPS3;
  
  # STRUCTURAL MODEL
  Proc ~ b1*Class + b2*SRL + m1*Class:SRL + c1*Sex + c2*ChronicIllness;
'

fit_1 <- sem(model = model_1, 
             data = data_sem, 
             estimator = "MLR", 
             missing = "fiml.x")

# Fit metrics
broom::glance(fit_1) %>%
  dplyr::select(AIC, BIC, cfi, tli, rmsea, srmr) %>%
  mutate(across(everything(), \(x) round(x, digits = 3))) %>%
  rename(CFI = cfi, TLI = tli, RMSEA = rmsea, SRMR = srmr)

# Model output
summary(fit_1, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)


# Specifying model -------------------------------------------------------------
model_2 <- '
  # LATENT VARIABLES
  Proc =~ PPS1 + PPS2 + PPS3 + PPS4 + PPS5 + PPS6 + PPS7 + PPS8 + PPS9 + PPS10 + PPS11 + PPS12;
  
  # RESIDUALS
  # (Monaghan et al., 2024)
  PPS1 ~~ PPS2;
  PPS10 ~~ PPS11;
  
  # Modification Indices
  PPS9 ~~ PPS10;
  PPS2 ~~ PPS3;
  
  # STRUCTURAL MODEL
  Proc ~ SRL + Sex + ChronicIllness
'

fit_2 <- sem(model = model_2, 
             data = data_sem, 
             estimator = "MLR", 
             missing = "fiml.x", 
             group = "Class")

# Fit metrics
broom::glance(fit_2) %>%
  dplyr::select(AIC, BIC, cfi, tli, rmsea, srmr) %>%
  mutate(across(everything(), \(x) round(x, digits = 3))) %>%
  rename(CFI = cfi, TLI = tli, RMSEA = rmsea, SRMR = srmr)

# Model output
summary(fit_2, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        modindices = FALSE, 
        rsquare = TRUE)



################################################################################
#################################################################################
#################################################################################
#################################################################################
# OLD CODE ---------------------------------------------------------------------
# Likelihood ratio tests
# lca_model_1 <- poLCA(
#   formula = f, data = data_polca, nclass = 1,
#   maxiter = 3000, nrep = 100, na.rm = FALSE, graphs = FALSE)
# 
# lca_model_2 <- poLCA(
#   formula = f, data = data_polca, nclass = 2,
#   maxiter = 3000, nrep = 100, na.rm = FALSE, graphs = FALSE)
# 
# lca_model_3 <- poLCA(
#   formula = f, data = data_polca, nclass = 3,
#   maxiter = 3000, nrep = 100, na.rm = FALSE, graphs = FALSE)
# 
# lca_model_4 <- poLCA(
#   formula = f, data = data_polca, nclass = 4,
#   maxiter = 3000, nrep = 100, na.rm = FALSE, graphs = FALSE)
# 
# # Model 2 vs. Model 1
# tidyLPA::calc_lrt(
#   n = nrow(data_polca), 
#   null_ll = lca_model_1$llik, null_param = lca_model_1$npar, null_classes = 1, 
#   alt_ll = lca_model_2$llik, alt_param = lca_model_2$npar, alt_classes = 2)
# 
# # Model 3 vs. Model 2
# tidyLPA::calc_lrt(
#   n = nrow(data_polca), 
#   null_ll = lca_model_2$llik, null_param = lca_model_2$npar, null_classes = 2, 
#   alt_ll = lca_model_3$llik, alt_param = lca_model_3$npar, alt_classes = 3)
# 
# # Model 4 vs. Model 3
# tidyLPA::calc_lrt(
#   n = nrow(data_polca), 
#   null_ll = lca_model_3$llik, null_param = lca_model_3$npar, null_classes = 3, 
#   alt_ll = lca_model_4$llik, alt_param = lca_model_4$npar, alt_classes = 4)
# 
# 
# # Plotting ---------------------------------------------------------------------
# classification_plots <- list()
# 
# # Group 1 ----------------------------------------------------------------------
# classification_plots[[1]] <- create_classification_plot(
#   data_classification, 0, "F_", 
#   "Consideration of Future Consequences (Group 1)")
# 
# classification_plots[[2]] <- create_classification_plot(
#   data_classification, 0, "I_", 
#   "Consideration of Immediate Consequences (Group 1)")
# 
# # Group 2 ----------------------------------------------------------------------
# classification_plots[[3]] <- create_classification_plot(
#   data_classification, 1, "F_", 
#   "Consideration of Future Consequences (Group 2)")
# 
# classification_plots[[4]] <- create_classification_plot(
#   data_classification, 1, "I_", 
#   "Consideration of Immediate Consequences (Group 2)")
# 
# # Patchwork --------------------------------------------------------------------
# (classification_plots[[1]] + classification_plots[[2]]) / 
#   (classification_plots[[3]] + classification_plots[[4]])


# Running 3 models with k-number of clusters and assessing based on AIC & BIC
# for(k in 1:4){
#   model <- poLCA(
#     formula = f, data = data_polca, nclass = k,
#     maxiter = 3000, nrep = 100, na.rm = FALSE,
#     graphs = TRUE, verbose = FALSE)
# 
#   # AIC and BIC
#   AIC <- round(model$aic, digits = 2)
#   BIC <- round(model$bic, digits = 2)
#   
#   # SS-BIC
#   ss_bic <- -2 * model$llik + log((nrow(data_polca) + 2) / 24) * model$npar
#   
#   # Entropy
#   entropy_val <- round(entropy(posterior = model$posterior, k = k), digits = 3)
#   
#   # Log Likelihood
#   ll <- round(model$llik, digits = 2)
#   
#   # Parameters and DF
#   npar <- model$npar
#   df <- model$resid.df
#   
#   cat("For R =", k, "LL =", ll, "AIC =", AIC, "BIC =", BIC, "SS-BIC =", ss_bic, "Entropy =", entropy_val, "npar = ", npar, "df = ", df, "\n")
# }