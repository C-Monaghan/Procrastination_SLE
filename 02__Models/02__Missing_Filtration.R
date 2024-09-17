# Missing data analysis
# Can I keep some participants??

rm(list = ls())

library(dplyr)
library(naniar)
library(ggplot2)
library(patchwork)

# Reading in data
data_raw <- readxl::read_xlsx(file.path("01__Data/01__raw_data.xlsx"))

# Missing data as a whole ------------------------------------------------------
miss_raw <- data_raw %>%
  dplyr::select(Sex:CFC12IRev) %>%
  visdat::vis_dat(palette = "cb_safe") +
  labs(title = "Missing data (N = 229)") +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_x_axis_labels_size(size = 7)

# I have removed particpants with high amounts of missing data in the subscales
# but kept those with low amounts of missing data
data_filter <- data_raw %>%
  filter(rowSums(!is.na(dplyr::select(., CFC9:CFC14))) > 0) %>%
  filter(rowSums(!is.na(dplyr::select(., PPS1:PPS12))) > 0) # 143 participants

# Visualising filtering dataset
miss_filter <- data_filter %>%
  dplyr::select(Sex:CFC12IRev) %>%
  visdat::vis_dat(palette = "cb_safe") +
  labs(title = "Missing data (N = 143)") +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_x_axis_labels_size(size = 7)

# Plotting with patchwork
patch_1 <- (miss_raw + miss_filter)

# Exporting --------------------------------------------------------------------
export_data <- "01__Data/"
export_graphics <- "02__Models/Results/"

# Filtered dataset
writexl::write_xlsx(x = data_filter, path = file.path("01__Data/02__data_filtered.xlsx"))

# Plot
cowplot::save_plot(
  filename = file.path(export_graphics, "01__Missing_data.png"),
  plot = patch_1,
  base_height = 10)
