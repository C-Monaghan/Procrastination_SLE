# Converting .sav file to a .xlsx file
rm(list = ls())

data <- haven::read_sav(file.path("Data/01__Data.sav"))

writexl::write_xlsx(x = data, path = file.path("Data/02__Data.xlsx"))


