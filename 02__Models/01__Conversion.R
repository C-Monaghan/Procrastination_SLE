# Converting .sav file to a .xlsx file
rm(list = ls())

data <- haven::read_sav(file.path("01__Data/01__raw_data.sav"))

writexl::write_xlsx(x = data, path = file.path("01__Data/01__raw_data.xlsx"))


