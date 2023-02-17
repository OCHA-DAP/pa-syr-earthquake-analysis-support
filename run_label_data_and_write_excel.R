library(tidyverse)
library(janitor)
library(readxl)
library(googlesheets4)
library(glue)
ks <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "survey")
kc <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "choices")



dat_fp <- file.path(Sys.getenv("SYR_EQUAKE_DIR"),"kobo_data_incoming","Syria_earthquake_-_shelters_multi-sectoral_assessment_-_latest_version_-_False_-_2023-02-17-07-19-17.xlsx")
dat <- read_excel(dat_fp)

dat %>% select(ends_with("tot\\d"))
# label data
dat_labelled <- label_dataset(dat, ks = ks, kc = kc) %>% 
    clean_up_composite()


file_name_end<- Sys.Date() %>% 
    str_remove_all("-")
output_filename <- paste0("syr_equake_assessment_data_clean",file_name_end,".xlsx")
write.xlsx(x = dat_labelled,file = output_filename)
