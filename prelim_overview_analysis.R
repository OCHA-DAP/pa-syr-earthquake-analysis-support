 library(tidyverse)
 library(janitor)
 library(readxl)
 library(googlesheets4)
 library(glue)
 ks <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "survey")
 kc <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "choices")
 dat_fp <- file.path(Sys.getenv("SYR_EQUAKE_DIR"),"kobo_data_incoming","Syria_earthquake_-_shelters_multi-sectoral_assessment_-_latest_version_-_False_-_2023-02-17-07-19-17.xlsx")
 dat <- read_excel(dat_fp)
 dat_labelled <- label_dataset(dat, ks = ks, kc = kc)
 
 
 # select one pcts
 
 adm3_so_analyzed_na_rm <- dat_labelled %>% 
     select(shelter_name_txt,ends_with("_so")) %>% 
     # not sure what will happen with shelters so we can do something like this:
     
     group_by(admin_1_so,admin_2_so,admin_3_so,shelter_name_txt) %>%
     slice(1) %>% 
     ungroup() %>% 
     pivot_longer(-c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt")) %>% 
     filter(!is.na(value)) %>% 
     group_by(admin_1_so,admin_2_so,admin_3_so,name,value) %>% 
     summarise(
         n=n(),.groups = "drop_last"
     ) %>% 
     arrange(admin_1_so,admin_2_so, admin_3_so,name,value) %>% 
     mutate(
         pct=n/sum(n)
     ) %>% 
     ungroup() %>% 
     arrange(admin_1_so,admin_2_so, admin_3_so,name,value) %>% 
     mutate(
         type="select_one"
     )
 
 # select multiples pct
 
 adm3_sm_analyzed_na_rm <- dat_labelled %>% 
     select(matches("^admin_\\d*"),shelter_name_txt,matches("*_sm\\/")) %>% 
     # not sure what will happen with shelters so we can do something like this:
     
     group_by(admin_1_so,admin_2_so,admin_3_so,shelter_name_txt) %>%
     slice(1) %>% 
     ungroup() %>% 
     pivot_longer(-c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt")) %>% 
     filter(!is.na(value)) %>% 
     group_by(admin_1_so,admin_2_so,admin_3_so,name) %>%
     mutate(
         value= fct_expand(as_factor(value),c("1","0"))
     ) %>% 
     group_by(value,.add=T) %>% 
     summarise(
         n=n()
     ) %>% 
     arrange(admin_1_so,admin_2_so, admin_3_so,name,value) %>% 
     group_by(admin_1_so,admin_2_so,admin_3_so,name) %>% 
     mutate(
         pct=n/sum(n)
     ) %>% 
     ungroup() %>% 
     arrange(admin_1_so,admin_2_so, admin_3_so,name,value) %>% 
     mutate(
         type="select_multiple"
     )
 
 # numeric means

  adm3_num_analyzed_na_rm <- dat_labelled %>% 
     select(matches("^admin_\\d*"),shelter_name_txt,ends_with("_num")) %>% 
      mutate(
          across(ends_with("_num"),~as.numeric(.x))
      ) %>% 
     # not sure what will happen with shelters so we can do something like this:
     
     group_by(admin_1_so,admin_2_so,admin_3_so,shelter_name_txt) %>%
     slice(1) %>% 
     ungroup() %>% 
     pivot_longer(-c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt")) %>% 
     filter(!is.na(value)) %>% 
     group_by(admin_1_so,admin_2_so,admin_3_so,name) %>%
     summarise(
         value=mean(value, na.rm=T)
     ) %>% 
     arrange(admin_1_so,admin_2_so, admin_3_so,name,value) %>% 
      mutate(type="numeric mean")
     
  adm3_num_analyzed_na_rm %>% 
      print(n=nrow(.))
 
 