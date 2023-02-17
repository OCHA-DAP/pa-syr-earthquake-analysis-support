 library(tidyverse)
 library(janitor)
 library(readxl)
 library(googlesheets4)
 library(glue)
 library(openxlsx)
 purrr::map(list.files("R/",full.names = T),~source(.x))
 ks <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "survey")
 kc <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "choices")
 dat_fp <- file.path(Sys.getenv("SYR_EQUAKE_DIR"),"kobo_data_incoming","Syria_earthquake_-_shelters_multi-sectoral_assessment_-_latest_version_-_False_-_2023-02-17-07-19-17.xlsx")
 dat <- read_excel(dat_fp)
 
 dat %>% select(ends_with("tot\\d"))
 # label data
 dat_labelled <- label_dataset(dat, ks = ks, kc = kc) %>% 
     clean_up_composite()
 
 dat_labelled %>% 
     glimpse()
 
 
 
 
 
 # For preliminary analysis we are going to pretend data is at shelter level... and
 # at least make it sort of true  group_by()->slice(1)
 # in final analysis would need to aggregate to settlement prior to these calculations
 
 
 
 # select one pcts
 adm3_so_analyzed_na_rm <- dat_labelled %>% 
     select(shelter_name_txt,ends_with("_so")) %>% 
     # not sure what will happen with shelters so we can do something like this:
     
     group_by(admin_1_so,admin_2_so,admin_3_so,shelter_name_txt) %>%
     slice(1) %>% 
     ungroup() %>% 
     select(-all_of(c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt"))) %>% 
     pivot_longer(cols = everything()) %>% 
     # pivot_longer(-c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt")) %>% 
     filter(!is.na(value)) %>% 
     group_by(name,value) %>% 
     summarise(
         n=n(),.groups = "drop_last"
     ) %>% 
     arrange(name,value) %>% 
     mutate(
         pct=n/sum(n)
     ) %>% 
     ungroup() %>% 
     arrange(name,value) %>% 
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
     select(-all_of(c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt"))) %>% 
     pivot_longer(cols = everything()) %>% 
     filter(!is.na(value)) %>% 
     mutate(
         value= fct_expand(as_factor(value),c("1","0"))
     ) %>% 
     group_by(name,value) %>% 
     summarise(
         n=n(),.groups = "drop_last"
     ) %>% 
     group_by(name) %>% 
     mutate(
         pct=n/sum(n)
     ) %>% 
     ungroup() %>% 
     arrange(name,value) %>% 
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
      select(-all_of(c("admin_1_so","admin_2_so","admin_3_so","shelter_name_txt"))) %>% 
      pivot_longer(cols = everything()) %>% 
     filter(!is.na(value)) %>% 
     group_by(name) %>%
     summarise(
         mean=mean(value, na.rm=T),
         median= median(value,na.rm=T),
         sum = sum(value,na.rm=T)
     ) %>% 
     arrange(name) %>% 
      mutate(type="numeric mean")
     
  
  
  # total number of people who have recieved assistance
  
  dat_labelled %>% select(ends_with("_num")) %>% View()
  
 
 