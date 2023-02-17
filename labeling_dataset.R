
library(tidyverse)
library(janitor)
library(readxl)
library(googlesheets4)
library(glue)

ks <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "survey")
kc <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "choices")

dat_fp <- file.path(Sys.getenv("SYR_EQUAKE_DIR"),"kobo_data_incoming","Syria_earthquake_-_shelters_multi-sectoral_assessment_-_latest_version_-_False_-_2023-02-17-07-19-17.xlsx") 

dat <- read_excel(dat_fp)


# keep only survey options we labelled
ks_critical <- ks %>% 
    filter(!is.na(name_new))

# grab all names
ks_name <- ks_critical %>% 
    pull(name) 

# all sm names
ks_name_sm <- ks_critical %>% 
    filter(str_detect(name_new,"_sm$")) %>% 
    mutate(
        type_match = trimws(str_remove_all(type,"select_multiple|select multiple"))
    )

# all other names from survey
ks_name_rest <- ks_critical %>% 
    filter(!name%in% ks_name_sm$name)

# make sm lookup -- annoyingly rename_with needs the same length vector for alllll arguments
sm_lookup <- kc %>% 
    left_join(ks_name_sm %>% 
                  rename(q_name= "name"), by =c("list_name"="type_match")) %>% 
    filter(!is.na(name_new)) %>% 
    mutate(name_choice= paste0(q_name,"/",name)) %>% 
    select(name,q_name, name_new,name_choice)


ks_name_all_rgx <- glue_collapse(paste0('^',ks_name),"|")

ks_name_rest_rgx <- glue_collapse(paste0('^',ks_name_rest$name),"|")
# ks_name_sm_rgx <- glue_collapse(paste0('^',ks_name_sm$name),"|")
sm_lookup_rgx <- glue_collapse(paste0('^',sm_lookup$q_name),"|")


# realize later this actually grabs too many cols bc the rgx is not perfect - but we fix later
dat_critical <- dat %>%
    select(matches(ks_name_all_rgx))

# reename sm
dat_sm_renamed <- dat_critical %>% 
    rename_with(
        .cols= sm_lookup$name_choice,
                .fn =  ~ str_replace_all(.x, pattern= sm_lookup$q_name, sm_lookup$name_new),
                
    )


# differences in tool and data so have to check that all names from tool are in 
# data and all names from data are in sruvey
col_rest_lookup <- ks_name_rest %>% 
    filter(name %in% colnames(dat_sm_renamed))

col_rest_rgx <- glue_collapse(paste0('^',col_rest_lookup$name,'$'),"|")

cols_rename <-  dat_sm_renamed %>% 
    select(matches(col_rest_rgx)) %>%
    colnames()

# i think this is where we were going to generate the new names, but it was messing up the rename_with so I sliced
cols_lookup <- col_rest_lookup %>% 
    filter(name %in% cols_rename) %>% 
    group_by(name) %>% 
    slice(1) %>% 
    ungroup()
    

# rename the rest
data_all_renamed <- dat_sm_renamed %>% 
    rename_with(
        .cols= cols_lookup$name,
        .fn =  ~ str_replace_all(.x, pattern= cols_lookup$name,cols_lookup$name_new)
        
    )

data_renamed_clean <- data_all_renamed %>% 
    select(matches("*_sm_*|_sm$|so$|_num$|_pct$")) 


