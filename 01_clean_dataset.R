library(tidyverse)
library(janitor)
library(readxl)
library(googlesheets4)
library(glue)
library(googleLanguageR)

ks <- read_sheet(ss = Sys.getenv("SYR_EQUAKE_GS_URL"),sheet = "survey")

dat_fp <- file.path(
    Sys.getenv("SYR_EQUAKE_DIR"),
    "kobo_data_incoming",
    "Syria_earthquake_-_shelters_multi-sectoral_assessment_-_latest_version_-_False_-_2023-02-20-16-00-44.xlsx"
)

dat <- read_excel(dat_fp)

##########################
#### CLEANING COLUMNS ####
##########################

# get group names for removal
group_names <- ks %>%
    filter(
        type == "begin_group"
    ) %>%
    pull(
        name
    ) %>%
    unique() %>%
    paste0("/", collapse = "|")

# get names for conversion

ks_names <- ks %>%
    filter(
        !is.na(name_new) & !is.na(name),
        !str_detect(name_new, "eng$")
    )

# create named vector for use in str_replace_all
reps <- ks_names$name_new
names(reps) <- ks_names$name

# simply pivot all data longer and use str_replace_all to rename vectors
# then re-pivot everything to wide data and re-convert
df_clean <- dat %>%
    rename_with(
        .fn = ~ str_remove_all(.x, group_names)
    ) %>%
    mutate(
        id = row_number(),
        across(
            .cols = everything(),
            .fns = as.character
        )
    ) %>%
    pivot_longer(
        cols = -id
    ) %>%
    mutate(
        name = str_replace_all(
            name,
            rev(reps) # reverse so substrings don't create problems
        )
    ) %>%
    pivot_wider() %>%
    type_convert() %>%
    select(
        matches(paste(reps, collapse = "|")),
        -ends_with("sm")
    )

######################
#### TRANSLATIONS ####
######################

# all columns ending in txt are translated from arabic to english
df_translated <- df_clean %>%
    mutate(
        across(
            ends_with("txt"),
            ~ sapply(
                .x,
                \(y) if (is.na(y)) {
                    NA_character_
                } else {
                    gl_translate(y, target = "en", source = "ar") %>%
                        pull(translatedText)
                }
            ),
            .names = "{.col}_eng"
        )
    )

# re-arrange so _eng columns are just after their original
for (col in names(df_translated)[str_ends(names(df_translated), "eng")]) {
    df_translated <- relocate(df_translated, !!col, .after = str_remove(col, "_eng"))
}

############################
#### HH BEST ESTIMATES  ####
############################

# create best estimate as the most likely number
# other it's the mean of max/min where available or the sole available
# number when there.
# Also get total individuals as a sum.
df_ests <- df_translated %>%
    mutate(
        max_hh_num = as.numeric(max_hh_txt),
        .after = max_hh_txt
    ) %>%
    mutate(
        across(
            .cols = matches("total_(.*)_num$"),
            .fns = as.numeric
        ),
        hh_best_estimate_num = ifelse(
            !is.na(likely_hh_num),
            likely_hh_num,
            ceiling(rowMeans(select(., min_hh_num, max_hh_num), na.rm = TRUE))
        ),
        .after = max_hh_num
    ) %>%
    mutate(
        total_indiv = rowSums(select(., matches("total_(.*)_num$")), na.rm = TRUE),
        .after = total_elders_60_num
    )

########################
#### OUTPUT DATASET ####
########################

output_fn <- paste0(
    "syr_equake_assessment_data_clean",
    Sys.Date(),
    ".csv"
)

write_excel_csv(
    x = df_ests,
    file = file.path(
        Sys.getenv("SYR_EQUAKE_DIR"),
        "clean_data",
        output_fn
    ),
    na = ""
)
