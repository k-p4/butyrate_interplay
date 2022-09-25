# set seed

set.seed(24601)

# load required packages
if (!require(readxl)) {install.packages("readxl")}
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(broom)) {install.packages("broom")}
if (!require(coin)) {install.packages("coin")}


# import files
butyrate_treatment_interplay_9reps <- read_excel("data/butyrate_treatment_interplay_9reps.xlsx", 
                                                 sheet = "Sheet9")

# rename dataframe
df_aci_1 <- butyrate_treatment_interplay_9reps


# check if imported correctly
df_aci_1


# pivot longer takes col names as a vector to pivot on
df_aci_2 <- df_aci_1 %>% 
        pivot_longer(cols = c('aci_ctrl_1', 'aci_ctrl_2', 'aci_ctrl_3', 'aci_ctrl_4', 'aci_ctrl_5', 'aci_ctrl_6', 'aci_ctrl_7', 'aci_ctrl_8', 'aci_ctrl_9', 'aci_but_1', 'aci_but_2', 'aci_but_3', 'aci_but_4', 'aci_but_5', 'aci_but_6', 'aci_but_7', 'aci_but_8', 'aci_but_9'),
                     names_to = 'sample_name',
                     values_to = 'interplay_score')


# use str_extract to create sample_name col. 
# regex parses based on underscore - hardcoded to expect int_condition_biorep format
df_aci_3 <- df_aci_2 %>% 
        mutate(bio_rep = str_extract(sample_name, "([^_]+)_[^_]+$"))


# use str_extract to create bio_rep col. regex drops based to underscore delim - hardcoded as above
df_aci_4 <- df_aci_3 %>% 
        mutate(treatment = str_extract(bio_rep, "^[^_]+"))


df_aci_5 <- df_aci_4 %>% 
        select(ptm_combination, bio_rep, treatment, interplay_score, -c(sample_name))

# clean up df_aci_ for independence tests
df_aci_6 <- df_aci_5 %>% 
        select(interplay_score, ptm_combination, treatment)

# drop rows containing K4me3K27un in this data set as err: data are ess constant
df_aci_7 <- df_aci_6 %>%
        filter(!grepl('K4me3K27un', ptm_combination))

# KS test for distributions and normality
df_aci_8 <- df_aci_7 %>%
        group_by(ptm_combination, treatment) %>% 
        nest() %>% 
        pivot_wider(names_from = treatment, values_from = data) %>% 
        # depreciated: spread(key = treatment, value = data) %>% 
        mutate(
                ks_test = map2(ctrl, but, ~{ks.test(.x$interplay_score, .y$interplay_score) %>% tidy()}),
                ctrl = map(ctrl, nrow),
                but = map(but, nrow)
        ) %>%
        unnest(cols = c(ctrl, but, ks_test))

# Wilcox test
df_aci_9 <- df_aci_7 %>%
        group_by(ptm_combination, treatment) %>% 
        nest() %>% 
        pivot_wider(names_from = treatment, values_from = data) %>% 
        # depreciated: spread(key = treatment, value = data) %>% 
        mutate(
                wilcox_test = map2(ctrl, but, ~{wilcox.test(.x$interplay_score, .y$interplay_score) %>% tidy()}),
                ctrl = map(ctrl, nrow),
                but = map(but, nrow)
        ) %>% 
        unnest(cols = c(ctrl, but, wilcox_test))

# Welch t.test
df_aci_10 <- df_aci_7 %>%
        group_by(ptm_combination, treatment) %>% 
        nest() %>% 
        pivot_wider(names_from = treatment, values_from = data) %>% 
        # depreciated: spread(key = treatment, value = data) %>% 
        mutate(
                t_test = map2(ctrl, but, ~{t.test(.x$interplay_score, .y$interplay_score) %>% tidy()}),
                ctrl = map(ctrl, nrow),
                but = map(but, nrow)
        ) %>% 
        unnest(cols = c(ctrl, but, t_test))









