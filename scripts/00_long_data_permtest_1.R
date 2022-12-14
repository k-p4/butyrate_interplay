
# set seed ----------------------------------------------------------------


set.seed(655321)


# load packages -----------------------------------------------------------


if (!require(readxl)) {install.packages("readxl")}
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(broom)) {install.packages("broom")}
if (!require(coin)) {install.packages("coin")}


# import files ------------------------------------------------------------


butyrate_treatment_interplay_9reps <- read_excel("data/butyrate_treatment_interplay_9reps.xlsx", 
                                                 sheet = "Sheet4")


# rename dataframe --------------------------------------------------------


df_int_1 <- butyrate_treatment_interplay_9reps


# check import ------------------------------------------------------------


df_int_1


# pivot longer takes col names as a vector to pivot on --------------------


df_int_2 <- df_int_1 %>% 
        pivot_longer(cols = c('int_ctrl_1', 'int_ctrl_2', 'int_ctrl_3', 'int_ctrl_4', 'int_ctrl_5', 'int_ctrl_6', 'int_ctrl_7', 'int_ctrl_8', 'int_ctrl_9', 'int_but_1', 'int_but_2', 'int_but_3', 'int_but_4', 'int_but_5', 'int_but_6', 'int_but_7', 'int_but_8', 'int_but_9'),
                            names_to = 'sample_name',
                            values_to = 'interplay_score')


# cleaning data and adding treatment conditions ---------------------------


# use str_extract to create sample_name col. 
# regex parses based on underscore - hardcoded to expect int_condition_biorep format

df_int_3 <- df_int_2 %>% 
        mutate(bio_rep = str_extract(sample_name, "([^_]+)_[^_]+$"))


# use str_extract to create bio_rep col. regex drops based to underscore delim - hardcoded as above

df_int_4 <- df_int_3 %>% 
        mutate(treatment = str_extract(bio_rep, "^[^_]+"))


df_int_5 <- df_int_4 %>% 
        select(ptm_combination, bio_rep, treatment, interplay_score, -c(sample_name))

# clean up df_int_ for independence tests

df_int_6 <- df_int_5 %>% 
        select(interplay_score, ptm_combination, treatment)

# drop rows containing K4me3K27un in this data set as err: data are ess constant

df_int_7 <- df_int_6 %>%
        filter(!grepl('K4me3K27un', ptm_combination))


# KS test for distributions and normality ---------------------------------


df_int_8 <- df_int_7 %>%
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


# wilcox ranksum test -----------------------------------------------------


df_int_9 <- df_int_7 %>%
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


# welch t test ------------------------------------------------------------


df_int_10 <- df_int_7 %>%
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


# permutation test of independence ----------------------------------------


# No pivot then map permutation test 
df_int_11 <- df_int_7 %>%
        group_by(ptm_combination) %>%
        nest() %>% 
        mutate(
                perm_test = map(.x = data, .f = ~pvalue(independence_test(.x$interplay_score ~ as.factor(.x$treatment), data = data)))
                ) %>% 
        unnest(cols = c(ptm_combination, perm_test))

