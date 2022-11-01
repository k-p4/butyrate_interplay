
# set seed ----------------------------------------------------------------


set.seed(655321)


# load packages -----------------------------------------------------------


if (!require(readxl)) {install.packages("readxl")}
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(broom)) {install.packages("broom")}
if (!require(coin)) {install.packages("coin")}


# import files ------------------------------------------------------------


butyrate_treatment_interplay_9reps <- read_excel("data/butyrate_treatment_interplay_9reps.xlsx", 
                                                 sheet = "Sheet8")


# rename dataframe --------------------------------------------------------


df_ogint_1 <- butyrate_treatment_interplay_9reps


# check import ------------------------------------------------------------


df_ogint_1


# pivot longer takes col names as a vector to pivot on --------------------


df_ogint_2 <- df_ogint_1 %>% 
        pivot_longer(cols = c('ogint_ctrl_1', 'ogint_ctrl_2', 'ogint_ctrl_3', 'ogint_ctrl_4', 'ogint_ctrl_5', 'ogint_ctrl_6', 'ogint_ctrl_7', 'ogint_ctrl_8', 'ogint_ctrl_9', 'ogint_but_1', 'ogint_but_2', 'ogint_but_3', 'ogint_but_4', 'ogint_but_5', 'ogint_but_6', 'ogint_but_7', 'ogint_but_8', 'ogint_but_9'),
                     names_to = 'sample_name',
                     values_to = 'interplay_score')


# cleaning data and adding treatment conditions ---------------------------


# use str_extract to create sample_name col. 
# regex parses based on underscore - hardcoded to expect ogint_condition_biorep format

df_ogint_3 <- df_ogint_2 %>% 
        mutate(bio_rep = str_extract(sample_name, "([^_]+)_[^_]+$"))


# use str_extract to create bio_rep col. regex drops based to underscore delim - hardcoded as above

df_ogint_4 <- df_ogint_3 %>% 
        mutate(treatment = str_extract(bio_rep, "^[^_]+"))


df_ogint_5 <- df_ogint_4 %>% 
        select(ptm_combination, bio_rep, treatment, interplay_score, -c(sample_name))

# clean up df_ogint_ for independence tests

df_ogint_6 <- df_ogint_5 %>% 
        select(interplay_score, ptm_combination, treatment)

# drop rows containing K4me3K27un in this data set as err: data are ess constant

df_ogint_7 <- df_ogint_6 %>%
        filter(!grepl('K4me3K27un', ptm_combination))


# KS test for distributions and normality ---------------------------------


df_ogint_8 <- df_ogint_7 %>%
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


df_ogint_9 <- df_ogint_7 %>%
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


df_ogint_10 <- df_ogint_7 %>%
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
df_ogint_11 <- df_ogint_7 %>%
        group_by(ptm_combination) %>%
        nest() %>% 
        mutate(
                perm_test = map(.x = data, .f = ~pvalue(independence_test(.x$interplay_score ~ as.factor(.x$treatment), data = data)))
        ) %>% 
        unnest(cols = c(ptm_combination, perm_test))

