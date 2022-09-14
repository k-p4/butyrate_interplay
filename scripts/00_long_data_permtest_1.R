# load required packages
if (!require(readxl)) {install.packages("readxl")}
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(broom)) {install.packages("broom")}
if (!require(coin)) {install.packages("coin")}


# import files
butyrate_treatment_interplay_9reps <- read_excel("data/butyrate_treatment_interplay_9reps.xlsx", 
                                                 sheet = "Sheet4")

# rename dataframe
df1 <- butyrate_treatment_interplay_9reps


# check if imported correctly
df1


# pivot longer takes col names as a vector to pivot on
df2 <- df1 %>% 
        pivot_longer(cols = c('int_ctrl_1', 'int_ctrl_2', 'int_ctrl_3', 'int_ctrl_4', 'int_ctrl_5', 'int_ctrl_6', 'int_ctrl_7', 'int_ctrl_8', 'int_ctrl_9', 'int_but_1', 'int_but_2', 'int_but_3', 'int_but_4', 'int_but_5', 'int_but_6', 'int_but_7', 'int_but_8', 'int_but_9'),
                            names_to = 'sample_name',
                            values_to = 'interplay_score')


# use str_extract to create sample_name col. 
# regex parses based on underscore - hardcoded to expect int_condition_biorep format
df3 <- df2 %>% 
        mutate(bio_rep = str_extract(sample_name, "([^_]+)_[^_]+$"))


# use str_extract to create bio_rep col. regex drops based to underscore delim - hardcoded as above
df4 <- df3 %>% 
        mutate(treatment = str_extract(bio_rep, "^[^_]+"))


df5 <- df4 %>% 
        select(ptm_combination, bio_rep, treatment, interplay_score, -c(sample_name))

# clean up df for independence tests
df6 <- df5 %>% 
        select(interplay_score, ptm_combination, treatment)

# drop rows containing K4me3K27un in this data set as err: data are ess constant
df7 <- df6 %>%
        filter(!grepl('K4me3K27un', ptm_combination))

# KS test for distributions and normality
df8 <- df7 %>%
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
df9 <- df7 %>%
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
df10 <- df7 %>%
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


# permutation test and writing permissions to be solved


# # coin permutation test of independece
# df11 <- df7 %>%
#         group_by(ptm_combination, treatment) %>% 
#         nest() %>% 
#         pivot_wider(names_from = treatment, values_from = data) %>% 
#         # depreciated: spread(key = treatment, value = data) %>% 
#         mutate(
#                 independence_test = map2(ctrl, but, ~{independence_test(.x$interplay_score ~ .y$interplay_score) %>% tidy()}),
#                 ctrl = map(ctrl, nrow),
#                 but = map(but, nrow)
#         ) %>% 
#         unnest(cols = c(ctrl, but, independence_test))
