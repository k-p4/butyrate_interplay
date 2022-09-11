# load required packages
if (!require(readxl)) {install.packages("readxl")}
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(coin)) {install.packages("coin")}


# import files
butyrate_treatment_interplay_9reps <- read_excel("data/butyrate_treatment_interplay_9reps.xlsx", 
                                                 sheet = "Sheet4")

# rename dataframe
df1 <- butyrate_treatment_interplay_9reps


# check if imported correctly
View(df1)


# pivot longer takes col names as a vector to pivot on
df2 <- df1 %>% pivot_longer(cols = c('int_ctrl_1', 'int_ctrl_2', 'int_ctrl_3', 'int_ctrl_4', 'int_ctrl_5', 'int_ctrl_6', 'int_ctrl_7', 'int_ctrl_8', 'int_ctrl_9', 'int_but_1', 'int_but_2', 'int_but_3', 'int_but_4', 'int_but_5', 'int_but_6', 'int_but_7', 'int_but_8', 'int_but_9'),
                            names_to = 'sample_name',
                            values_to = 'interplay_score')


# use str_extract to create sample_name col. 
# regex parses based on underscore - hardcoded to expect int_condition_biorep format
df3 <- df2 %>% mutate(bio_rep = str_extract(sample_name, "([^_]+)_[^_]+$"))


# use str_extract to create bio_rep col. regex drops based to underscore delim - hardcoded as above
df4 <- df3 %>% mutate(treatment = str_extract(bio_rep, "^[^_]+"))


# clean up df for independence tests
df5 <- df4 %>% select(ptm_combination, bio_rep, treatment, interplay_score, -c(sample_name))


View(df5)


# hypothesis testing

# library(dplyr)
# library(broom)
# 
# df <- data %>% 
#         group_by(Product_type) %>% 
#         do(tidy(t.test(.$Price_Online, 
#                        .$Price_Offline, 
#                        mu = 0, 
#                        alt = "two.sided", 
#                        paired = TRUE, 
#                        conf.level = 0.99))))
# 
# library(broom)
# library(dplyr)
# library(purrr)
# library(tidyr)
# 
# t_test <- function(df, mu = 0, alt = "two.sided", paired = T, conf.level = .99) {
#         tidy(t.test(df$Price_Offline, 
#                     df$Price_Online,
#                     mu = mu, 
#                     alt = alt,
#                     paired = paired,
#                     conf.level = conf.level))
# }
# 
# d <- df %>%
#         group_by(Product_type) %>%
#         nest() %>%
#         mutate(ttest = map(data, t_test)) %>%
#         unnest(ttest, .drop = T)
# 
# 
# 
# result <- by(data, data$Product_type, function(x) 
#         t.test(x$Price_Online, x$Price_Offline, mu=0, alt="two.sided", 
#                paired=TRUE, conf.level=0.99)[c(1:9)])