# load libraries
library(readxl)
library(tidyverse)
library(corrplot)


# import h32 data cleaned in excel
interplay1_1 <- read_excel("data/H32_interplay_matrices.xlsx", 
                           sheet = "Sheet1", col_names = FALSE)
View(interplay1_1)


interplay1_2 <- read_excel("data/H32_interplay_matrices.xlsx", 
                           sheet = "Sheet2", col_names = FALSE)
View(interplay1_2)


interplay1_3 <- read_excel("data/H32_interplay_matrices.xlsx", 
                           sheet = "Sheet3", col_names = FALSE)
View(interplay1_3)


interplay2_1 <- read_excel("data/H32_interplay_matrices.xlsx", 
                           sheet = "Sheet4", col_names = FALSE)
View(interplay2_1)


interplay2_2 <- read_excel("data/H32_interplay_matrices.xlsx", 
                           sheet = "Sheet5", col_names = FALSE)
View(interplay2_2)


interplay2_3 <- read_excel("data/H32_interplay_matrices.xlsx", 
                           sheet = "Sheet6", col_names = FALSE)
View(interplay2_3)



# convert df to matrix for corrplot input
h32_interplay1_1 <- as.matrix(interplay1_1)

h32_interplay1_2 <- as.matrix(interplay1_2)

h32_interplay1_3 <- as.matrix(interplay1_3)

h32_interplay2_1 <- as.matrix(interplay2_1)

h32_interplay2_2 <- as.matrix(interplay2_2)

h32_interplay2_3 <- as.matrix(interplay2_3)



# names for rows and columns
groupnames <- c("K4me1", "K4me2", "K4me3", "K4un", "K9me1", "K9me2", "K9me3", "K9ac", "K9un", "K14ac", "K14un", "K18ac", "K18un", "K23ac",  "K23un", "K27me1", "K27me2", "K27me3", "K27ac", "K27un", "K36me1", "K36me2", "K36me3", "K36un")
row.names(h32_interplay1_1) <- groupnames
colnames(h32_interplay1_1) <- groupnames

row.names(h32_interplay1_2) <- groupnames
colnames(h32_interplay1_2) <- groupnames

row.names(h32_interplay1_3) <- groupnames
colnames(h32_interplay1_3) <- groupnames

row.names(h32_interplay2_1) <- groupnames
colnames(h32_interplay2_1) <- groupnames

row.names(h32_interplay2_2) <- groupnames
colnames(h32_interplay2_2) <- groupnames

row.names(h32_interplay2_3) <- groupnames
colnames(h32_interplay2_3) <- groupnames



# corrplot for all loaded matrices

# plot corrplot for h32_interplay1_1
corrplot(
        h32_interplay1_1, # specify matrix
         method = 'color' , # shape in each cell
         type = 'lower', # plot only lower half
         diag = FALSE, # remove diagonal with perfect correlation 
         col = COL2('RdBu', 20), # color scheme
         addCoef.col = "dark grey", # font color
         number.cex = 0.50) # text size


# plot corrplot for h32_interplay1_2
corrplot(h32_interplay1_2, # specify matrix
         method = 'color' , # shape in each cell
         type = 'lower', # plot only lower half
         diag = FALSE, # remove diagonal with perfect correlation 
         col = COL2('RdBu', 20), # color scheme
         addCoef.col = "dark grey", # font color
         number.cex = 0.50) # text size


# plot corrplot for h32_interplay1_3
corrplot(h32_interplay1_3, # specify matrix
         method = 'color' , # shape in each cell
         type = 'lower', # plot only lower half
         diag = FALSE, # remove diagonal with perfect correlation 
         col = COL2('RdBu', 20), # color scheme
         addCoef.col = "dark grey", # font color
         number.cex = 0.50) # text size


# plot corrplot for h32_interplay2_1
corrplot(h32_interplay2_1, # specify matrix
         method = 'color' , # shape in each cell
         type = 'lower', # plot only lower half
         diag = FALSE, # remove diagonal with perfect correlation 
         col = COL2('RdBu', 20), # color scheme
         addCoef.col = "dark grey", # font color
         number.cex = 0.50) # text size


# plot corrplot for h32_interplay2_2
corrplot(h32_interplay2_2, # specify matrix
         method = 'color' , # shape in each cell
         type = 'lower', # plot only lower half
         diag = FALSE, # remove diagonal with perfect correlation 
         col = COL2('RdBu', 20), # color scheme
         addCoef.col = "dark grey", # font color
         number.cex = 0.50) # text size


# plot corrplot for h32_interplay2_3
corrplot(h32_interplay2_3, # specify matrix
         method = 'color' , # shape in each cell
         type = 'lower', # plot only lower half
         diag = FALSE, # remove diagonal with perfect correlation 
         col = COL2('RdBu', 20), # color scheme
         addCoef.col = "dark grey", # font color
         number.cex = 0.50) # text size
