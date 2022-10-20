
# load packages -----------------------------------------------------------

library(readxl)
library(corrplot)
library(RColorBrewer)


# read files --------------------------------------------------------------

H31_ctrl <- read_excel("data/interplay/matrix_all.xlsx", 
                         sheet = "H31 ctrl matrix", col_names = FALSE)


H31_but <- read_excel("data/interplay/matrix_all.xlsx",
                         sheet = "H31 but matrix", col_names = FALSE)


H32_ctrl <- read_excel("data/interplay/matrix_all.xlsx", 
                       sheet = "H32 ctrl matrix", col_names = FALSE)


H32_but <- read_excel("data/interplay/matrix_all.xlsx", 
                      sheet = "H32 but matrix", col_names = FALSE)


H33_ctrl <- read_excel("data/interplay/matrix_all.xlsx", 
                       sheet = "H33 ctrl matrix", col_names = FALSE)


H33_but <- read_excel("data/interplay/matrix_all.xlsx", 
                      sheet = "H33 but matrix", col_names = FALSE)


# convert df to matrix ----------------------------------------------------

H31_ctrl <- as.matrix(H31_ctrl)
H31_but <- as.matrix(H31_but)

H32_ctrl <- as.matrix(H32_ctrl)
H32_but <- as.matrix(H32_but)

H33_ctrl <- as.matrix(H33_ctrl)
H33_but <- as.matrix(H33_but)




# vector of PTM names for row and col names -------------------------------

groupnames <- c("K4me1",
                "K4me2",
                "K4me3",
                "K4un",
                "K9me1",
                "K9me2",
                "K9me3",
                "K9ac",
                "K9un",
                "K14ac",
                "K14un",
                "K18ac",
                "K18un",
                "K23ac",
                "K23un",
                "K27me1",
                "K27me2",
                "K27me3",
                "K27ac",
                "K27un",
                "K36me1",
                "K36me2",
                "K36me3",
                "K36un"
                )



# H31 interplay plots -----------------------------------------------------

rownames(H31_ctrl) <- groupnames
colnames(H31_ctrl) <- groupnames


pdf(file = "figures/H31_ctrl_interplay.pdf")
corrplot(H31_ctrl,
         method = 'color',
         # col = brewer.pal(n = 11, name = "RdBu"),
         type = "upper",
         na.label = "×"
         )
dev.off()



rownames(H31_but) <- groupnames
colnames(H31_but) <- groupnames


pdf(file = "figures/H31_but_interplay.pdf")
corrplot(H31_but,
         method = 'color',
         # col = brewer.pal(n = 11, name = "RdBu"),
         type = "upper",
         na.label = "×"
         )
dev.off()


# H32 interplay plots -----------------------------------------------------

rownames(H32_ctrl) <- groupnames
colnames(H32_ctrl) <- groupnames


pdf(file = "figures/H32_ctrl_interplay.pdf")
corrplot(H32_ctrl,
         method = 'color',
         # col = brewer.pal(n = 11, name = "RdBu"),
         type = "upper",
         na.label = "×"
         )
dev.off()


rownames(H32_but) <- groupnames
colnames(H32_but) <- groupnames


pdf(file = "figures/H32_but_interplay.pdf")
corrplot(H32_but,
         method = 'color',
         # col = brewer.pal(n = 11, name = "RdBu"),
         type = "upper",
         na.label = "×"
         )
dev.off()


# H33 interplay plots -----------------------------------------------------

rownames(H33_ctrl) <- groupnames
colnames(H33_ctrl) <- groupnames


pdf(file = "figures/H33_ctrl_interplay.pdf")
corrplot(H33_ctrl,
         method = 'color',
         # col = brewer.pal(n = 11, name = "RdBu"),
         type = "upper",
         na.label = "×"
         )
dev.off()


rownames(H33_but) <- groupnames
colnames(H33_but) <- groupnames


pdf(file = "figures/H33_but_interplay")
corrplot(H33_but,
         method = 'color',
         # col = brewer.pal(n = 11, name = "RdBu"),
         type = "upper",
         na.label = "×"
         )
dev.off()

