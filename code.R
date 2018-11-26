################################################################################
############# Create regional innovation networks from paten data ##############

#### working directory and options
getwd()
rm(list = ls())
options(scipen = 999)
#### load some packages
library(openxlsx)
library(tidyverse)

#################################  Import data  ################################ 
open.data <- read.xlsx("./data/fr-esr-brevets-france-inpi_FR_VO.xlsx", 1,
                       colNames = TRUE)
glimpse(open.data)
open.data.2 <- select(open.data, pubnum,
                      `Identifiant.interne.de.l'inventeur`,
                      `Département.de.l'inventeur`)
head(open.data.2)
#### reshape open.data.2
## the data is incomplete
sum(stringr::str_count(open.data.2$`Identifiant.interne.de.l'inventeur`, ","), na.rm = TRUE)
sum(stringr::str_count(open.data.2$`Département.de.l'inventeur`, ","), na.rm = TRUE)
## subset the data to keep complete rows
open.data.2$nb.id.inv <- stringr::str_count(open.data.2$`Identifiant.interne.de.l'inventeur`, ",") + 1
open.data.2$nb.dep.inv <- stringr::str_count(open.data.2$`Département.de.l'inventeur`, ",") + 1
# complete open.data.2
open.data.2.complete <- filter(open.data.2, nb.id.inv == nb.dep.inv)
# reshaped version of open.data.2.complete
open.data.3 <- separate_rows(open.data.2.complete, `Identifiant.interne.de.l'inventeur`,
                             `Département.de.l'inventeur`, sep = ",")






df2 <- df %>% Reduce(f = separate_rows_, x = colnames(df)[-1])
