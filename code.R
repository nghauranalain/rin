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
                       colNames = T)
glimpse(open.data)
open.data.2 <- select(open.data, pubnum,
                      `Identifiant.interne.de.l'inventeur`,
                      `Département.de.l'inventeur`)
head(open.data.2)

#### reshape open.data.2
## the data is incomplete
sum(stringr::str_count(open.data.2$`Identifiant.interne.de.l'inventeur`, ","), na.rm = T)
sum(stringr::str_count(open.data.2$`Département.de.l'inventeur`, ","), na.rm = T)
## subset the data to keep complete rows
open.data.2$nb.id.inv <- stringr::str_count(open.data.2$`Identifiant.interne.de.l'inventeur`, ",") + 1
open.data.2$nb.dep.inv <- stringr::str_count(open.data.2$`Département.de.l'inventeur`, ",") + 1
# complete open.data.2
open.data.2.complete <- filter(open.data.2, nb.id.inv == nb.dep.inv)
# reshaped version of open.data.2.complete
open.data.3 <- separate_rows(select(open.data.2.complete, -nb.id.inv, -nb.dep.inv),
                             `Identifiant.interne.de.l'inventeur`,
                             `Département.de.l'inventeur`, sep = ",") # 307342 rows


#### Add "région inventeur" to open.data.3



#### Add "date de priorité" & "sous domaine techno" to open.data.3
colnames(open.data)
open.data.3 <- left_join(open.data.3, select(open.data, pubnum,
                                             Earliest.patent.priority.year,
                                             Sous.domaine.technologique.de.la.CIB.simplifiée),
                         by = "pubnum")
str(open.data.3)
open.data.3$Earliest.patent.priority.year <- as.numeric(open.data.3$Earliest.patent.priority.year)

#### Descriptive stats
# number of patents per year
open.data.3 %>%
        filter(Earliest.patent.priority.year %in% 2001:2013) %>%
        group_by(Earliest.patent.priority.year) %>%
        summarise(nb.patents = n_distinct(pubnum))
# new variable: number of inventors per patent
open.data.3 <- open.data.3 %>% group_by(pubnum) %>% mutate(nb.inv = n())
# number of patents per departments
glimpse(open.data.3)
open.data.3.stats <- open.data.3 %>%
        filter(Earliest.patent.priority.year %in% 2001:2013) %>%
        group_by(`Département.de.l'inventeur`, Earliest.patent.priority.year) %>%
        summarise(nb.patents = n_distinct(pubnum),
                  nb.patents.one.inv = n_distinct(pubnum[nb.inv == 1]),
                  nb.patents.many.inv = n_distinct(pubnum[nb.inv != 1]))
#xlsx::write.xlsx(as.data.frame(open.data.3.stats), file = "../patents.xlsx",
                 #row.names = FALSE) #export

#### Keep patents from 2001 to 2013 only
open.data.3 <- filter(open.data.3, Earliest.patent.priority.year %in% 2001:2013)

#### Create network data for each department, for each year 
glimpse(open.data.3)
# define patent group
open.data.3$pat.grp <- paste(open.data.3$`Département.de.l'inventeur`,
                             open.data.3$Earliest.patent.priority.year, sep = "_")
# get unique pat.grp
groups <- sort(unique(open.data.3$pat.grp))
# create and save 1338 csv files. Each csv represent the network data for each department
# and for each year (if applicable)
#my.list <- list()
#for (i in seq_along(groups)) {
#        my.list[[i]] <- filter(open.data.3, pubnum %in% pubnum[pat.cat == groups[i]])
#        write.csv(my.list[[i]], file =  paste0("../", groups[i], ".csv"), row.names = F)
#}












