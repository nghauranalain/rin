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
        filter(Earliest.patent.priority.year %in% 2002:2013) %>%
        group_by(Earliest.patent.priority.year) %>%
        summarise(nb.patents = n_distinct(pubnum))
# new variable: number of inventors per patent
open.data.3 <- open.data.3 %>% group_by(pubnum) %>% mutate(nb.inv = n())
# number of patents per departments
glimpse(open.data.3)
#open.data.3.stats <- open.data.3 %>%
        #filter(Earliest.patent.priority.year %in% 2001:2013) %>%
        #group_by(`Département.de.l'inventeur`, Earliest.patent.priority.year) %>%
        #summarise(nb.patents = n_distinct(pubnum),
         #         nb.patents.one.inv = n_distinct(pubnum[nb.inv == 1]),
          #        nb.patents.many.inv = n_distinct(pubnum[nb.inv != 1]))
#xlsx::write.xlsx(as.data.frame(open.data.3.stats), file = "../patents.xlsx",
                 #row.names = FALSE) #export

#### Keep relevant patent based on lag 
## treatment intensity data
df.tr <- read.csv("T:/These_GATE/Paper_2/Treatment/treatment_intensity.csv",
                  header = T)

## IMPORTANT for robustness tests
tr.years <- sort(unique(df.tr$year), decreasing = F)
l <- 3 # lag
net.years <- sort(unique(df.tr$year) + l, decreasing = F)
r <- 3 # range
Hmisc::cut2(tr.years, m = r) # groups of tr.years based on range
Hmisc::cut2(net.years, m = r) # groups of net.years based on range

## create group variable (period) based of net.years
as.data.frame(cbind(net.years, period = Hmisc::cut2(net.years, m = r)))
open.data.4 <- left_join(open.data.3,
                        as.data.frame(cbind(net.years, period = Hmisc::cut2(net.years, m = r))),
                        by = c("Earliest.patent.priority.year" = "net.years"))
## rm non relevant period NA
open.data.4 <- filter(open.data.4, !is.na(period))
table(open.data.4$Earliest.patent.priority.year)
table(open.data.4$period)


#### Create network data for each department, for each period 
glimpse(open.data.4)
# define patent group
open.data.4$pat.grp <- paste(open.data.4$`Département.de.l'inventeur`,
                             open.data.4$period, sep = "_")
## rm non relevant dep
unique(open.data.4$`Département.de.l'inventeur`)
deps <- read.delim("./data/depts2013.txt", header = T) # departement codes
regs <- read.delim("./data/reg2013.txt", header = T) # region codes
glimpse(deps)
glimpse(regs)
deps <- deps %>% select(DEP, NCCENR, REGION)
regs <- regs %>% select(REGION, NCCENR2 = NCCENR)

open.data.4 <- left_join(open.data.4, deps,
                         by = c("Département.de.l'inventeur" = "NCCENR"))
open.data.4 <- left_join(open.data.4, regs, by = "REGION")

glimpse(open.data.4)
open.data.4$DEP <- as.character(open.data.4$DEP)
sort(unique(open.data.4$DEP))
open.data.4 <- open.data.4 %>% filter(nchar(DEP) == 2
                                      & !DEP %in% c("2A", "2B"))
n_distinct(open.data.4$DEP)
n_distinct(open.data.4$NCCENR2)

## rename columns
colnames(open.data.4) <- c("pubnum", "id_inv", "dep_inv", "priority_year",
                           "CIB_domains", "nb.inv", "period", "pat.grp",
                           "dep_inv_code", "reg_inv_code", "reg_inv")
# change col order
open.data.4 <- open.data.4[, c("pubnum", "priority_year", "id_inv", "dep_inv",
                               "dep_inv_code", "reg_inv", "reg_inv_code",
                               "nb.inv", "CIB_domains", "period", "pat.grp")]

## get unique pat.grp
groups <- sort(unique(open.data.4$pat.grp))

## create and save 376 csv files. Each csv represent the network data for each department
## and for each period
my.list <- list()
for (i in seq_along(groups)) {
        my.list[[i]] <- filter(open.data.4, pubnum %in% pubnum[pat.grp == groups[i]])
        write.csv(my.list[[i]], file =  paste0("../", groups[i], ".csv"), row.names = F)
}












