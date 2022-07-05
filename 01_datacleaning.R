# script to clean HOVER data and make new variables for analysis
# adopted from HOVER_summary.Rmd file use throughout study enrollment

# load packages----------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(knitr)
library(stringr)
library(viridis)
library(scales)
library(sf)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(tmaptools)
library(OpenStreetMap)
library(REDCapR)
library(readr)
library(lubridate)

# establish colors----------------------------------------------------------------
drc_colors <- c("#007fff", 	"#ce102f", 	"#f7d618")
#hbvcolors <- c('#00429d', '#93003a',"#A0A0A0") #red and blue
hbvcolors <- c('#33374b', '#e2738c',"#b7d1da",'ghostwhite') #navy and pink, and gray
nounprojgraphcol <- c("#4D4D4D","#B2182B")
# colors for responses https://www.color-hex.com/color-palette/1010756 

# load data----------------------------------------------------------------
# API route
data0 <- redcap_read_oneshot(redcap_uri = "https://global.redcap.unc.edu/api/",
                             token = read_file("/Users/camillem/Documents/GitHub/redcap_api/hover_api.txt"))$data

#subset data to all valid IDs
# remove duplicate Menage1 

data1<- data0 %>% 
  dplyr::filter(hrhhid!="HRB -1045" & hrhhid!="HRB -1048" & hrhhid!="HRB -1059") 

# per Patrick, HR2084 enrolled on April 17 remains HRK 2084 and HRK-2084 on April 30 should be HRK-2085
data1$hrhhid <- ifelse(data1$hrhhid=="HRK-2084", "HRK-2085", data1$hrhhid)

data1$hrhhid <- toupper(data1$hrhhid)
# Add maternity center where women originally presented for maternity care
# Binza is in a higher SES part of town than Kingasani.
data1$mat <- substr(data1$hrhhid,3,3)
#table(data1$mat)
data1$maternity <- ifelse(data1$mat=="B","Binza",
                          ifelse(data1$mat=="K","Kingasani",
                                 ifelse(data1$hrhhid=="HR2012","Kingasani",
                                        ifelse(data1$hrhhid=="RB-1047","Binza",
                                               ifelse(data1$hrhhid=="HR2086","Kingasani",
                                                      ifelse(data1$hrhhid=="HR2087","Kingasani",""))))))



# select household entries
hhdata1 <- data1[(is.na(data1$redcap_repeat_instrument)), ]
#hhdata1 <- data1[data1$redcap_repeat_instrument != "questionnaire_individuel", ]
# ^ backup in case is.na() is not working

# for hh dataset, select only hh variables
hhdata1 <- hhdata1 %>% 
  dplyr::select(starts_with("h"), "questionnaire_menage_complete", "maternity")
hhdata1 <- hhdata1 %>% relocate("maternity", .after = "h10_hbv_rdt")
# table(hhdata1$hxcoord, useNA = "always") #check if any non-missing GPS coords of hh are same (duplicate hh)
# save GPS coords as numeric
hhdata1$hxcoord <- as.numeric(hhdata1$hxcoord)
hhdata1$hycoord <- as.numeric(hhdata1$hycoord)

# correct HRB-1028 coordinates using coords from the correct neighborhood (interview took place at BINZA)
hhdata1$hxcoord[hhdata1$hrhhid=="HRB-1028"] <- -4.405503
hhdata1$hycoord[hhdata1$hrhhid=="HRB-1028"] <- 15.273940
# correct HRB-1040 coordinates using coords from the correct neighborhood (interview took place at BINZA)
hhdata1$hxcoord[hhdata1$hrhhid=="HRB-1040"] <- 4.409953
hhdata1$hycoord[hhdata1$hrhhid=="HRB-1040"] <- 15.245214

# select individual entries
inddata1 <- data1[!is.na(data1$redcap_repeat_instrument),] #data1$redcap_repeat_instrument=="questionnaire_individuel"
# select only individual variables
inddata1 <- inddata1 %>% 
  dplyr::select("hrhhid", "redcap_repeat_instance", "participant_code", "maternity","hxcoord", "hycoord",starts_with("hr"), starts_with("i"))

# add index mother HBV status (HBV status of hh)
inddata1 <- left_join(inddata1, hhdata1[, c("hrhhid", "h10_hbv_rdt", "hdov")], by = "hrhhid")

inddata1 <- inddata1 %>% relocate("h10_hbv_rdt", .after = "participant_code")
inddata1<- inddata1 %>% 
  dplyr::filter(!is.na(inddata1$participant_code))

## Households with clusters
totalhbsagpositive <- inddata1 %>%
  dplyr::group_by(hrhhid) %>%
  dplyr::summarise(totalpositive = sum(i27a_rdt_result, na.rm=TRUE), n=n()) 

hhdata1 <- left_join(hhdata1, totalhbsagpositive, by = "hrhhid")
inddata1 <- left_join(inddata1, hhdata1[, c("hrhhid","totalpositive")],  by = "hrhhid")

inddata1 = subset(inddata1, select = -c(totalpositive.x,totalpositive.y) )


clusters <- inddata1[inddata1$totalpositive >1, ]
table(clusters$hrhhid)

