# script to clean HOVER data and make new variables for analysis
# adopted from HOVER_summary.Rmd file use throughout study enrollment

#Load packages----------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(knitr)
library(viridis)
library(scales)
library(REDCapR)
library(readr)
library(lubridate)
library(sf)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(tmaptools)
library(here)

## establish colors----------------------------------------------------------------
drc_colors <- c("#007fff", 	"#ce102f", 	"#f7d618")
#hbvcolors <- c('#00429d', '#93003a',"#A0A0A0") #red and blue
hbvcolors <- c('#33374b', '#e2738c',"#b7d1da",'ghostwhite') #navy and pink, and gray
nounprojgraphcol <- c("#4D4D4D","#B2182B")
# colors for responses https://www.color-hex.com/color-palette/1010756 

#Load data----------------------------------------------------------------

#Data for this study were originally importing directly from the REDCap server using an API token.
#While this route for data import is not available for users outside the study, the code for how this was done is shown here. CSV import using the publicly available datasets is shown further below.
data0 <- redcap_read_oneshot(redcap_uri = "https://global.redcap.unc.edu/api/",
                             token = read_file(here("redcap_api", "file.txt") %>% str_remove("hbv_hover/")))$data
# household and individual data are loaded in same dataset, one row per questionnaire. for all household questionnaire entries, the individual questions are empty, and vice versa. 
# the first row for a given household is the household questionnaire and the following rows are the individual entries for that household

#every household should have at least two rows in the downloaded file, as there should be one household questionnaire and at least one individual questionnaire completed. remove rows of false entries (opened but not completed enrollments)
data1 <- data0 %>% 
  add_count(hrhhid) %>% 
  filter(n>1) %>% 
  rename(origids = hrhhid)

# create new random ID variable
set.seed(888)
ids <- data1 %>% select(hrhhid) %>% unique() %>% mutate(hrhhid = sample(1000:9999, nrow(ids), replace = F))
write.csv(ids, here("Data", "hover_id_link.csv"), row.names = FALSE)
data2 <- left_join(data1, ids, by = "origids")
data2 <- data2 %>% relocate(hrhhid) %>% select(-origids)
#make sure ID variable saved as character
data2$hrhhid <- as.character(data2$hrhhid)
# create year variable 
data2$hdov_year <- year(data2$hdov)

## select household entries
hhdata1 <- data2[(is.na(data2$redcap_repeat_instrument)), ]
# for hh dataset, select only hh variables
hhdata1 <- hhdata1 %>% 
  dplyr::select(starts_with("h"), "questionnaire_menage_complete")
                         
## select individual entries
inddata1 <- data2[!is.na(data2$redcap_repeat_instrument),] #data2$redcap_repeat_instrument=="questionnaire_individuel"
# select only individual variables
inddata1 <- inddata1 %>% 
  dplyr::select("hrhhid", "redcap_repeat_instance", "participant_code","hxcoord", "hycoord",starts_with("hr"), starts_with("i"))

# add index mother recruitment HBV status h10_hbv_rdt (index status of hh) on individual dataset
inddata1 <- left_join(inddata1, hhdata1[, c("hrhhid", "h10_hbv_rdt", "hdov", "hdov_year")] ,by = "hrhhid")

inddata1 <- inddata1 %>% relocate("h10_hbv_rdt", .after = "participant_code")
#remove empty entries
inddata1<- inddata1 %>% 
  dplyr::filter(!is.na(inddata1$participant_code))
#individual PID var
inddata1$pid <- paste0(inddata1$hrhhid,"-",inddata1$participant_code)

##Import from CSV----
inddata1 <- read.csv(here("hover_individual.csv")) # this route assumes the data has been stored in the same folder as the code. adjust as needed
hhdata1 <- read.csv(here("hover_individual.csv")) # this route assumes the data has been stored in the same folder as the code. adjust as needed

# Clean GPS data--------------------------------------------------
# code for cleaning GPS data below, but actual GPS data are not shared with publicly available dataset, due to security of PII
# save GPS coords as numeric
hhdata1$hxcoord <- as.numeric(hhdata1$hxcoord)
hhdata1$hycoord <- as.numeric(hhdata1$hycoord)
# check GPS data formatting
hhdata1$hycoord_edit <- ifelse(floor(log10(hhdata1$hycoord))==7,
                               hhdata1$hycoord/1000000,
                               ifelse(floor(log10(hhdata1$hycoord))==6,hhdata1$hycoord/100000,
                                      hhdata1$hycoord))
hhdata1$hxcoord_edit <- ifelse(floor(log10(hhdata1$hxcoord))==5,
                               hhdata1$hxcoord/100000,
                               hhdata1$hxcoord)

hhdata1$hxcoord_edit <- (hhdata1$hxcoord_edit)*-1

# Clean household variables--------------------------------------------------------------------------------
hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h10_hbv_rdt_f=factor(
    hhdata1$h10_hbv_rdt, 
    levels = c(0, 1),
    labels = c("HBsAg-unexposed", "HBsAg-exposed")))

## Roof ----------------------------------------
#roof type - create indicator for modern or not, following https://gh.bmj.com/content/5/6/e002316.long, see supplement
# reflective of divide by expensive vs cheaper for wealth index
# Modern roof: metal, zinc/cement, tiles/slate, or cement (options=7, 9, 10, 11, 12 )
# 1, 01 = Chaume/palme/feuilles | 2, 02 = Mottes de terre | 3, 03 = Nattes | 4, 04 = Palmes/bambou | 5, 05 = Planches en bois | 6, 06 = Carton | 7, 07 = Tôle | 8, 08 = Bois | 9, 09 = Zinc/fibre de ciment | 10, 10 = Tuiles | 11, 11 = Béton (ciment) | 12, 12 = Shingles | 97, 97 = Autre | 98, 98 = Ne sait pas | 99, 99 = Refusé(e)

hhdata1 = hhdata1 %>%
  mutate(modernroof = case_when(
    h1_roof_type___7 == 1  ~ 1, #sheet metal
    h1_roof_type___9 >= 1  ~ 1, # zinc fiber
    h1_roof_type___10 == 1  ~ 1, # tiles
    h1_roof_type___11 == 1  ~ 1, #ciment
    h1_roof_type___12 == 1  ~ 1, # shingles
    h1_roof_type___7 == 1  ~ 1,
    h1_roof_type___1 == 1  ~ 0, # palm/leaves
    h1_roof_type___2 == 1  ~ 0, # clods of earth
    h1_roof_type___3 == 1  ~ 0, # Mats
    h1_roof_type___4 == 1  ~ 0, # bamboo
    h1_roof_type___5 == 1  ~ 0, # wooden planks
    h1_roof_type___6 == 1  ~ 0, # cardboard/plywood
    h1_roof_type___8 == 1  ~ 0, # Wood
    # no 97, 98, 99, and other
    TRUE ~ NA_real_) %>% as.numeric())

addmargins(table(hhdata1$modernroof,useNA = "always"))

# select out missing obs to check
# do these truly not have rooves?
missingroof <- hhdata1 %>% 
  dplyr::select( "hrhhid","h10_hbv_rdt", "hdov", starts_with("h1_roof")) %>% 
  filter(is.na(hhdata1$modernroof))

## Walls----------------------------------------
# Modern wall: cement, stone, bricks, or covered adobe (31, 32, 33, 34, 35)

hhdata1 = hhdata1 %>%
  mutate(modernwalls = case_when(
    h1_walls_type___1 == 1  ~ 0, #earth
    h1_walls_type___2 == 1  ~ 0, # bamboo/palms/trunks
    h1_walls_type___3 == 1  ~ 0, # bamboo w mud
    h1_walls_type___4 == 1  ~ 1, #stones with mud #****check this
    h1_walls_type___5 == 1  ~ 0, # uncovered adobe
    h1_walls_type___6 == 1  ~ 0, # plywood
    h1_walls_type___7 == 1  ~ 0, # cardboard
    h1_walls_type___8 == 1  ~ 0, # reclaimed wood
    h1_walls_type___9 == 1  ~ 1, # cement
    h1_walls_type___10 == 1  ~ 1, # stones with cement
    h1_walls_type___11 == 1  ~ 1, # bricks
    h1_walls_type___12 == 1  ~ 1, # cement blocks
    h1_walls_type___13 == 1  ~ 1, # covered adobe
    h1_walls_type___14 == 1  ~ 1, # wood planks/shingles
    h1_walls_otherlist == "Tol"  ~ 0, # tol/tole = sheet metal - not modern
    h1_walls_otherlist == "Tole"  ~ 0, # tol/tole = sheet metal
    h1_walls == 0 ~ 0, 
    # no 98, 99, and other
    TRUE ~ NA_real_) %>% as.numeric())
addmargins(table(hhdata1$modernwalls,useNA = "always"))
# no missing

## Flooring----------------------------------------
# Modern floor: vinyl, asphalt, ceramic tiles, cement, or carpet
hhdata1 = hhdata1 %>%
  mutate(modernfloor = case_when(
    h1_floor_type___1 == 1  ~ 0, #earth
    h1_floor_type___2 == 1  ~ 0, # dung, none
    h1_floor_type___3 == 1  ~ 0, # wooden planks
    h1_floor_type___4 == 1  ~ 0, # bamboo/palm leaves
    h1_floor_type___5 == 1  ~ 1, # parquet/polished wood
    h1_floor_type___6 == 1  ~ 1, # vinyl/asphalt
    h1_floor_type___7 == 1  ~ 1, # tiles
    h1_floor_type___8 == 1  ~ 1, # cement
    h1_floor_type___9 == 1  ~ 1, # carpet
    # no 97, 98, 99, and other
    TRUE ~ NA_real_) %>% as.numeric())
addmargins(table(hhdata1$modernfloor,useNA = "always"))
# no missing

## Windows----------------------------------------
# Modern windows: glass or screens
table(hhdata1$h1_windows_otherlist, hhdata1$h1_windows_type___98, useNA = "always")

hhdata1 = hhdata1 %>%
  mutate(modernwindow = case_when(
    h1_windows == 0 ~ 0, # no windows
    is.na(h1_windows) ~ 0, # no windows
    h1_windows_type___1 == 1  ~ 1, #glass
    h1_windows_type___2 == 1  ~ 1, # screen
    h1_windows_type___3 == 1  ~ 0, # open
    h1_windows_type___4 == 1  ~ 0, # plastic/paper/carton
    h1_windows_type___5 == 1  ~ 0, # planks
    h1_windows_type___97 == 1 ~ 0, # all 'other' are wood or metal
    h1_windows_type___98 == 1 ~ 0, # don't know: count as don't know since screen/glass only one
    TRUE ~ NA_real_) %>% as.numeric())
addmargins(table(hhdata1$modernwindow,useNA = "always"))
table(hhdata1$h1_windows, hhdata1$modernwindow,useNA = "always")

# no missing

## Holes in wall/house----------------------------------------
addmargins(table(hhdata1$h2_walls_holes, hhdata1$modernwalls, useNA = "always"))

# Modern housing: modernroof==1, modernwalls==1, modernfloor==1, modernwindows==1, h2_walls_holes==0
hhdata1 = hhdata1 %>%
  mutate(modernhousing = case_when(
    modernroof == 1 & modernwalls == 1  & modernfloor == 1  & modernwindow == 1  & h2_walls_holes == 0 ~ 1,
    TRUE ~ 0) %>% as.numeric())
addmargins(table(hhdata1$modernhousing, useNA = "always"))
addmargins(table(hhdata1$modernhousing, hhdata1$modernfloor ,useNA = "always")) 

## wealth objects of hh--------------------------------------------------
hhdata1 <- hhdata1 %>% 
  rename(h3_hh_wealth_electr = h3_hh_wealth___1,
         h3_hh_wealth_toilet = h3_hh_wealth___2,
         h3_hh_wealth_radio = h3_hh_wealth___3,
         h3_hh_wealth_tv = h3_hh_wealth___4,
         h3_hh_wealth_fridge = h3_hh_wealth___5,
         h3_hh_wealth_cooktop = h3_hh_wealth___6,
         h3_hh_wealth_generator = h3_hh_wealth___7,
         h3_hh_wealth_beds = h3_hh_wealth___8,
         h3_hh_wealth_lamps = h3_hh_wealth___9,
         h3_hh_wealth_over = h3_hh_wealth___10,
         h3_hh_wealth_hoes = h3_hh_wealth___11,
         h3_hh_wealth_sewing = h3_hh_wealth___12)

table(hhdata1$h3_hh_wealth___98) # one reports don't know      
table(hhdata1$h3_hh_wealth___99) # none refuse            

## wealth objects of members--------------------------------------------------
table(hhdata1$h4_hh_member_wealth___1)       

hhdata1 <- hhdata1 %>% 
  rename(h4_hh_member_wealth_watch = h4_hh_member_wealth___1,
         h4_hh_member_wealth_cellph = h4_hh_member_wealth___2,
         h4_hh_member_wealth_canoe = h4_hh_member_wealth___3,
         h4_hh_member_wealth_moto = h4_hh_member_wealth___4,
         h4_hh_member_wealth_car = h4_hh_member_wealth___5,
         h4_hh_member_wealth_animalcart = h4_hh_member_wealth___6,         
         h4_hh_member_wealth_motorboat = h4_hh_member_wealth___7,
         h4_hh_member_wealth_bike = h4_hh_member_wealth___8,
         h4_hh_member_wealth_comp = h4_hh_member_wealth___9,
         h4_hh_member_wealth_houserent = h4_hh_member_wealth___10)

table(hhdata1$h4_hh_member_wealth___98) # two reports don't know      
table(hhdata1$h4_hh_member_wealth___99) # none refuse      

## cultivatable land--------------------------------------------------
addmargins(table(hhdata1$h5_cultivable_land, useNA = "always")) # one refusal, one missing
addmargins(table(hhdata1$h5a_cultivable_land_hect, useNA = "always"))
table(hhdata1$h5_cultivable_land, hhdata1$h5a_cultivable_land_hect ,useNA = "always") # assign missing hectaires to 0 if fam reported not ownership

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5_cultivable_land_f=factor(
    hhdata1$h5_cultivable_land, 
    levels = c(0, 1, 99),
    labels = c("No", "Yes", "Refused")))

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5_cultivable_land_wi = case_when(
    h5_cultivable_land == 0 ~ 0,
    h5_cultivable_land == 1 ~ 1,
    h5_cultivable_land == 99 ~ 1, # assigning single refusal to yes
    #h5_cultivable_land == 99 ~ NA_real_, # other option to assign refused (n=1) to missing (but not included in wealth index)
    is.na(h5_cultivable_land) ~ 0, # assign missing (n=1) to 0
    TRUE ~ h5_cultivable_land))
addmargins(table(hhdata1$h5_cultivable_land_wi, useNA = "always"))

hhdata1$h5a_cultivable_land_hect_num <- as.numeric(hhdata1$h5a_cultivable_land_hect)
hhdata1$h5a_cultivable_land_hect_num <- ifelse(hhdata1$h5_cultivable_land==0,0,hhdata1$h5a_cultivable_land_hect_num) # can give all 0 if missing based above analysis above
table(hhdata1$h5_cultivable_land, hhdata1$h5a_cultivable_land_hect_num ,useNA = "always")

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5a_cultivable_land_hect_char = case_when(
    h5_cultivable_land == 0 ~ 0,
    h5a_cultivable_land_hect_num == 1 ~ 1,
    h5a_cultivable_land_hect_num == 2 ~ 2,
    h5a_cultivable_land_hect_num == 3 ~ 3,
    h5a_cultivable_land_hect_num == 4 ~ 4,
    h5a_cultivable_land_hect_num == 20 ~ 20,
    h5a_cultivable_land_hect_num == 200 ~ 200,
    h5a_cultivable_land_hect_num > 95 & h5a_cultivable_land_hect_num <= 98 ~ 98, # collapsing don't knows
    h5a_cultivable_land_hect_num == 99  ~ 99, # refusal
    is.na(h5a_cultivable_land_hect_num) ~ 99, # refusal
    TRUE ~ NA_real_))

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5a_cultivable_land_hect_char_f=factor(
    hhdata1$h5a_cultivable_land_hect_char, 
    levels = c(0, 1, 2, 3, 4, 20, 200, 98,99),
    labels = c("No land", "1 hectare", "2 hectares", "3 hectares", "4 hectares", "20 hectares", "200 hectares", "Exact quantity unknown","Refuse to say")))
addmargins(table(hhdata1$h5a_cultivable_land_hect_char_f, useNA = "always"))

missingland <- hhdata1 %>% filter(is.na(h5_cultivable_land) | h5_cultivable_land==99)

## Water source------------------------------------------------------
# explore frequency of different water sources
table(hhdata1$h6_water_access___3, hhdata1$h6_water_access___1)
table(hhdata1$h6_water_access___6, hhdata1$h6_water_access___3) 
table(hhdata1$h6_water_access___99) #97 = other, 98 = don't know, 99 = refused
table(hhdata1$h6_water_access_otherlist, hhdata1$h6_water_access___4) #97 = other, 98 = don't know, 99 = refused
# forage, Forage, Forrage = borehole

# not uncommon to have multiple sources - could reflect wealth to have access to multiple. 
# if only or at least one source is piped to house, code as 3 (best)
# if piped to neighbor, code as 2 (next best)
# if communal tap/protected borehole, code as 1 (next best to have some distinction in wealth index)
# if only spring or no source, 0.

hhdata1 = hhdata1 %>%
  mutate(privatewater = case_when(
    h6_water_access___1 == 1 ~ 3, #private piped water
    h6_water_access___2 == 1 ~ 2, # piped water from neighbor
    h6_water_access___3 == 1 ~ 1, # communal tap
    h6_water_access___4 == 1 ~ 1, # protected well
    h6_water_access___6 == 1 ~ 0, #  spring water
    h6_water_access_otherlist == "forage" ~ 1,
    h6_water_access_otherlist == "Forage" ~ 1,
    h6_water_access_otherlist == "Forrage" ~ 1,
    TRUE ~ NA_real_) %>% as.numeric())

table(hhdata1$privatewater, useNA = "always")
table(hhdata1$privatewater, hhdata1$h6_water_access___6)

## Cooking source------------------------------------------------------
addmargins(table(hhdata1$h7_cooking_fuel___1,hhdata1$h7_cooking_fuel___2,hhdata1$h7_cooking_fuel___3  ))

hhdata1 = hhdata1 %>%
  mutate(cookfuel = case_when(
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 0 ~ 0, # charcoal only
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 0 ~ 0, # charcoal only
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 0 ~ 1, # charcoal plus gas 
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 1 ~ 1, # charcoal plus electric 
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 1 ~ 2, # all 3
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 0 ~ 3, # gas only, none with this
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 1 ~ 3, # electric only
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 1 ~ 3, # electric or gas 
    TRUE ~ NA_real_) %>% as.numeric())
#Summary of cooking categorizations
# having electric with/without gas backup but no charcoal highest standard of living
# using charcoal lower standard
# if electric/gas with charcoal this lower than electric/gas alone
# charcoal plus one of the other below having all 3
# charcoal only lowest

table(hhdata1$cookfuel, useNA = "always")

## People in household per bed--------------------------------------------------
table(hhdata1$n, useNA = "always")
table(hhdata1$h3_hh_wealth_beds, useNA = "always") # yes/no to owning beds, not number

# Make wealth index------------------------------------------------------
# subset dataset for prcomp()
wealthpca_hoverhh <- hhdata1[, c("hrhhid", "n",
                                 #"cookfuel", 
                                 "privatewater", "modernwalls", "modernfloor", 
                                 #"modernroof", 
                                 "modernwindow",
                                 "h5_cultivable_land_wi",
                                     "h3_hh_wealth_electr" ,
                                     "h3_hh_wealth_toilet",
                                     "h3_hh_wealth_radio",
                                     "h3_hh_wealth_tv" ,
                                     "h3_hh_wealth_fridge" ,
                                     "h3_hh_wealth_cooktop",
                                     "h3_hh_wealth_generator", 
                                     "h3_hh_wealth_beds" ,
                                     "h3_hh_wealth_lamps" ,
                                     "h3_hh_wealth_over",
                                     "h3_hh_wealth_hoes" ,
                                     "h3_hh_wealth_sewing", 
     "h4_hh_member_wealth_watch", "h4_hh_member_wealth_cellph",  "h4_hh_member_wealth_canoe",   
  "h4_hh_member_wealth_moto", "h4_hh_member_wealth_car", "h4_hh_member_wealth_animalcart","h4_hh_member_wealth_motorboat", "h4_hh_member_wealth_bike", "h4_hh_member_wealth_comp", "h4_hh_member_wealth_houserent")]

summary(wealthpca_hoverhh) # look at the distributions of all possible variables to include
##observations##
# few have animal cart h4_hh_member_wealth_animalcart
# few have motorboat h4_hh_member_wealth_motorboat
# no one has a canoe
# is radio necessary - remove?

# drop these variables
wealthpca_hoverhh <- wealthpca_hoverhh %>% dplyr::select(!c(h4_hh_member_wealth_animalcart,h4_hh_member_wealth_motorboat, h4_hh_member_wealth_canoe,h3_hh_wealth_radio))

# complete case?

wealthpca_hoverhh_nomiss <- wealthpca_hoverhh[complete.cases(wealthpca_hoverhh), ]
view(wealthpca_hoverhh_nomiss)
# which IDs dropped
notcompcase <- subset(wealthpca_hoverhh, !(wealthpca_hoverhh$hrhhid %in% wealthpca_hoverhh_nomiss$hrhhid))
# none dropped 

pca_hover <- prcomp(as.matrix(wealthpca_hoverhh_nomiss[, 3:25]), scale=TRUE)#change to 28
summary(pca_hover) # first component explains 23% of variance
# this feels low (Odum Institute, talking with other students, and the internet suggest that you could include multiple components, to reach closer to 80%, but Marcel's group (see papers cited in AVERT supplement) only used the first component, so we followed that precedent).
pca_R_output <- as.data.frame(pca_hover$x)
# pca_R_output$subject_id <- rownames(pca_R_output)

pca_R_output <- cbind(pca_R_output, wealthpca_hoverhh_nomiss$hrhhid)
#Create percentiles and wealth index
summary(pca_R_output$PC1)

ggplot(pca_R_output)+
  geom_histogram( aes(PC1), binwidth = 0.5)+
  ggtitle("Distribution of values for Principal Component 1")+
  xlab("Principal component 1")+
  ylab("Count of participants")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)
  )

hist(pca_R_output$PC1, breaks=seq(-6,6,0.5))+title("Value of Principal Component 1")
quantile(pca_R_output$PC1, probs = c(0.25, 0.5,0.75, 1), na.rm = T)

# Graphical displays for supplementary material
var_explained_df <- data.frame(PC= paste0("PC", 1:23), #or 26
                               var_explained=(pca_hover$sdev)^2/sum((pca_hover$sdev)^2))

head(var_explained_df)
var_explained_df$PC <- factor(var_explained_df$PC, levels = var_explained_df$PC[order(-var_explained_df$var_explained)])

var_explained_df %>%
  ggplot(aes(x=PC,y=var_explained, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: PCA on scaled data")+
  ylab("Variance explained by each component")+
  xlab("Principal component")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
view(var_explained_df)
quantile(pca_R_output$PC1, probs = c(0.25, 0.5,0.75, 1), na.rm = T)

# Wealth variable used in Table 1
# when enrollment is complete, update these percentiles
pca_R_output$wealth_R <- as.numeric(0)
pca_R_output = pca_R_output %>%
  mutate(wealth_R = case_when(
    PC1 <= -1.3286 ~ 0,
    PC1 > -1.3286 & PC1 <= -0.3221 ~ 1,
    PC1 > -0.3221 & PC1 <= 1.1651 ~ 2,
    PC1 > 1.1651  ~ 3,
    TRUE ~ wealth_R
    ) %>% as.numeric())

table(pca_R_output$wealth_R, useNA = "always")
# rename PID variable so it matches enrollment and merge will work in next step
pca_R_output <- pca_R_output %>% rename(hrhhid = `wealthpca_hoverhh_nomiss$hrhhid`)
# add new wealth variable back onto main enrollment dataset

hhdata1 <- inner_join(hhdata1, pca_R_output[, c("hrhhid", "wealth_R")], by = c("hrhhid"))


#put wealth onto individual dataset
inddata1 <- left_join(inddata1, hhdata1[,c("hrhhid","wealth_R")],  by = "hrhhid")
table(inddata1$wealth_R)

inddata1$wealth_R <- as.factor(inddata1$wealth_R)

# indicators
inddata1$wealth_R_num <- as.numeric(inddata1$wealth_R) # lowest to highest = poorest to richest

inddata1 = inddata1 %>%
  mutate(wealth_R_lowestv = case_when(
      wealth_R_num == 1 ~ 0, #lowest vs upper 3
      wealth_R_num > 1 ~ 1),
    wealth_R_highestv = case_when(
      wealth_R_num < 4 ~ 0,
      wealth_R_num == 4 ~ 1))
table(inddata1$wealth_R_lowestv)
table(inddata1$wealth_R_highestv)
table(inddata1$wealth_R_num)
table(inddata1$wealth_R)

#Sharing personal items---------------------------
# nail clippers
table(hhdata1$h8_nail_cutting)

table(hhdata1$h8a_nail_clippers_owned)
table(hhdata1$h8b_nail_filer_owned)

# sharing razors
table(hhdata1$h9_razor)
table(hhdata1$h8a_razer_owned)

# make new shared clipper/razor variables
hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h8_nail_cutting_f=factor(
    hhdata1$h8_nail_cutting, 
    levels = c(0, 1, 98, 99),
    labels = c("No", "Yes", "Don't know", "Refused")))
# count of nail clipper and filers owned
hhdata1$h8a_nail_clippers_owned <- as.factor(as.numeric(hhdata1$h8a_nail_clippers_owned))
hhdata1$h8b_nail_filer_owned <- as.factor(as.numeric(hhdata1$h8b_nail_filer_owned))

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h9_razor_f=factor(
    hhdata1$h9_razor, 
    levels = c(0, 1, 98, 99),
    labels = c("No", "Yes", "Don't know", "Refused")))

hhdata1$h8a_razer_owned <- as.factor(as.numeric(hhdata1$h8a_razer_owned))
table(hhdata1$h9_razor_f ,hhdata1$h8a_razer_owned) # what does 96 mean - probably don't know

#Final hh spatial object with new variables---------
hover_gps = st_as_sf(hhdata1[!is.na(hhdata1$hxcoord_edit) &!is.na(hhdata1$hycoord_edit),], coords = c("hycoord_edit", "hxcoord_edit"), crs = 4326)  
hover_gps_full <- hover_gps
hover_gps <- subset(hover_gps, select=c( "h10_hbv_rdt_f" , "hrhhid","geometry")) 

#Clean individual survey variables------------------------
inddata1 <- inddata1 %>% 
  dplyr::mutate(h10_hbv_rdt_f=factor(
    inddata1$h10_hbv_rdt, 
    levels = c(0, 1),
    labels = c("Unexposed", "Exposed")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i27a_rdt_result_f=factor(
    inddata1$i27a_rdt_result, 
    levels = c(0, 1,4),
    labels = c("HBsAg-", "HBsAg+","Indéterminé")))
#labels = c("Negative", "Positive")))

inddata1$hr7_age_mois <- as.numeric(inddata1$hr7_age_mois)
inddata1$age_combined <- round(ifelse(as.numeric(inddata1$hr7a_month_year==0), as.numeric(inddata1$hr7_age_year), as.numeric(inddata1$hr7_age_mois)/12),2)

inddata1 %>% group_by(h10_hbv_rdt_f) %>% 
  ggplot()+geom_histogram(aes(x=age_combined, fill=h10_hbv_rdt_f))

# age group <15 vs > 15 (birth in 2007)
# https://www.gavi.org/sites/default/files/document/annual-progress-report-congo%2C-democratic-republic-of-the-2007pdf.pdf
# TDP+hepB first introduced as tetravalent in 2007, limited distribution
# TDP+hepB+Hib not really distributed until 2009, per 2009 Gavi progress report

# re-coded agegrp15_2 which was based on 15yrs but not year of study enr
inddata1 = inddata1 %>%
  mutate(age_cat = case_when(
    hdov_year=="2021" & age_combined <= 12 ~ 0, # born since penta introduced - ref group
    hdov_year=="2022" & age_combined <= 13 ~ 0, # born since penta introduced - ref group
    
    hdov_year=="2021" & age_combined > 12 & age_combined <= 14 ~ 1, # between tetra/penta
    hdov_year=="2022" & age_combined > 13 & age_combined <= 15 ~ 1, # between tetra/penta
    
    hdov_year=="2021" & age_combined > 14 ~ 2, # no vacc
    hdov_year=="2022" & age_combined > 15 ~ 2, # no vacc

    TRUE ~ 0) %>% as.factor())
table(inddata1$age_cat, useNA = "always")

inddata1 <- inddata1 %>% mutate(age_cat_cons = case_when(
  age_cat =="2" ~ 1, # if person was likely not vaccinated or possibly not, group together
  age_cat =="1" ~ 1, # if person was likely not vaccinated or possibly not, group together
  age_cat == "0" ~ 0)
  %>% as.factor(),
  age_cat_highrisk = case_when(
    age_cat == "2" ~ 1,
    age_cat == "1" ~ 0,
    age_cat == "0" ~ 0) %>% as.factor()) 
addmargins(table(inddata1$age_cat_cons, inddata1$h10_hbv_rdt_f, inddata1$hhmemcat_f))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr3_relationship_f=factor(
    inddata1$hr3_relationship, 
    levels = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,97,98,99),
    #labels = c("Sans parenté", "Mère index","Femme ou mari", "Fils/fille", "Gendre/belle fille", "Petit-fils/fille","Père/mère","Beaux-parents","Frère/soeur","Neveu/nièce","Neveu/nièce par alliance","Enfant adopté/ garde/de la femme/du mari","Tante/oncle","Grandpère/mère","Autre","Ne sait pas", "Refusé")))    
labels = c("Unrelated", "Index mother","Spouse","Son/daughter","Step-son/daughter","Grandson/daughter","Father/mother","In-laws","Brother/sister","Nephew/niece","Nephew/niece by marriage","Adopted/in custody","Aunt/uncle","Grandmother/father","Other","Don't know", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr3_relationship_fr=factor(
    inddata1$hr3_relationship, 
    levels = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,97,98,99),
    labels = c("Sans parenté", "Mère index","Femme ou mari", "Fils/fille", "Gendre/belle fille", "Petit-fils/fille","Père/mère","Beaux-parents","Frère/soeur","Neveu/nièce","Neveu/nièce par alliance","Enfant adopté/ garde/de la femme/du mari","Tante/oncle","Grandpère/mère","Autre","Ne sait pas", "Refusé")))    
    #labels = c("Unrelated", "Index mother","Spouse","Son/daughter","Step-son/daughter","Grandson/daughter","Father/mother","In-laws","Brother/sister","Nephew/niece","Nephew/niece by marriage","Adopted/in custody","Aunt/uncle","Grandmother/father","Other","Don't know", "Refused")))

#relationship simple
inddata1 <- inddata1 %>% 
  dplyr::mutate(hr3relat_simp = case_when(
    hr3_relationship == 0 ~ 6, #other
    hr3_relationship == 1 ~ 1, #index mother
    hr3_relationship == 2 ~ 2, # spouse
    hr3_relationship == 3 ~ 3, # child
    hr3_relationship == 8 ~ 4, # brother/sister
    hr3_relationship == 9 ~ 5, # niece/nephew
    hr3_relationship == 10 ~ 5, # niece/nephew par alliance - collapse
    hr3_relationship >=4 & hr3_relationship <=7 ~ 6, # collapsing categories - other
    hr3_relationship >=11 ~ 6, # collapsing categories - other
    TRUE ~ NA_real_))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr3relat_simp_f=factor(
    inddata1$hr3relat_simp, 
    levels = c(1,2,3,4,5,6),
    labels = c("Index mother","Spouse","Son/daughter","Brother/sister","Nephew/niece","Other")))
table(inddata1$hr3relat_simp_f)

# direct offspring enrolled
countsbyhh <- inddata1 %>% group_by(hrhhid) %>% summarise(numdiroff = sum(hr3_relationship==3), malepartner = sum(hr3_relationship==2))
addmargins(table(countsbyhh$numdiroff))
addmargins(table(countsbyhh$malepartner))

inddata1 <- left_join(inddata1, countsbyhh[, c("hrhhid", "numdiroff", "malepartner")], by = "hrhhid")
table(inddata1$numdiroff)
table(inddata1$malepartner)
hhdata1 <- left_join(hhdata1, countsbyhh[, c("hrhhid", "numdiroff", "malepartner")], by = "hrhhid")
addmargins(table(hhdata1$numdiroff))
table(hhdata1$malepartner)

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr4_sex_fr=factor(
    inddata1$hr4_sex, 
    levels = c(0, 1),
    labels = c("Masculin", "Féminin")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr4_sex_f=factor(
    inddata1$hr4_sex, 
    levels = c(0, 1),
    labels = c("Male", "Female")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr5_primary_residence_f=factor(
    inddata1$hr5_primary_residence, 
    levels = c(0, 1, 99),
   # labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr6_last_night_residence_f=factor(
    inddata1$hr6_last_night_residence, 
    levels = c(0, 1, 99),
    #labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr8_marital_status_f=factor(
    inddata1$hr8_marital_status, 
    levels = c(0, 1, 2,3,96),
    labels = c("Never married", "Married or living together", "Divorced", "Widow(er)", "N/A")))

# years living at this address
inddata1$i6_time_lived_resid_yrs <- as.numeric(inddata1$i6_time_lived_resid_yrs)
inddata1$i6_time_lived_resid_months <- as.numeric(inddata1$i6_time_lived_resid_months)

inddata1$i6_mon_conv <- as.numeric(inddata1$i6_time_lived_resid_yrs)*12 + as.numeric(inddata1$i6_time_lived_resid_months)
inddata1$i6_comb_yr <- (inddata1$i6_mon_conv)/12

# 42 have 0 for both and 14 are NA
ad_is <- inddata1 %>% filter(is.na(i6_comb_yr) | i6_comb_yr==0)
# is this not their primary residence
table(ad_is$hr6_last_night_residence)
table(ad_is$hr5_primary_residence_f)
# who are these
table(ad_is$hr3_relationship_f)
table(ad_is$age_combined)

table(inddata1$i6_comb_yr, useNA = "always")

##Education recode---------------------------------------------------------------
inddata1 <- inddata1 %>% 
  dplyr::mutate(hr9_school_gr = case_when(
    hr9_schooling == 0 ~ 0,
    hr9_schooling == 1 ~ 1,
    hr9_schooling == 2 ~ 2, 
    hr9_schooling == 3 ~ 3, # 1er cycle d'orientation
    hr9_schooling == 4 ~ 3, # 2ème cycle d'orientation
    hr9_schooling == 5 ~ 4, # some secondary ed
    hr9_schooling == 6 ~ 4, 
    hr9_schooling == 7 ~ 4, 
    hr9_schooling == 8 ~ 5, # finished secondary school 
    hr9_schooling == 9 ~ 6, # 3-yr uni
    hr9_schooling == 10 ~ 7, # 5-yr uni
    hr9_schooling == 11 ~ 8, # doctorat
    hr9_schooling == 96 ~ 96, 
    hr9_schooling == 97 ~ 97, # other but no opp to specify
    hr9_schooling == 98 ~ 98,
    hr9_schooling == 99 ~ 99,
   TRUE ~ NA_real_))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr9_school_gr_f=factor(
    inddata1$hr9_school_gr, 
    levels = c(0, 1, 2,3,4,5,6,7,8,96,97,98,99),
    labels = c("No schooling", "Primary school not finished", "Finished primary school",
               "1st/2nd orientation", "Some secondary school","Finished secondary school","3 years of university",
               "5 years of university", "Doctorate","N/A","Other","Don't know","Refused")))
# simplified eduation
inddata1 <- inddata1 %>% 
  dplyr::mutate(educ_simp = case_when(
    age_combined < 5 ~ 0, # below school age
    hr9_school_gr == 0 ~ 1, # no schooling
    hr9_school_gr >0 & hr9_school_gr < 3 ~ 2, # any primary
    hr9_school_gr >= 3 & hr9_school_gr <=5 ~ 3, #1er/2eme cycle d'orientation - secondary
    hr9_school_gr > 5 & hr9_school_gr <= 8 ~ 4, # any university/doctorat
    hr9_school_gr >= 96 ~ 99, # N/A, other, DK, refused
    is.na(hr9_school_gr) ~99,
    TRUE ~ NA_real_))
inddata1 <- inddata1 %>% 
  dplyr::mutate(educ_simp_f=factor(
    inddata1$educ_simp, 
    levels = c(0, 1, 2,3,4,99),
    labels = c("Below school age","No schooling", "Any primary", "Any secondary",
               "Any university", "Other")))
table(inddata1$educ_simp_f, inddata1$hr9_school_gr,useNA = "always")

##Occupation recode---------------------------------
inddata1 <- inddata1 %>% 
  dplyr::mutate(hr10_occupation_gr = case_when(# divide salaried, self-employed, student
    hr10_occupation == 0 ~ 0, # no occupation
    hr10_occupation == 1 ~ 1, # civil servant - salaried
    hr10_occupation == 2 ~ 1, # soldier/police - salaried
    hr10_occupation == 3 ~ 1, # private employee - salaried
    hr10_occupation == 4 ~ 2, # farmer - self
    hr10_occupation == 5 ~ 2, # fisherman - self
    hr10_occupation == 6 ~ 2, # driver - self 
    hr10_occupation == 7 ~ 3, # hotel employee - works for someone else 
    hr10_occupation == 8 ~ 3, # sweeper - works for someone else 
    hr10_occupation == 9 ~ 2, # market vendor - self
    hr10_occupation == 10 ~ 2, # shop owner - self
    hr10_occupation == 11 ~ 2, # street vendor - self
    hr10_occupation == 12 ~ 2, # money changer - self
    hr10_occupation == 13 ~ 2, # Artist - self
    hr10_occupation == 14 ~ 2, # tailor - self
    hr10_occupation == 15 ~ 2, # small business - self
    hr10_occupation == 16 ~ 4, # student
    hr10_occupation == 96 ~ NA_real_, # set as missing
    hr10_occupation >= 97 ~ 97, # other97/DK 98/refused 99
    TRUE ~ NA_real_))

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr10_occupation_gr_f=factor(
    inddata1$hr10_occupation_gr, 
    levels = c(0, 1, 2,3,4,97),
    labels = c("No occupation", "Salaried", "Self-employed",
               "Works for someone else", "Student","Other")))
table(inddata1$hr10_occupation_gr_f, inddata1$hr10_occupation, useNA = "always")
##Religion recode---------------------------
# look at religion by household 
## table(inddata1$hr11_religion, useNA = "always")
## table(inddata1$hr11_religion, inddata1$hrhhid, useNA = "always")
# mostly the same within households - consider putting under household

inddata1 <- inddata1 %>% 
  dplyr::mutate(hr11_religion_f=factor(
    inddata1$hr11_religion, 
    levels = c(0, 1, 2,3,4,5,6,7,8,9,97,98,99),
    labels = c("No religion", "Traditional", "Catholic",
               "Evangelical", "Revivalist","Adventist","Protestant", "Muslim","Kimbanguism",
               "Salvation Army", "Other","Don't know","Refused")))
table(inddata1$hr11_religion)

# simple religion - any vs none
inddata1 <- inddata1 %>% 
  dplyr::mutate(religion_simp = case_when(# any vs none
    hr11_religion == 0 ~ 0, # no religion
    hr11_religion >0  ~ 1, # any religion (dont know /refused are both <2 years old)
    TRUE ~ NA_real_))

inddata1 <- inddata1 %>% 
  dplyr::mutate(religion_simp_f=factor(
    inddata1$religion_simp, 
    levels = c(0, 1),
    labels = c("No religion", "Any religion")))
    
##Med hx/exposures--------------
inddata1 <- inddata1 %>% 
  dplyr::mutate(i2_past_hbv_dx_f=factor(
    inddata1$i2_past_hbv_dx, 
    levels = c(0, 1, 3,98,99),
    #labels = c("Non", "Oui", "Jamais testé", "Ne sait pas", "Refusé")))
labels = c("No", "Yes", "Never tested", "Don't know", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i1_hbv_positive_f=factor(
    inddata1$i1_hbv_positive, 
    levels = c(0, 1,2, 3,98,99),
   # labels = c("Non", "Oui, une fois","Oui, plus d'une fois", "Jamais testé", "Ne sait pas", "Refusé")))
labels = c("No", "Yes, once","Yes, more than once", "Never tested", "Don't know", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i3_hiv_pos_test_f=factor(
    inddata1$i3_hiv_pos_test, 
    levels = c(0, 1, 3,98,99),
  #  labels = c("Non", "Oui", "Jamais testé", "Ne sait pas", "Refusé")))
labels = c("No", "Yes", "Never tested", "Don't know", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i3a_hiv_treatment_f=factor(
    inddata1$i3a_hiv_treatment, 
    levels = c(0, 1, 98,99),
   # labels = c("Non", "Oui", "Ne sait pas","Refusé")))
   labels = c("No", "Yes", "Don't know", "Refused")))

table(inddata1$i3b_hiv_medications, inddata1$i3a_hiv_treatment_f, useNA = "always")

inddata1 = inddata1 %>%
  mutate(hivhaart = case_when(
    i3b_hiv_medications == ",TLD" ~ "TLD", #
    i3b_hiv_medications == "CS PILOTE" ~ "Not specified", #
    i3b_hiv_medications == "DLT" ~ "Dolutegravir", #
    i3b_hiv_medications == "Dolutegravir,3TC,TDF" ~ "TLD", #
    i3b_hiv_medications == "Dolutegravire,Abacavir" ~ "Dolutegravir, Abacavir", #
    i3b_hiv_medications == "TDF +3TC+EFV" ~ "TLD", #
    i3b_hiv_medications == "Tld" ~ "TLD", #
    i3b_hiv_medications == "TLD" ~ "TLD", #
    i3a_hiv_treatment == 0 & is.na(i3b_hiv_medications) ~ "Not taking",
    TRUE ~ "" # hh mmeber is all other types of relationships
  ))
table(inddata1$hivhaart, inddata1$i3a_hiv_treatment_f, useNA = "always") # one says not taking but taking TLD - recode taking/not taking var?

inddata1 <- inddata1 %>% 
  dplyr::mutate(i7_diabetes_f=factor(
    inddata1$i7_diabetes, 
    levels = c(0, 1,99),
    #labels = c("Non", "Oui", "Refusé")))
    labels = c("No", "Yes", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i4_fever_f=factor(
    inddata1$i4_fever, 
    levels = c(0, 1, 98,99),
    #labels = c("Non", "Oui", "Ne sait pas","Refusé")))
 labels = c("No", "Yes", "Don't know", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i5_pregnancy_f=factor(
    inddata1$i5_pregnancy, 
    levels = c(0, 1, 98,99),
    #labels = c("Non", "Oui", "Ne sait pas","Refusé")))
labels = c("No", "Yes", "Don't know", "Refused")))

table(inddata1$i14_shared_razor, useNA = "always") # some missing
#which ones missing
inddata1 %>% filter(is.na(i14_shared_razor)) %>% reframe(hrhhid, age_combined,hr4_sex_f, hr3_relationship_f)
# use household questions on razors and responses of other hh members to assign value for individual missing
# in both of these households, the individuals don't own razors, so  i14_shared_razor==0 is assigned

inddata1$i14_shared_razor <- ifelse(is.na(inddata1$i14_shared_razor),0,inddata1$i14_shared_razor) # can give all 0 if missing based above analysis above

inddata1 <- inddata1 %>%
  dplyr::mutate(i14_shared_razor_f=factor(
    inddata1$i14_shared_razor, 
    levels = c(0, 1, 99),
    #    labels = c("Non", "Oui", "Refusé")))
    labels = c("No", "Yes/Refused", "Yes/Refused"))) # four were refused - combine with yes
table(inddata1$i14_shared_razor_f, useNA = "always") # 

table(inddata1$i15_shared_nailclippers, useNA = "always") # some missing
#which ones missing
inddata1 %>% filter(is.na(i15_shared_nailclippers)) %>% summarise(hrhhid, age_combined,hr4_sex_f, hr3_relationship_f)
# use questions on inddata1 dataset for nail clippers to assign value for individual missing
# other children < 15 have value 0 so assigning member with missing value to 0
inddata1$i15_shared_nailclippers <- ifelse(is.na(inddata1$i15_shared_nailclippers),0,inddata1$i15_shared_nailclippers) 

inddata1 <- inddata1 %>% 
  dplyr::mutate(i15_shared_nailclippers_f=factor(
    inddata1$i15_shared_nailclippers, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
    labels = c("No", "Yes", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i8_transfusion_f=factor(
    inddata1$i8_transfusion, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

# variable for any shared hh objects
inddata1 = inddata1 %>%
  mutate(sharedhhobj = case_when(
    i14_shared_razor == 0 & i15_shared_nailclippers == 0 & i13_shared_toothbrush == 0 ~ 0, #
    i14_shared_razor >= 1 | i15_shared_nailclippers >= 1 | i13_shared_toothbrush >= 1 ~ 1 #
) %>% as.factor() ) # save as factor for analysis
table(inddata1$sharedhhobj, useNA = "always")

inddata1$i8a_transfusion_number <- as.numeric(inddata1$i8a_transfusion_number)
table(inddata1$i8a_transfusion_number)

inddata1 = inddata1 %>%
  mutate(transfus_num = case_when(
    is.na(i8a_transfusion_number) ~ 0, #
    i8a_transfusion_number == 1 ~ 1, #
    i8a_transfusion_number == 2 ~ 2, #
    i8a_transfusion_number == 3 ~ 3, #
    i8a_transfusion_number >= 4 ~ 9 # 4 or more or refused (1 refuse)
  ) %>% as.factor() ) # save as factor for analysis
table(inddata1$transfus_num, useNA = "always")

inddata1 = inddata1 %>%
  mutate(transfus_num2 = case_when(
    is.na(i8a_transfusion_number) ~ 0, #
    i8a_transfusion_number == 1 ~ 1, #
    i8a_transfusion_number >= 2 ~ 2 # 2 or more or refused (1 refuse)
  ) %>% as.factor() ) # save as factor for analysis
table(inddata1$transfus_num2, useNA = "always")

inddata1 = inddata1 %>%
  mutate(
      trans_bin = case_when(
        i8_transfusion == 1 ~ 1, # 1 or more + 4 refused
        i8_transfusion == 99 ~ 1,  # 1 or more + 4 refused
        is.na(i8_transfusion) ~ 0, # one missing (combine with none)
        i8_transfusion == 0 ~ 0) %>% as.factor()) 
table(inddata1$trans_bin, inddata1$i8_transfusion_f, useNA = "always")

# missing transfusion
inddata1 %>% filter(is.na(i8_transfusion)) %>% reframe(hrhhid, redcap_repeat_instance,hr3_relationship_f, i8_transfusion,i8a_transfusion_number)
inddata1 %>% filter(i8_transfusion==99) %>% reframe(hrhhid, hr3_relationship_f,age_combined, i8_transfusion,i8a_transfusion_number)

inddata1 <- inddata1 %>% 
  dplyr::mutate(i9_iv_drug_use_f=factor(
    inddata1$i9_iv_drug_use, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i10_street_salon_f=factor(
    inddata1$i10_street_salon, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

inddata1 = inddata1 %>%
  mutate(i10_street_salon_bin = case_when(
      i10_street_salon == 1 ~ 1, # 1 = uses street salons or refuses
      i10_street_salon == 99 ~ 1,  # 1 = uses street salons or refuses
      is.na(i10_street_salon) ~ 0, # one missing (combine with none)
      i10_street_salon == 0 ~ 0) %>% as.factor()) 
table(inddata1$i10_street_salon_bin, inddata1$i10_street_salon, useNA = "always")

inddata1$street_salon_permo <- as.numeric(inddata1$i10a_street_salon_number)
table(inddata1$street_salon_permo, useNA = "always")

inddata1 = inddata1 %>%
  mutate(street_salon_permo_4 = case_when(
    street_salon_permo > 4 ~ 4, # 1 = uses street salons or refuses
    i10_street_salon_f == "Refused" ~ 4,
    #street_salon_permo  4 ~ 4, # 1 = uses street salons or refuses
    TRUE ~ street_salon_permo) %>% as.factor()) 
table(inddata1$street_salon_permo_4, inddata1$i10_street_salon_f ,useNA = "always")


inddata1 <- inddata1 %>% 
  dplyr::mutate(i11_manucure_f=factor(
    inddata1$i11_manucure, 
    levels = c(0, 1, 99),
  #  labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i12_food_first_chew_f=factor(
    inddata1$i12_food_first_chew, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))

table(inddata1$i13_shared_toothbrush, useNA = "always") # some missing
#which ones missing
inddata1 %>% filter(is.na(i13_shared_toothbrush)) %>% reframe(hrhhid, age_combined,hr4_sex_f, hr3_relationship_f)
# use household questions on toothbrushes to assign value for individual missing
inddata1 %>% filter(hrhhid == "7425") %>%  reframe(hrhhid, age_combined,hr4_sex_f, hr3_relationship_f,i13_shared_toothbrush)
# no one else in 2007 shares toothbrush, assign the missing to 0
inddata1 %>% filter(hrhhid == "4162") %>%  reframe(hrhhid, age_combined,hr4_sex_f, hr3_relationship_f,i13_shared_toothbrush)
# spouse and youngest in 2038 share, assign sharing to index mother

# reassign the missing
inddata1$i13_shared_toothbrush <- ifelse(is.na(inddata1$i13_shared_toothbrush) & inddata1$hrhhid == "7425",0,inddata1$i13_shared_toothbrush) 
inddata1$i13_shared_toothbrush <- ifelse(is.na(inddata1$i13_shared_toothbrush) & inddata1$hrhhid == "4162",1,inddata1$i13_shared_toothbrush) 

inddata1 <- inddata1 %>% 
  dplyr::mutate(i13_shared_toothbrush_f=factor(
    inddata1$i13_shared_toothbrush, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))
addmargins(table(inddata1$i13_shared_toothbrush_f))

# traditional scarring - only one refused (an index mother), combine with yes

inddata1 <- inddata1 %>% 
  dplyr::mutate(i16_traditional_scarring_f=factor(
    inddata1$i16_traditional_scarring, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes/Refused", "Yes/Refused"))) # give yes and refused same coding
table(inddata1$i16_traditional_scarring_f)

inddata1 <- inddata1 %>% 
  dplyr::mutate(i17_tattoo_f=factor(
    inddata1$i17_tattoo, 
    levels = c(0, 1, 99),
    #    labels = c("Non", "Oui", "Refusé")))
    labels = c("No", "Yes", "Refused")))
addmargins(table(inddata1$i17_tattoo_f, useNA = "always"))

inddata1 = inddata1 %>%
  mutate(i17_tattoo_bin = case_when(
    i17_tattoo == 1 ~ 1, # 1 = uses street salons or refuses
    i17_tattoo == 99 ~ 1,  # 1 = uses street salons or refuses
    is.na(i17_tattoo) ~ 0, # one missing (combine with none)
    i17_tattoo == 0 ~ 0) %>% as.factor()) 
table(inddata1$i17_tattoo_bin, inddata1$i17_tattoo, useNA = "always")

inddata1 <- inddata1 %>% 
  dplyr::mutate(i25_sex_hx_receive_money_f=factor(
    inddata1$i25_sex_hx_receive_money, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))
addmargins(table(inddata1$i25_sex_hx_receive_money_f, useNA = "always"))

inddata1 <- inddata1 %>% 
  dplyr::mutate(i26_sex_hx_given_money_f=factor(
    inddata1$i26_sex_hx_given_money, 
    levels = c(0, 1, 99),
#    labels = c("Non", "Oui", "Refusé")))
labels = c("No", "Yes", "Refused")))
addmargins(table(inddata1$i26_sex_hx_given_money_f, useNA = "always"))

inddata1 = inddata1 %>%
  mutate(transactionalsex = case_when(
    i26_sex_hx_given_money >= 1 |  i25_sex_hx_receive_money >= 1 ~ 1, # has engaged or refused to answer to give/receive sex
    i26_sex_hx_given_money == 0 &  i25_sex_hx_receive_money == 0 ~ 0, # refused = own category
     TRUE ~ NA_real_
  ) %>% as.factor())
table(inddata1$transactionalsex, inddata1$i26_sex_hx_given_money,useNA = "always")
table(inddata1$transactionalsex, inddata1$i25_sex_hx_receive_money,useNA = "always")

#indic for indexmom/offspring----------------
inddata1$indexmom_indic <- ifelse(inddata1$hr3_relationship==1,1,0)
inddata1$directoff <- ifelse(inddata1$hr3_relationship==3,1,0) 

inddata1 <- inddata1 %>% 
  dplyr::mutate(indexmom=factor(
    inddata1$indexmom_indic, 
    levels = c(0, 1),
    # labels = c("Household member", "Index mother")))
    labels = c("Household member", "Index mother")))

inddata1 = inddata1 %>%
  mutate(hhmemcat = case_when(
    hr3_relationship == 1 ~ 2, #hh member is an index mother
    hr3_relationship == 3 ~ 1, #hh member is direct offspring
    TRUE ~ 0 # hh mmeber is all other types of relationships
  ) %>% as.numeric())
addmargins(table(inddata1$hhmemcat, inddata1$indexmom))


inddata1 <- inddata1 %>% 
  dplyr::mutate(hhmemcat_f=factor(
    inddata1$hhmemcat, 
    levels = c(0, 1, 2),
    # labels = c("Household member", "Index mother")))
    labels = c("Other household member","Direct offspring" ,"Index mother")))

# add male partner category
inddata1 = inddata1 %>%
  mutate(hhmemcat_4 = case_when(
    hr3_relationship == 1 ~ 3, #hh member is an index mother
    hr3_relationship == 3 ~ 2, #hh member is direct offspring
    hr3_relationship == 2 ~ 1, #hh member is male partner
    TRUE ~ 0 # hh mmeber is all other types of relationships
  ) %>% as.numeric())
inddata1 <- inddata1 %>% 
  dplyr::mutate(hhmemcat_4_f=factor(
    inddata1$hhmemcat_4, 
    levels = c(0, 1, 2,3),
    labels = c("Other household member","Male partner","Direct offspring" ,"Index mother")))

table(inddata1$indexmom_indic, useNA = "always")
table(inddata1$indexmom, useNA = "always")
table(inddata1$hhmemcat_f, useNA = "always")
table(inddata1$hhmemcat_4_f, useNA = "always")

#Perprotocol analysis (index mother's status at enrollment not ANC screening)----------
sensdefs <- inddata1 %>% filter(indexmom_indic==1) %>% 
  mutate(perprot_h10 = case_when( # using enrollment test results
  i27a_rdt_result == 1 ~ 1,
  i27a_rdt_result == 0 ~ 0,
  is.na(i27a_rdt_result) ~ 9
  ),
  anypos = case_when( # pos at either timepoint
    h10_hbv_rdt==1 | i27a_rdt_result==1 ~ 1,
    h10_hbv_rdt==0 & i27a_rdt_result==0 ~ 0
  ),
  onlypos = case_when( # pos at both timepoints (better reflection of chronic infxn)
    h10_hbv_rdt==1 & i27a_rdt_result==1 ~ 1,
    h10_hbv_rdt==0 | i27a_rdt_result==0 ~ 0
  ))

table(sensdefs$perprot_h10)
table(sensdefs$anypos)
table(sensdefs$onlypos)

inddata1 <- full_join(inddata1, sensdefs[, c("hrhhid", "perprot_h10","anypos","onlypos")], by = c("hrhhid"))

# cross tab of different exposure categorizations
table(inddata1$perprot_h10, inddata1$h10_hbv_rdt, useNA = "always")

inddata1 <- inddata1 %>% 
  dplyr::mutate(perprot_h10_f=factor(
    inddata1$perprot_h10, 
    levels = c(0, 1),
    labels = c("Unexposed (per prot)", "Exposed (per prot)")))
#labels = c("Negative", "Positive")))

serostatchange <- sensdefs %>% filter(h10_hbv_rdt != perprot_h10) %>% dplyr::select("hrhhid", "h10_hbv_rdt","perprot_h10")
table(serostatchange$hrhhid)
serostatchange$serostatchange <- 1 # any change
serostatchange$serochangedir <- ifelse(serostatchange$h10_hbv_rdt==0,"incident","cleared")
table(serostatchange$serochangedir)

# add this identifier back onto main datasets for households with serostatus change serostatchange 
inddata1 <- merge(inddata1, serostatchange[,c("hrhhid","serostatchange","serochangedir")],by = "hrhhid", all.x = T )
inddata1$serostatchange <- ifelse(is.na(inddata1$serostatchange),0,inddata1$serostatchange)
inddata1$serochangedir <- ifelse(is.na(inddata1$serochangedir),"no change",inddata1$serochangedir)
table(inddata1$serochangedir)

hhdata1 <- merge(hhdata1, serostatchange[,c("hrhhid","serostatchange","serochangedir")],by = "hrhhid", all.x = T )
hhdata1$serostatchange <- ifelse(is.na(hhdata1$serostatchange),0,hhdata1$serostatchange)
hhdata1$serochangedir <- ifelse(is.na(hhdata1$serochangedir),"no change",hhdata1$serochangedir)
table(hhdata1$serochangedir)

# household has another household member positive
inddata1  <- inddata1 %>% 
  dplyr::mutate(hhmempos= case_when(
    indexmom_indic==0 & i27a_rdt_result==1 ~ 1, # ind is not an index mom and result is pos
    TRUE ~ 0) %>% as.numeric())
table(inddata1$hhmempos)

# var for hh level
hhwothpos <- inddata1 %>% group_by(hrhhid) %>% summarise(numotherpos = sum(hhmempos))
view(hhwothpos)
inddata1 <- left_join(inddata1, hhwothpos, by = "hrhhid")
table(inddata1$numotherpos)

# lives with another positive
inddata1$anotherpos <- ifelse(inddata1$totalpositive - inddata1$i27a_rdt_result > 0 , 1,0)
table(inddata1$anotherpos)

#Sexual hx-------------
inddata1 = inddata1 %>%
  mutate(debutsex_all = case_when(
    i22_sex_hx_age_1st < 18 ~ 1,
    i22_sex_hx_age_1st >= 18 ~ 0, # includes refused/dont' know
    TRUE ~ 0
  ) %>% as.factor())

inddata1 = inddata1 %>%
  mutate(debutsex_miss = case_when(
    i22_sex_hx_age_1st < 18 ~ 1,
    i22_sex_hx_age_1st >= 18 &  i22_sex_hx_age_1st < 95 ~ 0,
    TRUE ~ NA_real_
  ) %>% as.factor())

inddata1 = inddata1 %>%
  mutate(debutsex_cat = case_when(
    i22_sex_hx_age_1st < 18 ~ 1,
    i22_sex_hx_age_1st >= 18 & i22_sex_hx_age_1st < 95 ~ 0,
    i22_sex_hx_age_1st >= 95 ~ 2,
    TRUE ~ NA_real_) %>% as.factor(),
    debutsex_indic = case_when(
      i22_sex_hx_age_1st < 18 ~ 1,
      i22_sex_hx_age_1st >= 18 & i22_sex_hx_age_1st < 95 ~ 0,
      i22_sex_hx_age_1st >= 95 ~ 1, # refused/don't know = with "higher" risk <18 
      TRUE ~ NA_real_) %>% as.factor())
# check grouping of don't know age/refused to answer age of sexual debut
inddata1 %>% filter(indexmom_indic==1) %>% count(i27a_rdt_result_f,debutsex_cat,i22_sex_hx_age_1st)
# more HBsAg+ index mothers refused/didin't know age of sexual debut - shouldn't group with older age 
addmargins(table(inddata1$debutsex_cat, inddata1$i27a_rdt_result_f, useNA = "always"))
addmargins(table(inddata1$debutsex_indic, inddata1$i27a_rdt_result_f, useNA = "always"))

inddata1 <- inddata1 %>% 
  dplyr::mutate(debutsex_cat=factor(
    inddata1$debutsex_cat, 
    levels = c(0, 1, 2),
    #  labels = c("Non", "Oui", "Refusé")))
    labels = c( "≥18","<18", "Refused/don't know")))
table(inddata1$debutsex_cat)

# Final list of sexual hx questions: debutsex_cat, partner3mo_bin, newpartner3mo_indic
# other variables are left to show how they were calculated

# original var of  partners:
# i23_sex_hx_part_past3mo: # sexual partners in last 3 mo
# i23a_sex_hx_past3mo_num: # of these who are new
# i24_sex_hx_part_past1yr: # sexual partners in last year
# i24a_sex_hx_past1yr_num: # of these who are new

# number of partners in last 3 months
inddata1$i23_sex_hx_part_past3mo <- as.numeric(inddata1$i23_sex_hx_part_past3mo)
class(inddata1$i23_sex_hx_part_past3mo)
table(inddata1$i23_sex_hx_part_past3mo,inddata1$hhmemcat_4_f, useNA = "always") # confirm what 95, 96 mean
# same individuals of value or 95 or 96 for subsequent questions

inddata1$i23_sex_hx_part_past3mo_f <- as.factor(inddata1$i23_sex_hx_part_past3mo)

inddata1 <- inddata1 %>% 
  dplyr::mutate(i23_sex_hx_part_past3mo_f=factor(
    inddata1$i23_sex_hx_part_past3mo_f, 
    levels = c(0, 1, 2,3,5,17,95,96,98,99),
    labels = c("0","1" ,"2","3","5","17","Don't know/refused","Don't know/refused","Don't know/refused","Don't know/refused")))
table(inddata1$i23_sex_hx_part_past3mo_f)

inddata1 = inddata1 %>%
  mutate(part3mo_cat = case_when(
    i23_sex_hx_part_past3mo > 1 & i23_sex_hx_part_past3mo < 99 ~ 2, #more than 1 sexual partner or don't know
    i23_sex_hx_part_past3mo == 99  ~ 9, # refused = own category
    i23_sex_hx_part_past3mo == 1 ~ 1, # make one or 0 sexual partners the reference group - no separate
    i23_sex_hx_part_past3mo == 0 ~ 0, # make one or 0 sexual partners the reference group - no separate
    TRUE ~ NA_real_
  ) %>% as.factor())
table(inddata1$part3mo_cat, inddata1$hhmemcat_4_f,useNA = "always")
# don't know and refused together

inddata1 <- inddata1 %>% 
  dplyr::mutate(part3mo_cat=factor(
    inddata1$part3mo_cat, 
    levels = c(0, 1, 2,9),
    labels = c("0","1" ,"More than 1","Refused")))
table(inddata1$part3mo_cat)

inddata1 = inddata1 %>%
  mutate(partner3mo_bin = case_when(
    i23_sex_hx_part_past3mo > 1 ~ 1, # more than 1, refused, don't know
    i23_sex_hx_part_past3mo <= 1 ~ 0, # 0 or 1 sexual partners
    TRUE ~ NA_real_
  ) %>% as.factor())
table(inddata1$partner3mo_bin,inddata1$i23_sex_hx_part_past3mo ,useNA = 'always')
table(inddata1$partner3mo_bin,inddata1$part3mo_cat, useNA = 'always')

# new partners in last 3 months
inddata1$i23a_sex_hx_past3mo_num <- as.numeric(inddata1$i23a_sex_hx_past3mo_num)
table(inddata1$i23a_sex_hx_past3mo_num, useNA = "always") # figure out what 95 means

# recode missing new partners in last 3mo
inddata1 = inddata1 %>%
  mutate(newsex3mo_recode = case_when(
    i23a_sex_hx_past3mo_num == 95 ~ 99, # all dont' know and missing as single category
    !is.na(i23a_sex_hx_past3mo_num) ~ i23a_sex_hx_past3mo_num, # if not missing, leave as is
    is.na(i23a_sex_hx_past3mo_num) & i23_sex_hx_part_past3mo == 0 ~ 0, # if total in last 3mo is 0, recode new as 0
    is.na(i23a_sex_hx_past3mo_num) & i23_sex_hx_part_past3mo == 1 ~ 0, # going to assume if only 1 in last 3 month, not new
    is.na(i23a_sex_hx_past3mo_num) & i23_sex_hx_part_past3mo >= 95 ~ 99, # if total in last 3mo is refused or don't know, recode as don't know/missing
    TRUE ~ NA_real_
  ) %>% as.numeric())
table(inddata1$newsex3mo_recode)

inddata1$i23a_sex_hx_past3mo_num_f <- as.factor(inddata1$newsex3mo_recode)

inddata1 <- inddata1 %>% 
  dplyr::mutate(i23a_sex_hx_past3mo_num_f=factor(
    inddata1$i23a_sex_hx_past3mo_num_f, 
    levels = c(0, 1, 2,95, 99),
    # labels = c("Household member", "Index mother")))
    labels = c("No new sexual partners","1 new" ,"More than 1/Don't know","More than 1/Don't know","Refused")))
table(inddata1$i23a_sex_hx_past3mo_num_f)
table(inddata1$i23a_sex_hx_past3mo_num)

inddata1 = inddata1 %>%
  mutate(partnew3mo_cat = case_when(
    newsex3mo_recode > 0 & newsex3mo_recode < 95 ~ 1, # 
    newsex3mo_recode >= 95  ~ 9, # 
    newsex3mo_recode <= 0 ~ 0,
    TRUE ~ NA_real_) %>% as.factor(),
    newpartner3mo_indic = case_when(
      newsex3mo_recode <= 0 ~ 0, # none new
      newsex3mo_recode > 0 ~ 1, #at least one new, don't know, refused
      TRUE ~ NA_real_) %>% as.factor())

table(inddata1$partnew3mo_cat, useNA = "always")
table(inddata1$newsex3mo_recode, useNA = "always")
table(inddata1$newpartner3mo_indic,inddata1$newsex3mo_recode, useNA = "always")

#  partners in last year
inddata1$i24_sex_hx_part_past1yr <- as.numeric(inddata1$i24_sex_hx_part_past1yr)
inddata1$i24_sex_hx_part_past1yr_f <- as.factor(inddata1$i24_sex_hx_part_past1yr)

inddata1 = inddata1 %>%
  mutate(part12mo_cat = case_when(
    i24_sex_hx_part_past1yr > 1 & i24_sex_hx_part_past1yr < 99 ~ 2, #DONT KNOW WITH >1
    i24_sex_hx_part_past1yr == 99  ~ 9, # REFUSED AS OWN CATEGORY
    i24_sex_hx_part_past1yr == 1 ~ 1,
    i24_sex_hx_part_past1yr == 0 ~ 0,
    TRUE ~ NA_real_
  ) %>% as.factor())
table(inddata1$part12mo_cat)

inddata1 <- inddata1 %>% 
  dplyr::mutate(part12mo_cat=factor(
    inddata1$part12mo_cat, 
    levels = c(0, 1, 2, 9),
    # labels = c("Household member", "Index mother")))
    labels = c("No sexual partners","1","More than 1/Don't know","Refused")))
table(inddata1$part12mo_cat)

inddata1 = inddata1 %>%
  mutate(partner12mo_bin = case_when(
    i24_sex_hx_part_past1yr > 1 ~ 1, # more than 1, refused, don't know
    i24_sex_hx_part_past1yr <= 1 ~ 0, # 0 or 1 sexual partners
    TRUE ~ NA_real_
  ) %>% as.factor())
table(inddata1$partner12mo_bin,inddata1$i24_sex_hx_part_past1yr ,useNA = 'always')

# new partners in last year
inddata1$i24a_sex_hx_past1yr_num <- as.numeric(inddata1$i24a_sex_hx_past1yr_num)
inddata1$i24a_sex_hx_past1yr_num_f <- as.factor(inddata1$i24a_sex_hx_past1yr_num)
table(inddata1$i24a_sex_hx_past1yr_num_f)

# recode missing new partners in last 3mo
inddata1 = inddata1 %>%
  mutate(newsex12mo_recode = case_when(
    i24a_sex_hx_past1yr_num >= 95 ~ 99, # all dont' know and missing as single category
    !is.na(i24a_sex_hx_past1yr_num) ~ i24a_sex_hx_past1yr_num, # if not missing, leave as is
    is.na(i24a_sex_hx_past1yr_num) & i24_sex_hx_part_past1yr == 0 ~ 0, # if total in last 3mo is 0, recode new as 0
    is.na(i24a_sex_hx_past1yr_num) & i24_sex_hx_part_past1yr == 1 ~ 0, # going to assume if only 1 in last 3 month, not new
    is.na(i24a_sex_hx_past1yr_num) & i24_sex_hx_part_past1yr > 95 ~ 99, # if total in last 3mo is refused or don't know, recode as don't know/missing
   # i24a_sex_hx_past1yr_num ==98 ~ 99, # all dont' know and missing as single category
    TRUE ~ NA_real_
  ) %>% as.numeric())
table(inddata1$newsex12mo_recode)

inddata1 = inddata1 %>%
  mutate(partnew12mo_cat = case_when(
    newsex12mo_recode > 0 & newsex12mo_recode <95  ~ 1, # AT LEAST ONE
    newsex12mo_recode > 95  ~ 9, # refused
    newsex12mo_recode <= 0 ~ 0, # no new sexual partners
    TRUE ~ NA_real_
  ) %>% as.factor())
table(inddata1$partnew12mo_cat, inddata1$newsex12mo_recode)

inddata1 = inddata1 %>%
  mutate(newpartner12mo_indic = case_when(
  newsex12mo_recode <= 0 ~ 0, # none new
  newsex12mo_recode > 0 ~ 1, #at least one new, don't know, refused
  TRUE ~ NA_real_) %>% as.factor())
table(inddata1$newpartner12mo_indic, inddata1$newsex12mo_recode, useNA = "always")

inddata1 <- inddata1 %>% 
  dplyr::mutate(partnew12mo_cat=factor(
    inddata1$partnew12mo_cat, 
    levels = c(0, 1, 9),
    # labels = c("Household member", "Index mother")))
    labels = c("No new sexual partners","At least 1","Refused/Don't know")))
table(inddata1$partnew12mo_cat)

# marital status
inddata1 = inddata1 %>%
  mutate(maritalrisk = case_when(
    hr8_marital_status == 0  ~ 1, 
    hr8_marital_status == 1  ~ 0, # make married/living together the referent group 
    hr8_marital_status > 1  ~ 2,# divorced/sep/widowed/N/A - not sure what N/A is
    TRUE ~ NA_real_
  ) %>% as.factor())

inddata1 <- inddata1 %>% 
  dplyr::mutate(maritalrisk_f=factor(
    inddata1$maritalrisk, 
    levels = c(0, 1, 2),
    labels = c("Married/living together","Not married","Divorced/Widowed")))

# add index mother age to both datasets-------
moms <- inddata1 %>% group_by(hrhhid) %>% filter(hr3_relationship == 1) # %>% rename(indexmotherage = age_combined)
moms$indexmotherage <- moms$age_combined
hhdata1 <- left_join(hhdata1, moms[, c("hrhhid", "indexmotherage")], by = "hrhhid")
# drop if repeat
#inddata1 <- inddata1 %>% select(-c(indexmotherage.x,indexmotherage.y))

inddata1 <- left_join(inddata1, hhdata1[, c("hrhhid", "indexmotherage")], by = "hrhhid")
table(inddata1$indexmotherage)

# diff between current age and age of sexual debut
table(moms$age_combined, moms$i22_sex_hx_age_1st, useNA = "always")


inddata1$agesincedebut = ifelse(inddata1$i22_sex_hx_age_1st < 95, inddata1$age_combined - inddata1$i22_sex_hx_age_1st, NA_real_)
hist(inddata1$agesincedebut)
# factor var for refused/answered/NA
inddata1$agesincedebut_ans <- ifelse(inddata1$i22_sex_hx_age_1st > 95, "Refused/Don't know", ifelse(!(is.na(inddata1$agesincedebut)),"Answered",NA_real_))
table(inddata1$agesincedebut_ans, useNA = "always")

# rerun again after adding the new variable for age since sexual debut
moms <- inddata1 %>% group_by(hrhhid) %>% filter(hr3_relationship == 1) # %>% rename(indexmotherage = age_combined)

# distribution of age of sexual debut
ggplot(moms)+
  geom_histogram(aes(x=i22_sex_hx_age_1st, fill=h10_hbv_rdt_f))
# stats on mothers since sexual debut
moms %>% filter(!is.na(i22_sex_hx_age_1st) &i22_sex_hx_age_1st<95) %>%  group_by(h10_hbv_rdt_f) %>% summarise(mean(i22_sex_hx_age_1st), median(i22_sex_hx_age_1st))
table(moms$agesincedebut_ans, moms$h10_hbv_rdt_f,useNA = "always")

moms %>% filter(i23_sex_hx_part_past3mo <95) %>% 
  ggplot()+
  geom_histogram(aes(x=agesincedebut, fill=as.factor(h10_hbv_rdt_f)))+
  facet_wrap(~i23_sex_hx_part_past3mo)

# age difference variables----------
## child's age - diff with index mother's----
inddata1 <- inddata1 %>%  
  mutate(agediff = case_when(
    hr3_relationship == 3  ~ indexmotherage - age_combined, #calculate this variable only if the person is the direct offspring
    TRUE ~ NA_real_
  ) %>% as.numeric())
table(inddata1$agediff) #n=7 with grandchildren, might as well verify all

# table(inddata1$agediff, useNA = "always") # check this calculated correctly
# age difference between grandchildren and index mother
inddata1 <- inddata1 %>%  
  mutate(agediff_grands = case_when(
    hr3_relationship == 5  ~ indexmotherage - age_combined, #calculate this variable only if the person is the direct offspring
    TRUE ~ NA_real_
  ) %>% as.numeric())
table(inddata1$agediff_grands) #n=3 with grandchildren, might as well verify all

# additional variables - should add these to the cleaned dataset that includes new enrollments

#.................................
# now look at households with at least one direct offspring in levels 0/1 vs hh with only group 2
# only for direct offspring: hr3_relationship == 3
diroff <- inddata1 %>% filter(hr3_relationship == 3) %>% select("hrhhid", "pid","h10_hbv_rdt","hr3_relationship", "i27a_rdt_result","i27a_rdt_result_f", "age_combined", "age_cat")
table(diroff$i27a_rdt_result_f, useNA = "always")

# count of HBV+ direct offspring in hh
diroffhh <- diroff %>% group_by(hrhhid) %>% summarise(hbvposdiroff = sum(i27a_rdt_result))
table(diroffhh$hbvposdiroff)

# understand NAs of hbvposdiroff
nas <- diroffhh %>% filter(is.na(hbvposdiroff))
nasid <- inddata1 %>% filter((hrhhid %in% nas$hrhhid))
table(nasid$hrhhid, nasid$i27_rdt_notdone_reason)
# unable to get sample - set hbvposdiroff==0 in this case
diroffhh$hbvposdiroff <- ifelse(is.na(diroffhh$hbvposdiroff),0,diroffhh$hbvposdiroff)

# indicator for has direct offspring
diroffhh$hasdiroff <- 1

#hbvposdiroff (number of positive direct offspring in hh), hasdiroff (indicator var for if hh has direct offspring)
#put these variables back onto hh and indd datasets
inddata1 <- left_join(inddata1, diroffhh,  by = "hrhhid")
hhdata1 <- left_join(hhdata1, diroffhh, by = "hrhhid")

# for those without diroff, set value to 0 to both variables
table(inddata1$hasdiroff, useNA = "always")
hhdata1$hasdiroff <- ifelse(is.na(hhdata1$hasdiroff), 0,hhdata1$hasdiroff)
inddata1$hasdiroff <- ifelse(is.na(inddata1$hasdiroff), 0,inddata1$hasdiroff)
table(inddata1$hbvposdiroff, useNA = "always")
hhdata1$hbvposdiroff <- ifelse(is.na(hhdata1$hbvposdiroff), 0,hhdata1$hbvposdiroff)
inddata1$hbvposdiroff <- ifelse(is.na(inddata1$hbvposdiroff), 0,inddata1$hbvposdiroff)

# save clean and remove those added during fogarty---------------------------

# ind_clean is cleaned dataset including those added during fogarty visits (date of enrollment is only on household survey so all new ones have original household date )
ind_clean <- inddata1

ind1006 <- readRDS(here("Data","ind1006"))

inddata1 <- subset(inddata1, (inddata1$pid %in% ind1006$pid2))
fognewenroll <- subset(ind_clean, !(ind_clean$pid %in% ind1006$pid))

# summary of datasets:.......................
# inddata1 - dataset to use for analysis because all code was written using this name
# ind1006 - saved list of pids of the last download before fogarty began (since the new enrollments don't have dates-only on original hh enrollment data)
# ind_clean - cleaned dataset post fogarty (ongoing) that includes new enrollments
# fognewenroll - new enrollments in fogarty (future analysis of fog data)
#...........................................

# Subgroups (moms, diroff,othermember)--------
moms <- inddata1 %>% group_by(hrhhid) %>% filter(hr3_relationship == 1)
directoff <- inddata1 %>% filter(hr3_relationship == 3)
othermember <- inddata1 %>% filter(hhmemcat==0)
men <- othermember %>% filter(hr3_relationship==2)

# indicator for if 'other hh member' (excluding dir off) is positive
oth_poshh <- othermember %>% group_by(hrhhid) %>% summarise(hbvposoth = sum(i27a_rdt_result))
table(oth_poshh$hbvposoth, useNA = "always")
# make indicator
oth_poshh <- oth_poshh %>% mutate(hbvposoth_indic = case_when(hbvposoth >0 ~ 1, hbvposoth==0 ~ 0))
# merge onto individual and household datasets
inddata1 <- left_join(inddata1, oth_poshh, by = "hrhhid")
# households without other household members (ie households with only mothers/children enrolled) will be NA - need to make 0
inddata1$hbvposoth_indic <- ifelse(is.na(inddata1$hbvposoth_indic), 0, inddata1$hbvposoth_indic)
inddata1$hbvposoth <- ifelse(is.na(inddata1$hbvposoth), 0, inddata1$hbvposoth)

# merge onto individual and household datasets
hhdata1 <- left_join(hhdata1, oth_poshh, by = "hrhhid")
# households without other household members (ie households with only mothers/children enrolled) will be NA - need to make 0
hhdata1$hbvposoth_indic <- ifelse(is.na(hhdata1$hbvposoth_indic), 0, hhdata1$hbvposoth_indic)
hhdata1$hbvposoth <- ifelse(is.na(hhdata1$hbvposoth), 0, hhdata1$hbvposoth)



# IRB checks aug 2023-----
table(inddata1$h10_hbv_rdt)
inddata1 %>% filter(age_combined<18) %>% summarise(count = n())
table(inddata1$i5_pregnancy_f, useNA = "always")

# USING AGE_CAT INSTEAD OF BELOW----
# indicator for cps vaccination based on age: cpshbvprox, cpshbvprox_rev 
# we have an indicator for 14 years or younger, or >= 15 years: agegrp15_2
# should distinguish between enrollments in 2021 vs 2022: for 2021 use 14 or younger, for 2022, use 15 or younger
# also should make 3 groups: 1) >=16 years and definitely not vaccinated in CPS; 2) 12-16 years and possibly vaccinated during roll-out
# and 3) <12 years most likely vaccinated 

# sens analysis with 2021 vs 2022 enrollments, potential birth at beginning vs end of year
inddata1 = inddata1 %>%
  mutate(cpshbvprox = case_when(
    hdov_year < '2022' & age_combined >= 15 ~ 0, # prob not vacc 2021 enroll: 14 oldest born in 2007 so 15yo and above likely wouldn't be vacc
    hdov_year > '2022' & age_combined >= 16 ~ 0, # prob not vacc 2022 enroll: 15 oldest born in 2007 so 16yo and above likely wouldn't be vacc
    hdov_year < '2022' & age_combined < 15 & age_combined >11  ~ 1, # poss vacc during rollout 2021 enroll: 12-14 yos in rollout
    hdov_year > '2022' & age_combined < 16 & age_combined >12 ~ 1, # poss vacc during rollout2 022 enroll: 13-15 yos in rollout
    hdov_year < '2022' & age_combined <= 11 ~ 2, # likely vacc 2021 enroll: <=11 likely vacc
    hdov_year > '2022' & age_combined <= 12 ~ 2, # likely vacc 2022 enroll: <=12 likely vacc
    TRUE ~ 0) %>% as.numeric())
table(inddata1$cpshbvprox, inddata1$age_combined)

#make a reverse of this so referent is likely vaccinated
inddata1$cpshbvprox_rev <- 2 - inddata1$cpshbvprox
table(inddata1$cpshbvprox_rev, useNA = "always")

inddata1 <- inddata1 %>% 
  dplyr::mutate(cpshbvprox_rev=factor(
    inddata1$cpshbvprox_rev, 
    levels = c(0, 1, 2),
    #    labels = c("Non", "Oui", "Refusé")))
    labels = c("Likely vaccinated", "Possibly vaccinated", "Probably not vaccinated")))
table(inddata1$cpshbvprox_rev, useNA = "always")
table(inddata1$cpshbvprox, useNA = "always")

# pos male partner - 2015 has two partners enrolled after fogarty
men <- inddata1 %>% filter(hr3_relationship == 2) %>% select("hrhhid", "pid","hdov","hdov_year","h10_hbv_rdt","hr3_relationship_f", "i3_hiv_pos_test_f","acq_ind","avert_indic","astmh_indic","i27a_rdt_result","i27a_rdt_result_f", 
                                                             "age_combined","age_cat","serostatchange","serochangedir","i23_sex_hx_part_past3mo","i23a_sex_hx_past3mo_num","i24_sex_hx_part_past1yr","i24a_sex_hx_past1yr_num","malepartner")

men <- men %>% mutate(malepartpos = case_when(
  i27a_rdt_result==1 ~ 1,
  i27a_rdt_result==0 ~ 0,
  TRUE ~ NA_real_ ))
table(men$malepartner, men$malepartpos)
inddata1 <- left_join(inddata1, men[,c("hrhhid","malepartpos")],  by = "hrhhid")
table(inddata1$hr3_relationship, inddata1$malepartpos, useNA = "always")
inddata1$malepartpos <- ifelse(is.na(inddata1$malepartpos), 0,inddata1$malepartpos)



