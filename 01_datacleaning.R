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

# inddata1 = subset(inddata1, select = -c(totalpositive.x,totalpositive.y) )


clusters <- inddata1[inddata1$totalpositive >1, ]
table(clusters$hrhhid)

# Cleaning household variables--------------------------------------------------------------------------------

# Roof ----------------------------------------
#roof type - create indicator for modern or not, following Molly DF's DHS coding, 
# reflective of divide by expensive vs cheaper for wealth index
# Modern roof: metal, zinc/cement, tiles/slate, or cement (options=7, 9, 10, 11, 12 )
# 1, 01 = Chaume/palme/feuilles | 2, 02 = Mottes de terre | 3, 03 = Nattes | 4, 04 = Palmes/bambou | 5, 05 = Planches en bois | 6, 06 = Carton | 7, 07 = Tôle | 8, 08 = Bois | 9, 09 = Zinc/fibre de ciment | 10, 10 = Tuiles | 11, 11 = Béton (ciment) | 12, 12 = Shingles | 97, 97 = Autre | 98, 98 = Ne sait pas | 99, 99 = Refusé(e)

hhdata2 = hhdata1 %>%
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
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(hhdata2$modernroof,useNA = "always"))
# select out missing obs to check with Patrick
# do these truly not have rooves?
missingroof <- hhdata2 %>% 
  dplyr::select( "hrhhid","h10_hbv_rdt", "maternity" ,"hdov", starts_with("h1_roof")) %>% 
  filter(is.na(hhdata2$modernroof))

# Walls----------------------------------------
# Modern wall: cement, stone, bricks, or covered adobe (31, 32, 33, 34, 35)

hhdata2 = hhdata2 %>%
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
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(hhdata2$modernwalls,useNA = "always"))
# select out missing obs to check with Patrick
missingwalls <- hhdata2 %>% 
  dplyr::select( "hrhhid","h10_hbv_rdt", "maternity" ,"hdov", starts_with("h1_walls")) %>% 
  filter(h1_walls == 0)

# Flooring----------------------------------------
# Modern floor: vinyl, asphalt, ceramic tiles, cement, or carpet
hhdata2 = hhdata2 %>%
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
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(hhdata2$modernfloor,useNA = "always"))
# no missing

# Windows----------------------------------------
# Modern windows: glass or screens
table(hhdata2$h1_windows, hhdata2$modernwindow,useNA = "always")
table(hhdata2$h1_windows_otherlist, hhdata2$h1_windows_type___98, useNA = "always")

hhdata2 = hhdata2 %>%
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
    TRUE ~ NA_real_ 
  ) %>% as.numeric()
  )
addmargins(table(hhdata2$modernwindow,useNA = "always"))

missingwindow <- hhdata2 %>% 
  dplyr::select( "hrhhid","h10_hbv_rdt", "maternity" ,"hdov", "modernwindow",starts_with("h1_wind")) %>% 
  filter(is.na(modernwindow))

# Holes in wall/house----------------------------------------
addmargins(table(hhdata2$h2_walls_holes, hhdata2$modernwalls, useNA = "always"))

# Modern housing: modernroof==1, modernwalls==1, modernfloor==1, modernwindows==1, h2_walls_holes==0
hhdata2 = hhdata2 %>%
  mutate(modernhousing = case_when(
    modernroof == 1 & modernwalls == 1  & modernfloor == 1  & modernwindow == 1  & h2_walls_holes == 0 ~ 1,
    TRUE ~ 0 
  ) %>% as.numeric()
  )
addmargins(table(hhdata2$modernhousing, useNA = "always"))
addmargins(table(hhdata2$modernhousing, hhdata2$modernfloor ,useNA = "always")) 
addmargins(table(hhdata2$modernhousing, useNA = "always"))


# wealth objects of hh--------------------------------------------------
hhdata2 <- hhdata2 %>% 
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

table(hhdata2$h3_hh_wealth___98) # one reports don't know      
table(hhdata2$h3_hh_wealth___99)       

# wealth objects of members--------------------------------------------------
table(hhdata2$h4_hh_member_wealth___1)       

hhdata2 <- hhdata2 %>% 
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

 
table(hhdata2$h4_hh_member_wealth___98) # one reports don't know      
table(hhdata2$h4_hh_member_wealth___99)       



# #cultivatable land--------------------------------------------------
addmargins(table(hhdata2$h5_cultivable_land, useNA = "always"))
addmargins(table(hhdata2$h5a_cultivable_land_hect, useNA = "always"))

hhdata2 <- hhdata2 %>% 
  dplyr::mutate(h5_cultivable_land_f=factor(
    hhdata1$h5_cultivable_land, 
    levels = c(0, 1, 99),
    labels = c("No", "Yes", "Refused")))

hhdata2 <- hhdata2 %>% 
  dplyr::mutate(h5_cultivable_land_wi = case_when(
    h5_cultivable_land == 0 ~ 0,
    h5_cultivable_land == 1 ~ 1,
    h5_cultivable_land == 99 ~ NA_real_, # assigning to missing so not included in wealth index
    TRUE ~ h5_cultivable_land))

table(hhdata2$h5_cultivable_land_wi, hhdata2$h5a_cultivable_land_hect_num ,useNA = "always")

hhdata2$h5a_cultivable_land_hect_num <- as.numeric(hhdata2$h5a_cultivable_land_hect)
class(hhdata2$h5a_cultivable_land_hect_num)

# Water source------------------------------------------------------
# explore frequency of different water sources
table(hhdata2$h6_water_access___3, hhdata2$h6_water_access___1)
table(hhdata2$h6_water_access___6, hhdata2$h6_water_access___3) 
table(hhdata2$h6_water_access___99) #97 = other, 98 = don't know, 99 = refused
table(hhdata2$h6_water_access_otherlist, hhdata2$h6_water_access___4) #97 = other, 98 = don't know, 99 = refused
# forage, Forage, Forrage = borehole

# not uncommon to have multiple sources - could reflect wealth to have access to multiple. 
# if only or at least one source is piped to house, code as 3 (best)
# if piped to neighbor, code as 2 (next best)
# if communal tap/protected borehole, code as 1 (next best to have some distinction in wealth index)
# if only spring or no source, 0.

hhdata2 = hhdata2 %>%
  mutate(privatewater = case_when(
    h6_water_access___1 == 1 ~ 3, #private piped water
    h6_water_access___2 == 1 ~ 2, # piped water from neighbor
    h6_water_access___3 == 1 ~ 1, # communal tap
    h6_water_access___4 == 1 ~ 1, # protected well
    h6_water_access___6 == 1 ~ 0, #  spring water
    h6_water_access_otherlist == "forage" ~ 1,
    h6_water_access_otherlist == "Forage" ~ 1,
    h6_water_access_otherlist == "Forrage" ~ 1,
    TRUE ~ NA_real_ 
  ) %>% as.numeric()
  )

table(hhdata2$privatewater, useNA = "always")
table(hhdata2$privatewater, test$h6_water_access___6)

# Cooking source------------------------------------------------------
addmargins(table(hhdata2$h7_cooking_fuel___1,hhdata2$h7_cooking_fuel___2,hhdata2$h7_cooking_fuel___3  ))

hhdata2 = hhdata2 %>%
  mutate(cookfuel = case_when(
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 0 ~ 0, # charcoal only
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 0 ~ 1, # charcoal plus gas 
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 1 ~ 1, # charcoal plus electric 
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 1 ~ 2, # all 3
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 0 ~ 3, # gas only, none with this
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 1 ~ 3, # electric only
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 1 ~ 3, # electric or gas 
    TRUE ~ NA_real_ 
  ) %>% as.numeric()
  )
table(hhdata2$cookfuel, useNA = "always")

# People in household per bed--------------------------------------------------
table(hhdata2$n, useNA = "always")
table(hhdata2$h3_hh_wealth_beds, useNA = "always")

# Make wealth index------------------------------------------------------
# subset dataset for prcomp()
wealthpca_hoverhh <- hhdata2[, c("hrhhid", "n","cookfuel", "privatewater", "modernwalls", "modernfloor", "modernroof", "modernwindow",
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

# drop variables
wealthpca_hoverhh <- wealthpca_hoverhh %>% select(-c(h4_hh_member_wealth_animalcart,h4_hh_member_wealth_motorboat, h4_hh_member_wealth_canoe))


# complete case?
# decide for cultivatable land if the 2 NA should be coded as 0
# other variables with missing?

wealthpca_hoverhh_nomiss <- wealthpca_hoverhh[complete.cases(wealthpca_hoverhh), ]

pca_hover <- prcomp(as.matrix(wealthpca_hoverhh_nomiss[, 3:28]), scale=TRUE)
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
var_explained_df <- data.frame(PC= paste0("PC", 1:26),
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
        axis.text = element_text(size = 15)
  )

quantile(pca_R_output$PC1, probs = c(0.25, 0.5,0.75, 1), na.rm = T)

# Wealth variable used in Table 1
# when enrollment is complete, update these percentiles
pca_R_output$wealth_R <- as.numeric(0)
pca_R_output = pca_R_output %>%
  mutate(wealth_R = case_when(
    PC1 <= -1.309517 ~ 0,
    PC1 > -1.309517 & PC1 <= -0.273219 ~ 1,
    PC1 > -0.273219 & PC1 <= 1.287901 ~ 2,
    PC1 > 1.287901  ~ 3,
    TRUE ~ wealth_R
  ) %>% as.numeric()
  )

table(pca_R_output$wealth_R, useNA = "always")
# rename PID variable so it matches enrollment and merge will work in next step
pca_R_output <- pca_R_output %>% rename(hrhhid = `wealthpca_hoverhh_nomiss$hrhhid`)
# add new wealth variable back onto main enrollment dataset
hhdata2 <- inner_join(hhdata2, pca_R_output[, c("hrhhid", "wealth_R")], by = c("hrhhid"))
# check new variable
table(hhdata2$wealth_R, hhdata2$maternity, useNA = "always")


# sharing nail clippers---------------------------
table(hhdata2$h8_nail_cutting)

table(hhdata2$h8a_nail_clippers_owned)
table(hhdata2$h8b_nail_filer_owned)

# sharing razors
table(hhdata2$h9_razor)
table(hhdata2$h8a_razer_owned)

# make new shared clipper/razor variables
hhdata2 <- hhdata2 %>% 
  dplyr::mutate(h8_nail_cutting_f=factor(
    hhdata2$h8_nail_cutting, 
    levels = c(0, 1, 98, 99),
    labels = c("No", "Yes", "Don't know", "Refused")))
# count of nail clipper and filers owned
hhdata2$h8a_nail_clippers_owned <- as.factor(as.numeric(hhdata2$h8a_nail_clippers_owned))
hhdata2$h8b_nail_filer_owned <- as.factor(as.numeric(hhdata2$h8b_nail_filer_owned))

hhdata2 <- hhdata2 %>% 
  dplyr::mutate(h9_razor_f=factor(
    hhdata2$h9_razor, 
    levels = c(0, 1, 98, 99),
    labels = c("No", "Yes", "Don't know", "Refused")))

hhdata2$h8a_razer_owned <- as.factor(as.numeric(hhdata2$h8a_razer_owned))

table(hhdata2$h9_razor_f ,hhdata2$h8a_razer_owned) # what does 96 mean










