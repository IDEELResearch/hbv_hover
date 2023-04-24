# Vaccine hesitancy
# install.packages()

library(readxl)
library(tidyverse)
library(writexl)

# load enrolled pids--------
vhs_pids <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Vaccine hesitancy/VHS Data/vhs_pids.xlsx", sheet = "Sheet2")
vhs_pids$hrhhid <- substr(vhs_pids$`HOVER ID`,5,8)
vhs_pids$participant_code <- substr(vhs_pids$`HOVER ID`,10,11)
vhs_pids$corrID <- paste0("HRK",vhs_pids$hrhhid,"-",vhs_pids$participant_code)

# add demogs
vhs_demog <- inddata1 %>% filter(pid %in% vhs_pids$`HOVER ID`) %>% select(pid, age_combined, hr4_sex_f, hr9_school_gr_f, hr11_religion_f, hr10_occupation_gr_f)
#find which pids had diff formatting
vhs_nomatch <- vhs_pids %>% filter(!(`HOVER ID` %in% vhs_demog$pid))
vhs_nomatch$hrhhid <- substr(vhs_nomatch$`HOVER ID`,5,8)
vhs_nomatch$participant_code <- substr(vhs_nomatch$`HOVER ID`,10,11)
vhs_nomatch$corrID <- paste0("HRK",vhs_nomatch$hrhhid,"-",vhs_nomatch$participant_code)

vhs_nomatch_data <-inddata1 %>% filter(pid %in% vhs_nomatch$corrID) %>% select(pid, age_combined, hr4_sex_f, hr9_school_gr_f, hr11_religion_f, hr10_occupation_gr_f)

vhs_merg <- rbind(vhs_demog,vhs_nomatch_data)

vhs_stillnomatch <- vhs_pids %>% filter(!(`HOVER ID` %in% vhs_merg$pid))
# 2011-03 is duplicated, 2085-06 is duplicated, 2089-07 does not exist


write_xlsx(vhs_merg,"~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Vaccine hesitancy/VHS Data/vhs_merg.xlsx")

# previous -------
vhs <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Vaccine hesitancy/VHS Data/ParrLtudeDhsitation_DATA_2022-08-11_0933.xlsx")
View(vhs)   

class(vhsenrol$participant_code)
vhsenrol <- inddata1 %>% filter(hrhhid %in% vhs$hover_hhid)
# vhsenrol$participant_code <- as.numeric(vhsenrol$participant_code)
#vhsenrol_ind <- vhsenrol %>% filter(participant_code %in% vhs$hover_indid)

vhsenrol <- vhsenrol %>% select("hrhhid","participant_code", "h10_hbv_rdt_f","i27a_rdt_result_f","hrname_last","hrname_post","hrname_first","hr3_relationship_f","age_combined","i39_vaccine_eligible", "i39a_vac_consent","i39_vac_whynot")

# check on hh that haven't been connected to prior study-------
addpriorstudy <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/HOVER/Patrick data updates/HOVER etudes precedentes.xlsx", sheet = "getpriorstudy")

addpriorstudy_names <- left_join(addpriorstudy, moms[, c("hrhhid","h10_hbv_rdt","hrname_last","hrname_post","hrname_first")], by = c("hrhhid"))

library(writexl)
write_xlsx(addpriorstudy_names,"/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/HOVER/Patrick data updates/addpriorstudy_names.xlsx")


# check risk factors in HOVER compared with fogarty-----
library(tidyverse)
HRB1083_check <- inddata1 %>% filter(hrhhid=="HRB-1083")
HRK2015_check <- inddata1 %>% filter(hrhhid=="HRK2015")
HRB1084_check <- inddata1 %>% filter(hrhhid=="HRB-1084")
HRB1082_check <- inddata1 %>% filter(hrhhid=="HRB-1082")
table(HRB1082_check$i16_traditional_scarring_f, HRB1082_check$hr3_relationship_f)
table(HRB1082_check$i27a_rdt_result_f, HRB1082_check$hr3_relationship_f)
table(HRK2015_check$i26_sex_hx_given_money_f, HRK2015_check$hr3_relationship)
table(HRB1083_check$i11_manucure, HRB1083_check$hr3_relationship)

hist(miref$n)

HRK2027_check <- inddata1 %>% filter(hrhhid=="HRK-2027")
table(HRK2027_check$i25_sex_hx_receive_money_f, HRK2027_check$hr3_relationship_f)

inddata1 %>% filter(hrhhid == "HRK-2027") %>% summarise(participant_code,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f)


HRK2034_check <- inddata1 %>% filter(hrhhid=="HRK2034")
summary(HRK2034_check)

HRK2040_check <- inddata1 %>% filter(hrhhid=="HRK2040")
summary(HRK2040_check)
table(HRK2040_check$i8a_transfusion_number, HRK2040_check$age_combined)

hrb1082_check <- inddata1 %>% filter(hrhhid=="HRB-1082")
summary(hrb1082_check)

hrk2081_check <- inddata1 %>% filter(hrhhid=="HRK2081")
summary(hrk2081_check)
table(hrk2081_check$i8_transfusion, hrk2081_check$age_combined)

hrk2049_check <- inddata1 %>% filter(hrhhid=="HRK2049")
summary(hrk2049_check)
table(hrk2049_check$i24a_sex_hx_past1yr_num, hrk2049_check$age_combined)

#other member HIV+
othermember %>% filter(i3_hiv_pos_test==1) %>% summarise(h10_hbv_rdt_f, hr3_relationship_f,hr4_sex_f ) 
table(othermember$i3_hiv_pos_test_f, othermember$hr4_sex_f, othermember$h10_hbv_rdt_f,useNA = "always") #spouse, nephew/niece
table(hhmemb$i3_hiv_pos_test_f, hhmemb$hr3_relationship_f, useNA = "always") #spouse, nephew/niece

# how many Hbsag report a previous hbv pos test
  inddata1 %>% filter(i27a_rdt_result==1) %>% summarise(h10_hbv_rdt_f, hr3_relationship_f,hr4_sex_f,i1_hbv_positive_f ) 
  table(moms$i1_hbv_positive_f, moms$h10_hbv_rdt, useNA="always")
  table(moms$i1_hbv_positive_f, moms$h10_hbv_rdt, useNA="always")

  
# check date of enrollment
inddata1 %>% filter(hrhhid=="HRB-1013") %>% summarise(hdov)

inddata1 %>% filter(hrhhid=="HRB-1013") %>% summarise(hr3_relationship_f,i3_hiv_pos_test)

inddata1 %>% filter(hhmempos==1) %>% group_by(h10_hbv_rdt_f) %>% summarise(hrhhid,oldestnotvacc, allkidsvacc,hr3_relationship_f, hr4_sex_f) %>% print(n = Inf)
inddata1 %>% filter(hhmempos==1) %>% group_by(h10_hbv_rdt_f) %>% summarise(h10_hbv_rdt_f) %>% print(n = Inf)

# Dec 13 check names/data verification--------
## Jolie
# is 1049 MI actually 18 years?
inddata1 %>% filter(hrhhid=="HRB-1049") %>% summarise(hdov,h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# are the children the index mother's children or mari or siblings? is the mother someone else?
inddata1 %>% filter(hrhhid=="HRB -1012") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# is the pere the mari?
inddata1 %>% filter(hrhhid=="HRB-1002") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# is the pere the mari?
inddata1 %>% filter(hrhhid=="HRB-1023") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# grand-child is whose child? is the uncle the brother? 
inddata1 %>% filter(hrhhid=="HRB-1008") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# is the 43-yo aunt the sister? 
inddata1 %>% filter(hrhhid=="HRB-1024") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# does the MI have a 9yo brother?
inddata1 %>% filter(hrhhid=="HRB-1035") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
# ASTMH ID of 1042? received birth dose?
inddata1 %>% filter(hrhhid=="HRB-1042") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)


inddata1 %>% filter(hrhhid=="HRB-1029") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)



#scratch
inddata1 %>% filter(hrhhid=="HRK2043") %>% summarise(h10_hbv_rdt_f,i27a_rdt_result_f,hrname_last,hrname_post,hrname_first,hr3_relationship_fr,age_combined, hr4_sex_f)
inddata1_check %>% filter(hrhhid=="HRB-1083") %>% summarise(i27a_rdt_result,hrname_last,hrname_post,hrname_first,hr3_relationship, hr4_sex)


#check new hover enrollments--------
# correct pid

inddata1$pid <- paste(inddata1$hrhhid,inddata1$participant_code,sep = "-")
inddata1_dc$pid <- paste(inddata1_dc$hrhhid,inddata1_dc$participant_code,sep = "-")

newenrol <- inddata1 %>% filter(!(pid %in% inddata1_dc$pid))
  

# Create and export list of eligibles---------------------------------------------------- 
exposed <- inddata1 %>%
  group_by(hrhhid)%>% 
  summarise(poscase=sum(i27a_rdt_result)) #n=n() if want number by household
#filter(poscase >0)
table(exposed$poscase)

inddata1 <- left_join(inddata1, exposed, by = "hrhhid")
table(inddata1$poscase)

vacc_hest_elig <- inddata1[inddata1$age_combined>=18 & inddata1$i27a_rdt_result==0 & inddata1$poscase>0, c("hrhhid","participant_code","h10_hbv_rdt_f", "hrname_last", "hrname_post","hrname_first","hr3_relationship_f","hr7_age_year","i39a_vac_consent","maternity")]

# vhs elig var
inddata1$vhselig <- ifelse(inddata1$age_combined>=18 & inddata1$i27a_rdt_result==0 & inddata1$poscase>0, 1,0)
table(inddata1$vhselig)


averagesize <- inddata1 %>%
  group_by(hrhhid)%>% 
  summarise(n=n())
mean(averagesize$n)
hhdata1 <- left_join(hhdata1, averagesize, by = "hrhhid")

vacc_hest_elig_binza <- vacc_hest_elig[vacc_hest_elig$maternity=="Binza",]
vacc_hest_elig_kingsani <- vacc_hest_elig[vacc_hest_elig$maternity=="Kingasani",]
library(writexl)
write_xlsx(vacc_hest_elig_binza,"vacc_hest_elig_binza.xlsx")
write_xlsx(vacc_hest_elig_kingsani,"vacc_hest_elig_kingsani.xlsx")

# fogarty selection-----------
HOVER_households_of_interest <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/HOVER households of interest.xlsx", sheet = "List de priorité")
#add avert 
avertnoms <- perprotexpsure %>% filter(hrhhid %in% HOVER_households_of_interest$pid)
avertnoms <- avertnoms %>% select("hrhhid", "hrname_last","hrname_post","hrname_first", "maternity")
write_xlsx(avertnoms,"avertnoms.xlsx")

# hh with neg mom but pos other members
library(tidyverse)
hhwpos <- inddata1 %>% filter(i27a_rdt_result == 1 & indexmom =="Membre de ménage")   

hhwpos <- hhwpos %>% select(c("hrhhid","participant_code", "h10_hbv_rdt_f","i27a_rdt_result_f", "totalpositive","n","hr3_relationship_f"))


# fogarty of interest - Oct 4----------
fogarty_1 <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/HOVER households of interest.xlsx", sheet = "List de priorité")

fogarty_hoverdata <- inddata1 %>% filter(hrhhid %in% fogarty_1$`HOVER PID`)

momnames <- fogarty_hoverdata %>% filter(indexmom_indic==1) %>% select(c("hrhhid","hrname_last","hrname_post","hrname_first"))

miss <- fogarty_1 %>% filter(!(`HOVER PID` %in% momnames$hrhhid))

# find total positives
fogarty_hoverdata %>% group_by(hrhhid) %>%  count(totalpositive) %>% print(n=Inf)

# add names for beginning to contact households

getnames <- inddata1 %>% filter(hrhhid %in% fogpriority$pid) 


table(getnames$indexmom_indic)

# summarize PID, age, n, relationship in index mom, name 
fogarty_hoverdata$pid <- paste(fogarty_hoverdata$hrhhid,fogarty_hoverdata$participant_code,sep = "-")

fogdata <- fogarty_hoverdata %>% select(c("pid","maternity","hr7_age_year","hr7_age_mois" ,"hr3_relationship_f","n","hrname_last","hrname_post","hrname_first"))

fogdata <- fogdata[order(fogdata$pid, fogdata$maternity),]
library(writexl)
write_xlsx(fogdata,"fogdata.xlsx")

## additional households Nov 8
library(readxl)
fogarty_2 <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/HOVER households of interest.xlsx", sheet = "Nov 8 add")
library(tidyverse)
fogarty2_hoverdata <- inddata1 %>% filter(hrhhid %in% fogarty_2$hrhhid)
fogarty2_hoverdata$pid <- paste(fogarty2_hoverdata$hrhhid,fogarty2_hoverdata$participant_code,sep = "-")

fogdata2 <- fogarty2_hoverdata %>% select(c("hrhhid","pid","maternity","hr7_age_year","hr7_age_mois" ,"hr3_relationship_f", "hr4_sex_f","hrname_last","hrname_post","hrname_first"))

fogdata2 <- fogdata2[order(fogdata2$pid, fogdata2$maternity),]
library(writexl)
write_xlsx(fogdata2,"fogdata2.xlsx")

# add hh - size and total pos
fogarty2_hoverdata %>% group_by(hrhhid) %>%  count(totalpositive) %>% print(n=Inf)


# personal tracking, no names, but hbv status
fdatahbv <- fogarty_hoverdata %>% select(c("pid","maternity","hr7_age_year","hr7_age_mois" ,"hr3_relationship_f","n","totalpositive","h10_hbv_rdt_f","i27a_rdt_result_f"))
write_xlsx(fdatahbv,"fdatahbv.xlsx")

# map of Fogarty eligibles---------
fogarty_2 <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/FOGARTY TRACKING DATASET.xlsx", sheet = "import")
# individuals
fogarty_hoverdata_up <- inddata1 %>% filter(hrhhid %in% fogarty_2$hrhhid)
fogarty_hoverdata_up <- fogarty_hoverdata_up %>% select(c("hrhhid","participant_code","hdov","n","totalpositive","maternity","hr7_age_year","hr7_age_mois" ,"hr3_relationship_f","hr4_sex_f"))
fogarty_hoverdata_up <- fogarty_hoverdata_up[order(fogarty_hoverdata_up$hrhhid, fogarty_hoverdata_up$participant_code),]



#map
fogarty_map <- hhdata1 %>% filter(hrhhid %in% fogarty_2$hrhhid) %>% select(c("hrhhid","maternity","n","totalpositive","hxcoord_edit","hycoord_edit"))
# check all IDs matched
nomatch <- fogarty_2 %>% filter(!(hrhhid %in% fogarty_map$hrhhid))

#identify the clusters
fogarty_map$cluster <- ifelse(fogarty_map$totalpositive>1,"group de cas","un seul cas positif")
table(fogarty_map$cluster)
# make ID for map showing hh size
fogarty_map$label <- paste(fogarty_map$hrhhid,fogarty_map$n,sep = ", ")
# make var for relative hh size
fogarty_map$hhsizerel <- (fogarty_map$n)/10

library(sf)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(tmaptools)
library(OpenStreetMap)
# latitudes are below equator, so need to be negative decimal degrees
options(scipen = 999)
#locations of Binza and Kingasani maternity centers
centers <- c("Binza","Kingasani","UPC")
lat <- c(-4.382935, -4.402250, -4.334622)
long <- c(15.261066, 15.408503, 15.2975683)
structures <- as.data.frame(cbind(centers, lat, long))
strugps <- st_as_sf(structures, coords = c("long","lat"), crs= 4326)

fogarty_map_gps = st_as_sf(fogarty_map[!is.na(fogarty_map$hxcoord_edit) &!is.na(fogarty_map$hycoord_edit),], coords = c("hycoord_edit", "hxcoord_edit"), crs = 4326)  
# interactive map
tmap_mode("view")
 #tmap_mode("plot") # or view
tm_basemap("OpenStreetMap") +
  tm_shape(st_jitter(fogarty_map_gps, factor=0.005)) + 
  tm_dots(id="label", col = "cluster", palette=c('#e2738c','#33374b'), size="hhsizerel")+ # size = 0.25
  tm_text("label", bg.color = "black", ymod=-3, auto.placement = F)+
  tm_shape(strugps)+
  tm_dots(col = "#ffffbf", id="centers", shapes = 14, size = 0.5)

 # tried ceramic library, leafem::addStaticLabels,


# power for proposal --------
# R library(clusterPower) package
library(clusterPower)
# nclusters – the number of clusters per arm
# nsubjects – the mean cluster size (e.g., number of women evaluable per cluster)
cpa.binary(alpha= 0.05, power= NA, nclusters= 156, nsubjects= 2, p1= 0.033, p2= 0.05,ICC= 0.05)

cpa.binary(alpha= 0.05, power= NA, nclusters= 156, nsubjects= 2, p1= 0.033, p2= 0.05,ICC= 0.2)



# Use to check PIDs of each member and get sex to determine son/daughter, niece/nephew, etc.------
# October 27
inddata1 %>% filter(hrhhid == "HRK2034") %>% summarise(participant_code,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f)

# get ACQ PIDS using names
inddata1 %>% filter(hrhhid == "HRB -1010" & hr3_relationship==1) %>% summarise(hrname_first,hrname_post,hrname_last)
inddata1 %>% filter(hrhhid == "HRB-1028" & hr3_relationship==1) %>% summarise(hrname_first,hrname_post,hrname_last)
inddata1 %>% filter(hrhhid == "HRK2008" & hr3_relationship==1) %>% summarise(hrname_first,hrname_post,hrname_last)
inddata1 %>% filter(hrhhid == "HRB-1038" & hr3_relationship==1) %>% summarise(hrname_first,hrname_post,hrname_last)


inddata1 %>% filter(hrhhid == "HRK2043" & hr3_relationship==1) %>% summarise(hrname_first,hrname_post,hrname_last)

# get AVERT IDs: HRK-2022, HRB-1008
inddata1 %>% filter(hrhhid == "HRB-1008" & hr3_relationship==1) %>% summarise(hrname_first,hrname_post,hrname_last)



# HBV knowledge for VHS R&R-------
# eligibles are : inddata1$vhselig

# knowledge questions
# i18, i19, i20

inddata1 <- inddata1 %>% 
  rename(i19_hbv_percep_sympt_none = i19_hbv_percep_sympt___1,
         i19_hbv_percep_sympt_jaun = i19_hbv_percep_sympt___2,
         i19_hbv_percep_sympt_abpain = i19_hbv_percep_sympt___3,
         i19_hbv_percep_sympt_rash = i19_hbv_percep_sympt___4,
         i19_hbv_percep_sympt_dk = i19_hbv_percep_sympt___98,
         i19_hbv_percep_sympt_ref = i19_hbv_percep_sympt___99, #none refused   
         
         i20_hbv_percep_trans_hand = i20_hbv_percep_trans___1,
         i20_hbv_percep_trans_blood = i20_hbv_percep_trans___2,
         i20_hbv_percep_trans_plate = i20_hbv_percep_trans___3,
         i20_hbv_percep_trans_mtct = i20_hbv_percep_trans___4,
         i20_hbv_percep_trans_sex = i20_hbv_percep_trans___5,
         i20_hbv_percep_trans_bf = i20_hbv_percep_trans___6,
         i20_hbv_percep_trans_dk = i20_hbv_percep_trans___98,
         i20_hbv_percep_trans_ref = i20_hbv_percep_trans___99,
         
         i20_hbv_percep_mtct_prev_bd = i20_hbv_percep_mtct_prev___1,
         i20_hbv_percep_mtct_prev_arv = i20_hbv_percep_mtct_prev___2,
         i20_hbv_percep_mtct_prev_dk = i20_hbv_percep_mtct_prev___98,
         i20_hbv_percep_mtct_prev_ref = i20_hbv_percep_mtct_prev___99)

ind_know <- inddata1 %>% 
  dplyr::select("hrhhid", "participant_code","pid","h10_hbv_rdt_f","maternity", "hr3_relationship_f", "age_combined",'i27a_rdt_result_f', "vhselig", starts_with("i18"), starts_with("i19"), starts_with("i20"))
summary(ind_know)
table(ind_know$i20_hbv_percep_mtct_prev_bd,ind_know$i20_hbv_percep_mtct_prev_arv,ind_know$i20_hbv_percep_mtct_prev_dk ,useNA = "always")


# who are those that don't answer any to MTCT - make sure they are all young children
ind_know_mtct0 <- ind_know %>% filter(i20_hbv_percep_mtct_prev_bd==0 &i20_hbv_percep_mtct_prev_arv==0 &i20_hbv_percep_mtct_prev_dk==0 )
# n=296
table(ind_know_mtct0$age_combined)
table(ind_know_mtct0$hr3_relationship_f)
# not all young children but the other fam relationships could be errors that I am hoping to have corrected
# two are 34 and 39 yo--MI and husband?
# 296 largely young children who aren't asked themselves - exclude
# leaves 716. 217 gave a non 0 answer to at least one, 499 said don't know. this is 30% with some knowledge. likely index mothers

ind_know_mtct_denom <- ind_know %>% filter(!(pid %in% ind_know_mtct0$pid))

addmargins(table(ind_know_mtct_denom$vhselig, ind_know_mtct_denom$i20_hbv_percep_mtct_prev_bd))
addmargins(table(ind_know_mtct_denom$vhselig, ind_know_mtct_denom$i20_hbv_percep_mtct_prev_arv))
addmargins(table(ind_know_mtct_denom$vhselig, ind_know_mtct_denom$i20_hbv_percep_trans_hand))


# Quant visualization----------------------------------
vhs_quant <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Vaccine hesitancy/VHS Data/vhs_quant.xlsx", sheet = "vhs_quant")

# save column names for function
vars <- noquote(colnames(vhs_quant)[4:13])
vars <- noquote(paste(vars, collapse=", "))

# save list manually

# vars <- c("conf_prevent_before" ,"conf_prevent_now", "conf_danger_before" ,  "conf_danger_now"  ,"conf_babyhealth_before", "conf_babyhealth_now",    "conf_yourhealth_before", "conf_yourhealth_now" ,   "conf_newrisks_before",   "conf_newrisks_now" )

# attempt by function
sumcount <- function(var){ 
  varsummary <- vhs_quant %>% count(paste0(var)) %>% rename(response = paste0(var)) # summarize stats for one variable
  varsummary$variable <- paste0(var) # paste the variable name in a column
  # extract the subject of the question
  conf_prevent_before <-  as.data.frame(str_match(conf_prevent_before$variable, "_(.*?)_")) %>% select(c(2)) %>% rename(topic = V2) %>% bind_cols(conf_prevent_before)
  # extract the time (before/now of pandemic)
  conf_prevent_before <- conf_prevent_before %>% mutate(time = sapply(strsplit(conf_prevent_before$variable, split= "_", fixed = TRUE), tail, 1L))
  rbind(varsummary) }

sum <- map_dfr(vars,sumcount) 

map(vars, sumcount)

#......................................................................
# manually

# vars <- c("conf_prevent_before" ,"conf_prevent_now", "conf_danger_before" ,  "conf_danger_now"  ,
#       "conf_babyhealth_before", "conf_babyhealth_now","conf_yourhealth_before", "conf_yourhealth_now" ,
      # "conf_newrisks_before",   "conf_newrisks_now" )
vars2 <- as.data.frame(colnames(vhs_quant)[4:13]) 


conf_prevent_before <- vhs_quant %>% count(conf_prevent_before) %>% rename(response = conf_prevent_before)
conf_prevent_before$variable <- "conf_prevent_before"
conf_prevent_before <-  as.data.frame(str_match(conf_prevent_before$variable, "_(.*?)_")) %>% select(c(2)) %>% rename(topic = V2) %>% bind_cols(conf_prevent_before)
conf_prevent_before <- conf_prevent_before %>% mutate(time = sapply(strsplit(conf_prevent_before$variable, split= "_", fixed = TRUE), tail, 1L))

conf_prevent_now <- vhs_quant %>% count(conf_prevent_now) %>% rename(response = conf_prevent_now)
conf_prevent_now$variable <- "conf_prevent_now"
conf_prevent_now <-  as.data.frame(str_match(conf_prevent_now$variable, "_(.*?)_")) %>% select(c(2)) %>% rename(topic = V2) %>% bind_cols(conf_prevent_now)
conf_prevent_now <- conf_prevent_now %>% mutate(time = sapply(strsplit(conf_prevent_now$variable, split= "_", fixed = TRUE), tail, 1L))
view(conf_prevent_now)

conf_danger_before <- vhs_quant %>% count(conf_danger_before) %>% rename(response = conf_danger_before)
conf_danger_before$variable <- "conf_danger_before"
conf_danger_before <-  as.data.frame(str_match(conf_danger_before$variable, "_(.*?)_")) %>% select(c(2)) %>% rename(topic = V2) %>% bind_cols(conf_danger_before)
conf_danger_before <- conf_danger_before %>% mutate(time = sapply(strsplit(conf_danger_before$variable, split= "_", fixed = TRUE), tail, 1L))
view(conf_danger_before)

conf_danger_now <- vhs_quant %>% count(conf_danger_now) %>% rename(response = conf_danger_now)
conf_danger_now$variable <- "conf_danger_now"
conf_danger_now <-  as.data.frame(str_match(conf_danger_now$variable, "_(.*?)_")) %>% select(c(2)) %>% rename(topic = V2) %>% bind_cols(conf_danger_now)
conf_danger_now <- conf_danger_now %>% mutate(time = sapply(strsplit(conf_danger_now$variable, split= "_", fixed = TRUE), tail, 1L))
view(conf_danger_now)

conf_babyhealth_before <- vhs_quant %>% count(conf_babyhealth_before) %>% rename(response = conf_babyhealth_before)
conf_babyhealth_before$variable <- "conf_babyhealth_before"
conf_babyhealth_before <-  as.data.frame(str_match(conf_babyhealth_before$variable, "_(.*?)_")) %>% select(c(2)) %>% rename(topic = V2) %>% bind_cols(conf_babyhealth_before)
conf_babyhealth_before <- conf_babyhealth_before %>% mutate(time = sapply(strsplit(conf_babyhealth_before$variable, split= "_", fixed = TRUE), tail, 1L))
view(conf_babyhealth_before)


all <- rbind(conf_prevent_before,conf_prevent_now,conf_danger_before,conf_danger_now,conf_babyhealth_before)

# plot

