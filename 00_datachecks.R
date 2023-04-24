# HOVER paper investigate
library(tidyverse)

ind1006 <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/HOVER/hoverdataanalysis/inddata1006.RDS")
# ind1006 <- inddata1 # on Mar 18 for redownloading hover data

# origin of latest version of the above file:
# was last download before fogarty began with these changes
ind1006 <- ind1006 %>% mutate(pid2 = case_when(
  hrhhid == "HRB-1082" ~ paste0(ind1006$hrhhid,"-0",ind1006$redcap_repeat_instance),
  TRUE ~ pid
))
ind1006$pid_orig <- ind1006$pid
ind1006$pid <-  ind1006$pid2
saveRDS(ind1006, file = "/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/HOVER/hoverdataanalysis/inddata1006.RDS")
# these PIDs were entered incorrectly and needed to be manually changed in the previously downloaded dataset



# english version
hhdata2 <- hhdata2 %>% 
  dplyr::mutate(recruited_f=factor(
    hhdata2$recruited, 
    levels = c("Nouveau screening", "Étude précédente" ),
    labels = c("2022 screening", "2018-19 screening")))
#labels = c("Negative", "Positive")))

table(hhdata2$h10_hbv_rdt_f, hhdata2$recruited_f)
table(hhdata2$modernhousing, hhdata2$recruited_f)

table(hrb1029$hdov)

# Patrick data issues-----------------------------
# hh without index mother
#hh with index mom
nomere <- subset(inddata1, (!(inddata1$hrhhid %in% perprotexpsure$hrhhid))) # all good

# missing HBsAg result
missingtest <- inddata1 %>% filter(is.na(i27a_rdt_result_f)) %>% select("hrhhid","participant_code","h10_hbv_rdt_f",
                                                                        "hr3_relationship_f","age_combined","i27a_rdt_result","i27_rdt_done", "i27_rdt_notdone_reason","hrname_last","hrname_post","hrname_first")

table(inddata1$i27_rdt_notdone_reason, useNA = "always")

# hh w same GPS coords
samegps <- hhdata2[duplicated(hhdata2[c('hxcoord_edit', 'hycoord_edit')]), c("hrhhid","hxcoord_edit","hycoord_edit")]
write_csv(samegps, file = "samegps.csv")
# for moran's i
unique <- hhdata2[!duplicated(hhdata2[c('hxcoord_edit', 'hycoord_edit')]),]


# individuals
inddata1 <- left_join(inddata1, hhdata2[, c("hrhhid", "recruited_f")], by = "hrhhid")
table(inddata1$h10_hbv_rdt_f, inddata1$recruited_f)

inddata1 %>% filter(recruited_f == "2022 screening") %>%  median(as.numeric(age_combined))

# check age variable------------------

inddata1 %>% group_by(hr3_relationship_f) %>% geom_density(age_combined)

ggplot(inddata1, aes(x = age_combined)) + geom_histogram() +facet_wrap( ~ hr3_relationship_f)
ggplot(inddata1, aes(x = age_combined)) + geom_boxplot() +facet_wrap( ~ hr3_relationship_f)
## grandparents------
##but below age 45 - 1 household
inddata1 %>% filter(hr3_relationship == 13 & age_combined < 45) %>% summarise(hrhhid, participant_code, age_combined,hr4_sex_f,hrname_last, hrname_post, hrname_first) # 

## mari-------- 
inddata1 %>% filter(hr3_relationship == 2) %>% summarise(min(age_combined), max(age_combined))
inddata1 %>% filter(hr3_relationship == 2 & age_combined < 22) %>% summarise(hrhhid, participant_code, age_combined,hr4_sex_f,hrname_last, hrname_post, hrname_first) # 

## index mother------
inddata1 %>% filter(hr3_relationship == 1) %>% summarise(min(age_combined), max(age_combined))
inddata1 %>% filter(hr3_relationship == 1 & age_combined < 20) %>% summarise(hrhhid, participant_code, age_combined,hr4_sex_f,hrname_last, hrname_post, hrname_first) # 
inddata1 %>% filter(hr3_relationship == 1 & age_combined > 40) %>% summarise(hrhhid, participant_code, age_combined,hr4_sex_f,hrname_last, hrname_post, hrname_first) # 

## check children's ages for mothers 
## child's age - diff with index mother's----
inddata1 <- inddata1 %>%  
  mutate(agediff = case_when(
  hr3_relationship == 3  ~ indexmotherage - age_combined, #calculate this variable only if the person is the direct offspring
  TRUE ~ NA_real_
 ) %>% as.numeric()
  )
table(inddata1$agediff, useNA = "always") # check this calculated correctly

#evaluate non-logical age differences 
# most concerning
inddata1 %>% filter(hr3_relationship == 3 & agediff < 15) %>% summarise(hrhhid, participant_code, age_combined, indexmotherage, agediff)#,hr4_sex_f,hrname_last, hrname_post, hrname_first) # 

## parents-------
inddata1 %>% filter(hr3_relationship == 6 & age_combined < 50) %>% summarise(hrhhid, participant_code, hr3_relationship_f,hr4_sex_f,age_combined,hrname_last, hrname_post, hrname_first) # 
#look at all members in these
inddata1 %>% filter(hrhhid == "HRB-1002") %>% summarise(hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)
inddata1 %>% filter(hrhhid == "HRB -1012") %>% summarise(hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f,agediff)
inddata1 %>% filter(hrhhid == "HRB-1023") %>% summarise(hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f,agediff)
inddata1 %>% filter(hrhhid == "HRK-2002") %>% summarise(hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f,agediff)

inddata1 %>% filter(hrhhid == "HRB-1029") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f,agediff)

inddata1 %>% filter(hrhhid == "HRB-1052") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, i3_hiv_pos_test,hivhaart)

# check for Sarah on Mar 21
inddata1 %>% filter(hrhhid == "HRK2081") %>% reframe(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, i3_hiv_pos_test,hivhaart,hrname_last, hrname_post, hrname_first)
library(tidyverse)

## grandchildren
inddata1 %>% filter(hr3_relationship == 5) %>% summarise(min(age_combined), max(age_combined))

library(tidyverse)
inddata1 %>% filter(hrhhid == "HRK2022") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)
inddata1 %>% filter(hrhhid == "HRB-1034") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff, i23_sex_hx_part_past3mo, i22_sex_hx_age_1st)

# complete list of prior study women were in 
# check which IDs not in
inddata1_check$pid <- paste0(inddata1_check$hrhhid,"-",inddata1_check$participant_code)
inddata1$pid <- paste0(inddata1$hrhhid,"-",inddata1$participant_code)
new <- subset(inddata1_check, !(inddata1_check$pid %in% inddata1$pid))

new <- inddata1_check

hrb1029 <- inddata1 %>% filter(hrhhid == "HRB-1029") 
hrk2088 <- inddata1 %>% filter(hrhhid == "HRK-2088") 

# %>% select("hrhhid", "pid","h10_hbv_rdt","hr3_relationship", "i27a_rdt_result","i27a_rdt_result_f", "age_combined",

addmargins(table(inddata1$hr4_sex_f))

# check ACQ households
inddata1 %>% filter(acq_ind==1 & hr3_relationship==1) %>% summarise(pid,acq,hrname_last, hrname_post,  hrname_first)


# select family tree option for the 13 exposed households without DO enrolled
nodiroff <- hhdata1 %>% filter(!(hrhhid %in% diroff3$hrhhid))

nodiroff <- inddata1 %>% filter(!(hrhhid %in% diroff3$hrhhid))

nodiroff %>% filter(hrhhid == "HRK2074") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f,agediff)
inddata1 %>% filter(hrhhid == "HRB -1012") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f,agediff)
inddata1 %>% filter(hrhhid == "HRK2050") %>% summarise(hrname_last, hrname_post, hrname_first, age_combined)

inddata1 %>% filter(pid == "HRB -1010-01") %>% summarise(age_combined,hr8_marital_status,i22_sex_hx_age_1st,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num, i26_sex_hx_given_money_f,i25_sex_hx_receive_money_f)

#everyone who refused to answer sexual history
inddata1 %>% filter(debutsex_cat == "2") %>% summarise(pid, hdov, hr3_relationship_f, age_combined,hr4_sex_f)
# only index mothers
inddata1 %>% filter(debutsex_cat == "2"&hr3_relationship==1 ) %>% summarise(pid, hdov, age_combined,hrname_last, hrname_post, hrname_first,i22_sex_hx_age_1st,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num)
inddata1 %>% filter(debutsex_cat == "2"&hr3_relationship==1 ) %>% summarise(pid,i26_sex_hx_given_money_f,i25_sex_hx_receive_money_f)

inddata1 %>% filter(i25_sex_hx_receive_money_f != "Non"&hr3_relationship==1 ) %>% count(maternity,h10_hbv_rdt_f, i27a_rdt_result_f)
inddata1 %>% filter(i25_sex_hx_receive_money_f != "Non"&hr3_relationship==1 ) %>% summarise(pid, hdov, age_combined,hr8_marital_status,i22_sex_hx_age_1st,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num)

inddata1$hr8_marital_status
inddata1 %>% filter(hr3_relationship==3 &i27a_rdt_result_f=="HBsAg+" &age_combined>14) %>% summarise(pid, hdov, age_combined,hr4_sex_f,hr8_marital_status_f,debutsex_cat,i22_sex_hx_age_1st,i23_sex_hx_part_past3mo,part12mo_cat,i24_sex_hx_part_past1yr)
inddata1 %>% filter(hr3_relationship==3 &age_combined>14) %>% summarise(pid, hdov, age_combined,hr4_sex_f,hr8_marital_status_f,debutsex_cat,i22_sex_hx_age_1st,i23_sex_hx_part_past3mo,part12mo_cat,i24_sex_hx_part_past1yr)

inddata1 %>% filter(hr3_relationship==3 &age_combined>14) %>% count(debutsex_cat)
inddata1 %>% filter(hr3_relationship==3 &age_combined>14) %>% count(i24_sex_hx_part_past1yr)

directoff$i22_sex_hx_age_1st
# sexhx of older kids who are positive
directoff %>% filter(age_combined>14) %>% count(i27a_rdt_result_f, i23_sex_hx_part_past3mo)

table(inddata1$i27a_rdt_result_f)
# age dist of sexual debut
NAdf<-
inddata1 %>%
  filter(i22_sex_hx_age_1st>94 | i22_sex_hx_age_1st==0) %>% 
  mutate(nosexdebut = case_when(i22_sex_hx_age_1st==0 ~ 1, TRUE~NA_real_),
         refusesexdebut = case_when(i22_sex_hx_age_1st>94 ~ 1, TRUE~NA_real_)) %>% 
  dplyr::group_by(hhmemcat_4_f, h10_hbv_rdt_f ) %>%
  dplyr::summarise(
          refused=sum(refusesexdebut),
          num_NA=sum(nosexdebut))
    
  
inddata1 %>% filter(i22_sex_hx_age_1st > 94) %>% count(i22_sex_hx_age_1st, hhmemcat_4, h10_hbv_rdt_f)
inddata1 %>% filter(i22_sex_hx_age_1st ==0) %>% count(i22_sex_hx_age_1st, hhmemcat_4, h10_hbv_rdt_f)

# need to add missing and refused
inddata1 %>% filter(i22_sex_hx_age_1st > 0 & i22_sex_hx_age_1st < 95) %>% 
  ggplot()+
    geom_histogram(aes(x=i22_sex_hx_age_1st, fill=i27a_rdt_result_f), bins = 40)+
    labs(x="Age (in years)", fill="HBsAg at enrollment")+
    scale_fill_manual(values = nounprojgraphcol)+
    ggtitle("Age of sexual debut")+
    theme_bw()+
    #geom_text(data=NAdf, aes(x=xcoor, y=ycoor, label=paste(num_NA,"for",name))) +
    facet_wrap(~ fct_rev(h10_hbv_rdt_f) + fct_rev(hhmemcat_f  ))

# changes in database in 2023
nodirlist1 <- test %>% filter(numdiroff==0) %>% count(hrhhid) %>% print(n=Inf)
test1006 <- ind1006 %>% group_by(hrhhid) %>% count(hasdiroff)
nodirlist2 <- test1006 %>% filter(hasdiroff==0) %>% count(hrhhid) %>% print(n=Inf)

discrep <- subset(nodirlist1, (!(nodirlist1$hrhhid %in% nodirlist2$hrhhid))) 
inddata1 %>% filter(hrhhid == "HRK2033") %>% reframe(recruited,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)
ind1006 %>% filter(hrhhid == "HRK2033") %>% reframe(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)

# partners
maleparnew <- test %>% filter(malepartner==1) %>% count(hrhhid) %>% print(n=Inf)
view(maleparnew)
oldmalepart <- ind1006 %>% filter(hr3_relationship==2) %>% reframe(hrhhid)

test1006 <- ind1006 %>% group_by(hrhhid) %>% count(hasdiroff)
nodirlist2 <- test1006 %>% filter(hasdiroff==0) %>% count(hrhhid) %>% print(n=Inf)

discrep_malepart <- subset(oldmalepart, (!(oldmalepart$hrhhid %in% maleparnew$hrhhid))) 
#HRK2009 has two in earlier version of db, later version only 1

inddata1 %>% filter(hrhhid == "HRK2009") %>% summarise(h10_hbv_rdt,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)
ind1006 %>% filter(hrhhid == "HRK2009") %>% summarise(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)


# facet zoom
library(ggforce)
ggplot(df) + 
  aes(x = b, y = a) +
  geom_col() +
  facet_zoom(ylim = c(0, 10))

# samples reformat dataset------
library(readxl)
fogsamp <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/FOGARTY DATASET.xlsx", sheet = "Sheet3")

fogsamp_long <- gather(fogsamp, aliquot, newpid, aPL1:fPL3, factor_key=TRUE)
table(fogsamp_long$sequencing)
fogsamp_long$box <- ifelse(fogsamp_long$sequencing=="positive" & fogsamp_long$aliquot=="cSH1","Box 12","")

fogsamp_long <- fogsamp_long %>% filter(newpid != "")

fogsamp_long$box <- ifelse(fogsamp_long$aliquot=="aPL1","Abbott",fogsamp_long$box)

fogsamp_long <- fogsamp_long[order(fogsamp_long$box, fogsamp_long$orderind, fogsamp_long$aliquot),]

library(writexl)
write_xlsx(fogsamp_long, "/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/fogsamp_long.xlsx")


