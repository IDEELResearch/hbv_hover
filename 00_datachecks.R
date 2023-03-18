# HOVER paper investigate
library(tidyverse)

inddata1 <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/HOVER/hoverdataanalysis/inddata.RDS")
ind1006 <- inddata1 # on Mar 18 for redownloading hover data
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


# facet zoom
library(ggforce)
ggplot(df) + 
  aes(x = b, y = a) +
  geom_col() +
  facet_zoom(ylim = c(0, 10))

