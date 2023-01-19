# HOVER paper investigate
library(tidyverse)

inddata1 <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/HOVER/hoverdataanalysis/inddata.RDS")
# english version
hhdata2 <- hhdata2 %>% 
  dplyr::mutate(recruited_f=factor(
    hhdata2$recruited, 
    levels = c("Nouveau screening", "Étude précédente" ),
    labels = c("2022 screening", "2018-19 screening")))
#labels = c("Negative", "Positive")))

table(hhdata2$h10_hbv_rdt_f, hhdata2$recruited_f)
table(hhdata2$modernhousing, hhdata2$recruited_f)

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


## grandchildren
inddata1 %>% filter(hr3_relationship == 5) %>% summarise(min(age_combined), max(age_combined))


inddata1 %>% filter(hrhhid == "HRB-1002") %>% summarise(hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, agediff)

# complete list of prior study women were in 
# check which IDs not in
inddata1_check$pid <- paste0(inddata1_check$hrhhid,"-",inddata1_check$participant_code)
inddata1$pid <- paste0(inddata1$hrhhid,"-",inddata1$participant_code)
new <- subset(inddata1_check, !(inddata1_check$pid %in% inddata1$pid))

new <- inddata1_check

hrb1029 <- inddata1 %>% filter(hrhhid == "HRB-1029") 
# %>% select("hrhhid", "pid","h10_hbv_rdt","hr3_relationship", "i27a_rdt_result","i27a_rdt_result_f", "age_combined",





