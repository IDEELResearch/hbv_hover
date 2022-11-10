# 04 fam tree /mother-offspring analysis
library(tidyverse)

# from 01_datacleaning, we have an indicator for 14 years or younger, or >= 15 years: agegrp15_2
table(inddata1$agegrp15_2)
#should distinguish between enrollments in 2021 vs 2022: for 2021 use 14 or younger, for 2022, use 15 or younger
# also should make 3 groups: 1) >=16 years and definitely not vaccinated in CPS; 2) 12-16 years and possibly vaccinated during roll-out
# and 3) <12 years most likely vaccinated 

hhsincerenew <- (hhdata1 %>% filter(hdov > '2021-10-11'))

# drop
# inddata1 <- inddata1 %>% select(-c(cpshbvprox))

# sens analysis with 2021 vs 2022 enrollments, potential birth at beginning vs end of year
inddata1 = inddata1 %>%
  mutate(cpshbvprox = case_when(
    hdov < '2022-01-01' & age_combined >= 14 ~ 0, # prob not vacc 2021 enroll: 14 oldest born in 2007 so 14yo and above likely wouldn't be vacc
    hdov > '2022-01-01' & age_combined >= 15 ~ 0, # prob not vacc 2022 enroll: 15 oldest born in 2007 so 15yo and above likely wouldn't be vacc
    hdov < '2022-01-01' & age_combined < 14 & age_combined >11  ~ 1, # poss vacc during rollout 2021 enroll: 12-13 yos in rollout
    hdov > '2022-01-01' & age_combined < 15 & age_combined >12 ~ 1, # poss vacc during rollout2 022 enroll: 13-14 yos in rollout
    hdov < '2022-01-01' & age_combined <= 11 ~ 2, # likely vacc 2021 enroll: <=11 likely vacc
    hdov > '2022-01-01' & age_combined <= 12 ~ 2, # likely vacc 2022 enroll: <=12 likely vacc
    TRUE ~ 0
  ) %>% as.numeric()
  )

table(inddata1$cpshbvprox, inddata1$age_combined)

# now look at households with at least one direct offspring in levels 0/1 vs hh with only group 2
# only for direct offspring: hr3_relationship == 3

diroff <- inddata1 %>% filter(hr3_relationship == 3) %>% select("hrhhid", "pid","h10_hbv_rdt","hr3_relationship", "i27a_rdt_result","i27a_rdt_result_f", "age_combined","cpshbvprox")
table(diroff$i27a_rdt_result_f)

# count of HBV+ direct offspring in hh
diroffhh <- diroff %>% group_by(hrhhid) %>% summarise(hbvposdiroff = sum(i27a_rdt_result))
# indicator for has direct offspring
diroffhh$hasdiroff <- 1

#hbvposdiroff, hasdiroff -----
#put this variable back onto hh and indd datasets
inddata1 <- left_join(inddata1, diroffhh,  by = "hrhhid")
hhdata1 <- left_join(hhdata1, diroffhh, by = "hrhhid")


table(inddata1$hasdiroff, useNA = "always")
hhdata1$hasdiroff <- ifelse(is.na(hhdata1$hasdiroff), 0,hhdata1$hasdiroff)
inddata1$hasdiroff <- ifelse(is.na(inddata1$hasdiroff), 0,inddata1$hasdiroff)

# households with direct offspring
hhwdirect <- diroff %>% group_by(hrhhid,h10_hbv_rdt) %>% count()
# exposed vs unexp
table(hhwdirect$h10_hbv_rdt)

# HOW TO EVALUATE cpshbvprox ALL=2 VS NOT 
diroff2 <- diroff %>% group_by(hrhhid) %>% 
  mutate(probvacc = case_when(
    cpshbvprox==2 ~ 0, # here 0 is yes so that we can sum within households in next step and create indicator based on sum=0
    TRUE ~ 1 # at least one child prob/possibly not vacc
  ) %>% as.numeric(),
      defnotvacc = case_when(
        cpshbvprox==0 ~ 1, # here 1 is yes so we can sum across hh members and use >=1 vs 0 to make indicator 
        TRUE ~ 0 # at least one child prob/possibly IS vacc
   ) %>% as.numeric(),
  )

table(diroff2$probvacc)
table(diroff2$defnotvacc)


# diroff2 <- diroff2 %>% group_by(hrhhid) %>% summarise(allkidsvacc = sum(probvacc))
#allkidsvacc----------
diroff3 <- diroff2 %>% group_by(hrhhid) %>% summarise(allkidsvacc = ifelse(sum(probvacc)==0,1,0)) #if all kids probably vaccinated, sum will be 0 at hh level - allkidsvacc=1, else hh gets value 0
#oldestdefnotvacc-----
diroff3_pt2 <- diroff2 %>% group_by(hrhhid) %>% summarise(oldestnotvacc = ifelse(sum(defnotvacc)>0,1,0)) # sum in hh - if at least one, make indicator

table(diroff3$allkidsvacc)
table(diroff3_pt2$oldestnotvacc)

# join these indicators back on to ind and hh datasets  
inddata1 <- left_join(inddata1, diroff3[, c("hrhhid","allkidsvacc")],  by = "hrhhid")
hhdata1 <- left_join(hhdata1, diroff3[, c("hrhhid","allkidsvacc")],  by = "hrhhid")

inddata1 <- left_join(inddata1, diroff3_pt2[, c("hrhhid","oldestnotvacc")],  by = "hrhhid")
hhdata1 <- left_join(hhdata1, diroff3_pt2[, c("hrhhid","oldestnotvacc")],  by = "hrhhid")
# drop
# inddata1 <- inddata1 %>% select(-c(oldestnotvacc.x, oldestnotvacc.y))
# hhdata1 <- hhdata1 %>% select(-c(oldestnotvacc.x, oldestnotvacc.y))


# eval pos offspring by vacc status
diroff4 <- left_join(diroff2, diroff3[, c("hrhhid","allkidsvacc")],  by = "hrhhid")

diroffhh <- left_join(diroffhh, diroff3, by = "hrhhid")

diroff4 <- left_join(diroff4, diroffhh[, c("hrhhid","hbvposdiroff")],  by = "hrhhid")
agebypos <- diroff4 %>% group_by(h10_hbv_rdt) %>% filter(hbvposdiroff >0) %>%  summarise(hrhhid, hbvposdiroff,age_combined)


# use for direct offspring analysis

directoff <- left_join(diroff4[, c("pid","probvacc","defnotvacc")], inddata1, by = "pid")
mismatch <- directoff %>% filter(!(pid %in% diroff$pid))

# Summary of new variables:------
# cpshbvprox: 3 categories for likely vacc, poss vacc, prob vacc
# hbvposdiroff: count of HBV+ direct offspring. NAs are either no diroff (n=23) or not tested (n=3)
# hasdiroff: yes/no 1/0 does hh have direct offspring enrolled
# allkidsvacc: hh level indicator for if all dir off in hh likely vacc vs no (incl poss/prob)
# oldestdefnotvacc: indicator for if hh has at least one direct offspring born before CPS had HBV vacc
addmargins(table(inddata1$allkidsvacc, inddata1$hbvposdiroff, useNA = "always"))


addmargins(table(inddata1$allkidsvacc, inddata1$h10_hbv_rdt_f, useNA = "always"))
addmargins(table(hhdata1$allkidsvacc, hhdata1$h10_hbv_rdt_f, useNA = "always"))

# select the households that: have a pos mother, have a kid that might not have received CPS
exphh_kidnotvacc_hh <- hhdata1 %>% filter(allkidsvacc==0 & h10_hbv_rdt==1)

# households selected in fogarty: fogarty_2 per vhs.R
# remove those not in fogarty to see who has exposed but possibly not vacc kids and won't be evaled in fogarty
exphh_kidnotvacc_hh$hrhhid
exphh_kidnotvacc_hh_nofog <- exphh_kidnotvacc_hh %>% filter(!(hrhhid %in% fogarty_2$hrhhid))

exphh_kidnotvacc_hh_infog <- exphh_kidnotvacc_hh %>% filter((hrhhid %in% fogarty_2$hrhhid)) %>%  select("hrhhid")

exphh_kidnotvacc <- inddata1 %>% filter((hrhhid %in% exphh_kidnotvacc_hh_nofog$hrhhid)) %>% select("hrhhid","participant_code","hdov","h10_hbv_rdt","n","hr3_relationship_f","age_combined", "hr4_sex_f", "i27a_rdt_result_f","cpshbvprox")

table(exphh_kidnotvacc$cpshbvprox, exphh_kidnotvacc$h10_hbv_rdt)
nrow(exphh_kidnotvacc$hrhhid)

# Nov 1 calcs
addmargins(table(inddata1$cpshbvprox, inddata1$hhmemcat_f, inddata1$h10_hbv_rdt))
addmargins(table(inddata1$h10_hbv_rdt, inddata1$hhmemcat_f))
addmargins(table(inddata1$h10_hbv_rdt, inddata1$hhmemcat_f, inddata1$i27a_rdt_result_f))

addmargins(table(directoff$hr4_sex_f, directoff$h10_hbv_rdt))

addmargins(table(directoff$totalpositive, directoff$h10_hbv_rdt))

counts <- inddata1 %>% group_by(h10_hbv_rdt, hr3_relationship_f,i27a_rdt_result_f ) %>% count(directoff)

positives <- inddata1 %>% filter(i27a_rdt_result==1)
length(unique(positives$hrhhid))

table(positives$hhmemcat_f)

diroffhh <- left_join(diroffhh, hhdata1[, c("hrhhid","h10_hbv_rdt_f")],  by = "hrhhid")
addmargins(table(diroffhh$h10_hbv_rdt_f, diroffhh$allkidsvacc))

hhid_exp_kidnotvac <- diroffhh %>% filter(h10_hbv_rdt_f=="HBV+" & allkidsvacc==0)

ind_hhid_exp_kidnotvac <- inddata1 %>% filter((hrhhid %in% hhid_exp_kidnotvac$hrhhid))
table(ind_hhid_exp_kidnotvac$hhmemcat_f)
addmargins(table(ind_hhid_exp_kidnotvac$i27a_rdt_result_f,ind_hhid_exp_kidnotvac$hr3_relationship_f ))

ind_hhid_exp_kidnotvac %>% filter(hr3_relationship==3 &i27a_rdt_result==1) %>% count(hrhhid)

table(ind_hhid_exp_kidnotvac$totalpositive)#, ind_hhid_exp_kidnotvac$hrhhid)

## Nov 3 handwritten fam trees--------------
# 16 hh with another member pos (27 total)
# 10 hh with a dir off pos (8 are exposed, 2 unexposed but mother tested pos)
# 6 hh with another hh member pos (3 exposed, 3 unexposed). in these 3 exposed, one had children born before vacc, other two liekly vacc

# that covers 8+3=11 households with an infected mother. what about the other 89?
# ___ have direct off, ___ do not.
# using hhmempos created in 01_datacleaning.R
table(inddata1$hhmempos)
# give hh value if there is a hh member positive
hhtest <- inddata1 %>% group_by(hrhhid) %>% mutate(hh_hasposmemb = max(hhmempos)) %>% filter(hh_hasposmemb==1)
# select only those hh w a positive member
hhwposmembers <- hhdata1 %>% filter((hrhhid %in% hhtest$hrhhid))
# select only those hh WITHOUT a positive member
hhnoposothermembers <- hhdata1 %>% filter(!(hrhhid %in% hhtest$hrhhid))

table(hhnoposothermembers$h10_hbv_rdt)
#restrict to those exposed (index mom pos) so that we can better understand these hh
exphh_89_noinf <- hhnoposothermembers %>% filter(h10_hbv_rdt==1)
# how many have direct offspring, and within this, how many have all children born since HBV was fully rolled out (roughly 11/12 years)
addmargins(table(exphh_89_noinf$hasdiroff, exphh_89_noinf$allkidsvacc, useNA = "always"))

ind_89_expnoinf <- diroff %>% filter((hrhhid %in% exphh_89_noinf$hrhhid))

table(ind_89_expnoinf$cpshbvprox)
#given hh a value--this doesn't seem to have worked
ind_89_expnoinf <- ind_89_expnoinf %>% group_by(hrhhid) %>% mutate(cpshbvprox_hh = max(cpshbvprox))

expnotvacc <- exphh_89_noinf %>% filter(allkidsvacc==0)

expnotvacc_ind <- diroff %>% filter((hrhhid %in% expnotvacc$hrhhid))
expnotvacc_ind %>% group_by(hrhhid) %>% summarise(min(cpshbvprox))

expnotvacc_ind <- expnotvacc_ind %>% group_by(hrhhid) %>%  mutate(somenotvacc = min(cpshbvprox))
table(expnotvacc_ind$somenotvacc, useNA = "always")
expnotvacc_ind %>% group_by(somenotvacc) %>% count()
# get 9 exposed households with no one else pos, with dir off not vacc
expnotvacc_ind_0 <- expnotvacc_ind %>% filter(somenotvacc==0)

expnotvacc_hh <- exphh_89_noinf %>% filter((hrhhid %in% expnotvacc_ind_0$hrhhid)) 
# get 9 exposed hh with no one else pos, with dir off possibly not vacc
expnotvacc_ind_1 <- expnotvacc_ind %>% filter(somenotvacc==1)
expnotvacc_hh_1 <- exphh_89_noinf %>% filter((hrhhid %in% expnotvacc_ind_1$hrhhid)) 



# select the 9 households with a child unvaccinated but exposed--actually 10 including the huosehold with husband infected (1011)
expnotvacc_ind %>% group_by(hrhhid) %>% filter(min(cpshbvprox)==0) %>% summarise(hrhhid)

# Nov 10 for hover table 1--------------
addmargins(table(hhdata1$allkidsvacc, hhdata1$h10_hbv_rdt_f, useNA = "always"))


#expunvacc <- 
hhdata1 %>% filter(allkidsvacc==0 & h10_hbv_rdt==1) %>% summarise(hrhhid)
addmargins(table(hhdata1$oldestnotvacc, hhdata1$allkidsvacc))
addmargins(table(hhdata1$oldestnotvacc, hhdata1$allkidsvacc,hhdata1$h10_hbv_rdt_f ))














