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

ind1006$participant_code <- ifelse(ind1006$pid=="HRK2043-02" & ind1006$hrname_first=="Precieuse","03",ind1006$participant_code)
ind1006$pid <- paste0(ind1006$hrhhid,"-",ind1006$participant_code)
ind1006 %>% filter(hrhhid=="HRK2043") %>% select("pid")

# remove names before saving
ind1006 <- ind1006 %>% dplyr::select(!c("hrname_first","hrname_last","hrname_post"))

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

ind_clean %>% filter(hrhhid == "HRK-2089") %>% summarise(pid,i27a_rdt_result_f,age_combined,	hr4_sex_f,hrname_first, hrname_post, hrname_last,	hr9_school_gr_f,	hr11_religion_f,	hr10_occupation_gr_f)
ind_clean %>% filter(hrhhid == "HRK-2085") %>% summarise(pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f,	hr9_school_gr_f,	hr11_religion_f,	hr10_occupation_gr_f)
ind_clean %>% filter(hrhhid == "HRK-2085") %>% summarise(pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f,hrname_first, hrname_post, hrname_last)

ind_clean %>% filter(hrhhid == "HRK2083") %>% summarise(pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f,hrname_first, hrname_post, hrname_last)
ind_clean %>% filter(hrhhid == "HRK2083") %>% summarise(pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f,	hr9_school_gr_f,	hr11_religion_f,	hr10_occupation_gr_f)
ind_clean %>% filter(hrhhid == "HRB-1067") %>% summarise(pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f,	hr9_school_gr_f,	hr11_religion_f,	hr10_occupation_gr_f)



ind_clean %>% filter(hrhhid == "HRB -1010") %>% summarise(pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f, hrname_last,hrname_post, hrname_first)

ind_clean %>% filter(hrhhid == "HRK2015") %>% summarise(paststudymutexcl,pid,i27a_rdt_result_f,hr3_relationship_f,age_combined,	hr4_sex_f,hrname_last,hrname_post, hrname_first)
library(tidyverse)



# check for Sarah on Mar 21
inddata1 %>% filter(hrhhid == "HRK2081") %>% reframe(hdov,hr3_relationship_f,age_combined, hr4_sex_f, i27a_rdt_result_f, i3_hiv_pos_test,hivhaart,hrname_last, hrname_post, hrname_first)

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

# look to simplify fig 3 variables-------------------

# transfusions
table(inddata1$i8a_transfusion_number, inddata1$i8_transfusion, useNA = "always")
table(inddata1$transfus_num, inddata1$i8_transfusion_f, useNA = "always")
addmargins(table(inddata1$transfus_num, inddata1$i27a_rdt_result_f, useNA = "always"))

transfcheck <- glm(i27a_rdt_result ~ trans_bin, family = binomial(link = "logit"), data=inddata1)
summary(transfcheck)

addmargins(table(moms$transfus_num2, moms$i27a_rdt_result_f, useNA = "always"))
trmom <- glm(i27a_rdt_result ~ trans_bin, family = binomial(link = "logit"), data=moms)
summary(trmom)

addmargins(table(directoff$trans_bin, directoff$i27a_rdt_result_f, useNA = "always"))
addmargins(table(directoff$transfus_num2, directoff$i27a_rdt_result_f, useNA = "always"))
trdo <- glm(i27a_rdt_result ~ trans_bin + h10_hbv_rdt+age_combined, family = binomial(link = "logit"), data=directoff)
summary(trdo)

addmargins(table(othermember$transfus_num2, othermember$i27a_rdt_result_f, useNA = "always"))
troth <- glm(i27a_rdt_result ~ trans_bin, family = binomial(link = "logit"), data=othermember)
summary(troth)

# sexual history
# any partners
addmargins(table(moms$i24_sex_hx_part_past1yr, useNA = "always"))
addmargins(table(moms$part3mo_cat,moms$part12mo_cat, useNA = "always"))
addmargins(table(moms$part3mo_cat,moms$i27a_rdt_result_f, useNA = "always"))
addmargins(table(moms$part12mo_cat,moms$i27a_rdt_result_f, useNA = "always"))

addmargins(table(moms$partner3mo_bin,useNA = "always"))

sx3momom <- glm(h10_hbv_rdt ~ transactionalsex, family = binomial(link = "logit"), data=moms)
summary(sx3momom)
exp(sx3momom$coefficients)
exp(confint(sx3momom))

sx12mooth <- glm(i27a_rdt_result ~ transactionalsex, family = binomial(link = "logit"), data=othermember)
summary(sx12mooth)
exp(sx12mooth$coefficients)
exp(confint(sx12mooth))

# new sexual partners
addmargins(table(moms$i24_sex_hx_part_past1yr, useNA = "always"))
addmargins(table(moms$part3mo_cat,moms$part12mo_cat, useNA = "always"))
addmargins(table(moms$part3mo_cat,moms$i27a_rdt_result_f, useNA = "always"))
addmargins(table(moms$part12mo_cat,moms$i27a_rdt_result_f, useNA = "always"))

# given or received moeny for sex
addmargins(table(moms$transactionalsex, moms$h10_hbv_rdt_f, useNA = "always"))

addmargins(table(moms$i25_sex_hx_receive_money, moms$h10_hbv_rdt_f,useNA = "always"))
addmargins(table(moms$i26_sex_hx_given_money_f, moms$h10_hbv_rdt_f,useNA = "always"))
addmargins(table(moms$i26_sex_hx_given_money_f, moms$i25_sex_hx_receive_money,useNA = "always"))

# shared objects in household

addmargins(table(inddata1$i14_shared_razor_f, inddata1$i27a_rdt_result_f, useNA = "always"))
addmargins(table(inddata1$i15_shared_nailclippers_f, inddata1$i27a_rdt_result_f, useNA = "always"))
addmargins(table(inddata1$i13_shared_toothbrush_f, inddata1$i27a_rdt_result_f, useNA = "always"))

addmargins(table(inddata1$sharedhhobj, inddata1$i27a_rdt_result_f, useNA = "always"))
# just among moms
addmargins(table(moms$sharedhhobj, moms$i27a_rdt_result_f, useNA = "always"))
addmargins(table(moms$i14_shared_razor_f, moms$i27a_rdt_result_f, useNA = "always"))
addmargins(table(moms$i13_shared_toothbrush_f, moms$i27a_rdt_result_f, useNA = "always"))

# wealth
wealthlow <- glm(i27a_rdt_result_f ~ wealth_R_lowestv, family = binomial(link = "logit"), data=othermember)
summary(wealthlow)
exp(wealthlow$coefficients)
exp(confint(wealthlow))

wealthhigh <- glm(i27a_rdt_result_f ~ wealth_R_highestv, family = binomial(link = "logit"), data=othermember)
summary(wealthhigh)
exp(wealthhigh$coefficients)
exp(confint(wealthhigh))

wealth <- glm(i27a_rdt_result_f ~ wealth_R, family = binomial(link = "logit"), data=othermember)
summary(wealth)
exp(wealth$coefficients)
exp(confint(wealth))
addmargins(table(othermember$wealth_R, othermember$i27a_rdt_result_f,useNA = "always"))
addmargins(table(othermember$wealth_R_lowestv, othermember$i27a_rdt_result_f,useNA = "always"))

addmargins(table(moms$i12_food_first_chew_f, moms$i27a_rdt_result_f, useNA = "always"))

addmargins(table(directoff$i16_traditional_scarring_f, directoff$i27a_rdt_result_f, useNA = "always"))
addmargins(table(othermember$i16_traditional_scarring_f, othermember$i27a_rdt_result_f, useNA = "always"))

# updated model
table(directoff$h10_hbv_rdt)
alldovar <- c("age_combined","hr4_sex_f","cpshbvprox_rev", "wealth_R_lowestv","sharedhhobj",'i12_food_first_chew_f','trans_bin', "i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin","newpartner3mo_indic")

directoffexp <-directoff %>% filter(h10_hbv_rdt == 1)
directoffunexp <-directoff %>% filter(h10_hbv_rdt == 0)
library(lme4) 
library(tidyr)

  m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + sharedhhobj, data=directoffexp, family=binomial("logit"))
  summary(m)
exp( -0.8154 )
  

m_0 <- glmer(i27a_rdt_result ~  (1 | hrhhid) + sharedhhobj, data=directoffunexp, family=binomial("logit"))
summary(m_0)
exp( 0.4702)

# Sept 1 cleaning other code files------
hhdata1 %>% #group_by(h10_hbv_rdt) %>% 
  ggplot()+
  geom_histogram(aes(x=n, fill=fct_rev(as.factor(totalpositive))))+ #hhprev
  scale_fill_brewer(palette = "RdPu", direction = -1) +
  facet_wrap(~ as.factor(h10_hbv_rdt))+
  theme(panel.background = element_blank())


inddata1 %>% filter(age_combined < 17) %>% 
  ggplot()+geom_histogram(aes(x=age_combined, fill=age_cat))


# sexual partners

inddata1 %>% filter(!is.na(i23_sex_hx_part_past3mo)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i23_sex_hx_part_past3mo),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("Total sexual partners, last 3 months")+
  theme(panel.background = element_blank())

inddata1 %>% filter(!is.na(i23a_sex_hx_past3mo_num)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i23a_sex_hx_past3mo_num),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("Total sexual partners, last 3 months")+
  theme(panel.background = element_blank())

inddata1 %>% filter(!is.na(i24_sex_hx_part_past1yr)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i24_sex_hx_part_past1yr),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("Total sexual partners, last 12 months")+
  theme(panel.background = element_blank())

inddata1 %>% filter(!is.na(i24a_sex_hx_past1yr_num)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i24a_sex_hx_past1yr_num),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("New sexual partners, last 12 months")+
  theme(panel.background = element_blank())

# focus on moms-----
moms %>%
  dplyr::group_by(i23_sex_hx_part_past3mo) %>%
  dplyr::summarise(count_pos = sum(i27a_rdt_result, na.rm=TRUE), n=n(), prop_pos = 100*(count_pos/n)) 

# % of new partners who are new
moms %>%
  dplyr::group_by(i23_sex_hx_part_past3mo) %>%
  dplyr::summarise(count_pos = sum(i27a_rdt_result, na.rm=TRUE), n=n(), prop_pos = 100*(count_pos/n)) 

newsex12mo_recode
newsex3mo_recode

table(moms$partner3mo_bin, moms$serochangedir)
moms = moms %>%
  mutate(partn_3mo_dkref = case_when(
    i23_sex_hx_part_past3mo > 1 & i23_sex_hx_part_past3mo < 98 ~ 1, #more than 1 sexual partner or don't know
    i23_sex_hx_part_past3mo >= 98  ~ 2, # refused = own category
    i23_sex_hx_part_past3mo == 1 ~ 0, # make one or 0 sexual partners the reference group - no separate
    i23_sex_hx_part_past3mo == 0 ~ 0, # make one or 0 sexual partners the reference group - no separate
    TRUE ~ NA_real_
  ) %>% as.factor())
table(moms$partn_3mo_dkref, moms$h10_hbv_rdt_f,useNA = "always")
addmargins(table(moms$partner3mo_bin, moms$h10_hbv_rdt_f,useNA = "always"))

# sexual partners in last 3 mo
addmargins(table(moms$partner3mo_bin, moms$h10_hbv_rdt_f,useNA = "always"))
# new sexual partners in last 3mo
addmargins(table(moms$newpartner3mo_indic, moms$h10_hbv_rdt_f,useNA = "always"))
addmargins(table(moms$newpartner3mo_indic, moms$h10_hbv_rdt_f,useNA = "always"))

whymiss <- moms %>% filter(is.na(newpartner3mo_indic))
whymiss %>% group_by(h10_hbv_rdt_f) %>% reframe(maternity,serochangedir,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num) %>% print(n=Inf)

inddata1 %>% filter(is.na(newpartner3mo_indic) & age_combined >=15) %>% 
 group_by(h10_hbv_rdt_f) %>% reframe(maternity,serochangedir,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num) %>% print(n=Inf)


moms %>% filter(is.na(i24a_sex_hx_past1yr_num) & age_combined >=15) %>% 
  group_by(h10_hbv_rdt_f) %>% reframe(maternity,serochangedir,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num) %>% print(n=Inf)



moms %>%  
  ggplot()+geom_bar(aes(x=as.factor(i23_sex_hx_part_past3mo),fill=as.factor(newsex3mo_recode )))+ #i27a_rdt_result_f
  facet_wrap(~fct_rev(h10_hbv_rdt_f))+
  ggtitle("Total sexual partners in last 3 months, colored by how many are new")+
  theme(panel.background = element_blank())+
  xlab("Number of sexual partners")+
  ylab("Count (index mothers)")

moms %>%  
  ggplot()+geom_bar(aes(x=as.factor(i24_sex_hx_part_past1yr),fill=as.factor(newsex12mo_recode )))+ #i27a_rdt_result_f
  facet_wrap(~fct_rev(h10_hbv_rdt_f))+
  ggtitle("Total sexual partners in last 12 months, colored by how many are new")+
  theme(panel.background = element_blank())+
  xlab("Number of sexual partners")+
  ylab("Count (index mothers)")


# four variables
moms %>%  
  ggplot()+geom_bar(aes(x=as.factor(i23_sex_hx_part_past3mo),fill=as.factor(h10_hbv_rdt_f )))+ #i27a_rdt_result_f
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(h10_hbv_rdt_f))+
  ggtitle("Total sexual partners, last 3 months")+
  theme(panel.background = element_blank())

moms %>% filter(!is.na(i23a_sex_hx_past3mo_num)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i23a_sex_hx_past3mo_num),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("Total sexual partners, last 3 months")+
  theme(panel.background = element_blank())

moms %>% filter(!is.na(i24_sex_hx_part_past1yr)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i24_sex_hx_part_past1yr),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("Total sexual partners, last 12 months")+
  theme(panel.background = element_blank())
table(moms$i24_sex_hx_part_past1yr, moms$serochangedir, useNA = "always")

moms %>% filter(!is.na(i24a_sex_hx_past1yr_num)) %>% 
  ggplot()+geom_bar(aes(x=as.factor(i24a_sex_hx_past1yr_num),fill=i27a_rdt_result_f ))+
  scale_fill_manual(values = nounprojgraphcol)+ 
  facet_wrap(~fct_rev(hhmemcat_4_f))+
  ggtitle("New sexual partners, last 12 months")+
  theme(panel.background = element_blank())
table(moms$i24a_sex_hx_past1yr_num, moms$serochangedir, useNA = "always")


# Table 1 checks Oct 2023---------------------------------------------------------------------------
addmargins(table(inddata1$hr4_sex_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$hr3relat_simp_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$maritalrisk_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$educ_simp_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$hr10_occupation_gr_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$hr5_primary_residence_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$hr6_last_night_residence_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$i1_hbv_positive_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$i3_hiv_pos_test_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$hivhaart, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))
addmargins(table(inddata1$i1_hbv_positive_f, inddata1$h10_hbv_rdt_f,  inddata1$hhmemcat_f))

# fig 2c
inddata1 %>% filter(hbvposdiroff > 0) %>% group_by(h10_hbv_rdt_f) %>% reframe(hrhhid, hhmemcat_f, i27a_rdt_result_f) %>% print(n=Inf)
inddata1 %>% filter(hhmempos > 0) %>% group_by(h10_hbv_rdt_f) %>% reframe(hrhhid, hhmemcat_f, i27a_rdt_result_f) %>% print(n=Inf)
# 
inddata1 %>% filter(serochangedir=="cleared") %>% reframe(hrhhid, hhmemcat_f, i27a_rdt_result_f)

table(inddata1$serochangedir)
