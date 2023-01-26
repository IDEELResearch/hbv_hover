# Run 01_datacleaning.R first

# packages for this program
library(tableone)
library(tidyverse)
#Table 1--------------------
##Household--------------------------------------------------------------------------------
# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

hhdata2 <- merge(hhdata2, hhIDs, by = c("hrhhid"), all.x = T)

# summary(hhdata2)

#organize variables for table 1
# all vars
tab1varcleaned <- c("n","maternity","modernfloor", "modernroof","modernwindow","modernwalls","modernhousing", "h5_cultivable_land_f",  "privatewater", "h3_hh_wealth_electr","wealth_R", "h8_nail_cutting_f", "h8a_nail_clippers_owned", "h8b_nail_filer_owned","h9_razor_f", "h8a_razer_owned")
# num vars
numericvars <- c("n") # age, under 5s in hh, total hh members
# cat vars
catvars <- c("maternity" ,"modernfloor", "modernroof","modernwindow","modernwalls","modernhousing", "h5_cultivable_land_f", "privatewater","h3_hh_wealth_electr","wealth_R", "h8_nail_cutting_f", "h8a_nail_clippers_owned", "h8b_nail_filer_owned", "h9_razor_f", "h8a_razer_owned")

#first step in create table 1
hover_hh_tab1 <- CreateTableOne(vars = tab1varcleaned, factorVars = catvars, data=hhdata2, strata = "h10_hbv_rdt", addOverall = T)

tab1hhexport <- print(hover_hh_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1hhexport
write.csv(tab1hhexport, file = "tab1hhexport.csv")
# TODO

# -add factor for exposed vs unexposed
# -add all hh back (check on wealth index)


##All individuals------------------------------
# use inddata1_dc for main hover paper
inddata1 <- inddata1_dc
# all vars
tab1_ind <- c("i27a_rdt_result_f","hhmemcat_f","hr3_relationship_f","age_combined","agegrp15_2", "hr4_sex_f", "maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","i4_fever_f", 
              "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
              "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f")

# num vars
numvars_ind <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_ind <- c("i27a_rdt_result_f","hhmemcat_f","hr3_relationship_f","agegrp15_2" ,"hr4_sex_f", "maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","i4_fever_f",
                 "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f", "i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                  "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f", "i26_sex_hx_given_money_f")
#first step in create table 1
hover_ind_tab1 <- CreateTableOne(vars = tab1_ind, factorVars = catvars_ind, data=inddata1, strata = c("h10_hbv_rdt_f"), addOverall = T)

tab1ind_exp <- print(hover_ind_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1ind_exp
write.csv(tab1ind_exp, file = "tab1ind_exp.csv")

##Index mothers---------
# Age
# by exposure
hhdata1 %>% group_by(h10_hbv_rdt_f) %>% summarise(mean(indexmotherage), sd(indexmotherage))
# overall
hhdata1 %>% summarise(mean(indexmotherage), sd(indexmotherage))

# moms dataset if needed
## moms <- inddata1 %>% filter(hr3_relationship == 1) 

# mom all vars
tab1_mom <- c("indexmotherage", "maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","i4_fever_f", 
              "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
              "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f")

# num vars
numvars_mom <- c("indexmotherage") # age, under 5s in hh, total hh members
# cat vars
catvars_mom <- c("maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","i4_fever_f",
                 "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f", "i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                 "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f", "i26_sex_hx_given_money_f")
hover_mom_tab1 <- CreateTableOne(vars = tab1_mom, factorVars = catvars_mom, data=moms, strata = c("h10_hbv_rdt_f"), addOverall = T)

tab1mom_exp <- print(hover_mom_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1mom_exp
write.csv(tab1mom_exp, file = "tab1mom_exp.csv")

moms %>% group_by(h10_hbv_rdt_f) %>% filter(i3_hiv_pos_test==1) %>%  summarise(i3b_hiv_medications)

# time at residence
# those with 0 time or NA
moms %>% group_by(h10_hbv_rdt_f) %>% filter(is.na(i6_comb_yr) | i6_comb_yr==0) %>%  count()
# median, IQR time at residence if not missing/0
moms %>% group_by(h10_hbv_rdt_f) %>% filter(!is.na(i6_comb_yr) & i6_comb_yr!=0) %>%  summarise(median(i6_comb_yr), quantile(i6_comb_yr, probs=c(0.25, 0.75)))
# overall
moms %>% group_by(hr3_relationship) %>% filter(!is.na(i6_comb_yr) & i6_comb_yr!=0) %>%  summarise(median(i6_comb_yr), quantile(i6_comb_yr, probs=c(0.25, 0.75)))



# median age of moms
moms %>% group_by(h10_hbv_rdt) %>% summarise(quantile(age_combined, probs=c(0.25, 0.5,0.75)))
quantile(moms$age_combined, c(0.25, 0.75))

# self-reported HIV infection
selfrephiv <- moms %>% filter(i3_hiv_pos_test == 1)
table(selfrephiv$i27a_rdt_result_f, selfrephiv$acq_ind, useNA = "always")
table(selfrephiv$i3a_hiv_treatment_f, selfrephiv$acq_ind, useNA = "always")

addmargins(table(moms$acq_ind, moms$i3_hiv_pos_test_f, moms$i27a_rdt_result_f,useNA = "always"))
addmargins(table(moms$acq_ind, moms$i3a_hiv_treatment_f, useNA = "always"))
addmargins(table(moms$acq_ind, moms$i3b_hiv_medications, useNA = "always"))
##Direct offspring---------
# dataset is directoff, created in 04_famtree.R
# to avoid rerunning those contents again:
directoff <- inddata1 %>% filter(hr3_relationship == 3)

#  all vars for DO
tab1_diroff <- c("age_combined", "hr4_sex_f", "cpshbvprox","maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","i4_fever_f", 
              "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
              "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f")
# num vars
numvars_diroff <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_diroff <- c("cpshbvprox","hr4_sex_f","maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","i4_fever_f",
                 "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f", "i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                 "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f", "i26_sex_hx_given_money_f")
hover_diroff_tab1 <- CreateTableOne(vars = tab1_diroff, factorVars = catvars_diroff, data=directoff, strata = c("h10_hbv_rdt_f"), addOverall = T)

tab1diroff_exp <- print(hover_diroff_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1diroff_exp
write.csv(tab1diroff_exp, file = "tab1diroff_exp.csv")

# who are the offspring HIV+
table(directoff$i3_hiv_pos_test)

directoff %>% filter(i3_hiv_pos_test==1) %>% summarise(pid, age_combined)

#median age of direct off
directoff %>% group_by(h10_hbv_rdt) %>% summarise(quantile(age_combined, probs=c(0.25, 0.5,0.75)))
quantile(directoff$age_combined, c(0.25, 0.5,0.75))

# range of offspring in hh
directoffsum <- directoff %>% group_by(hrhhid) %>% summarise(ndiroff=n())
directoffsum <- left_join(directoffsum, inddata1[,c("hrhhid","h10_hbv_rdt_f")], by = "hrhhid")

directoffsum %>% group_by(h10_hbv_rdt_f) %>% summarise(quantile(ndiroff, probs=c(0,0.25, 0.5,0.75,1)))
median(directoffsum$ndiroff)


## other hh members-------
othermember <- inddata1 %>% filter(hhmemcat==0)

#  all vars
tab1_other <- c("age_combined", "hr4_sex_f","hr3_relationship_f" ,"cpshbvprox","maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","i4_fever_f", 
                 "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                 "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f")
# num vars
numvars_other <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_other <- c("cpshbvprox","hr4_sex_f","hr3_relationship_f" ,"maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","i4_fever_f",
                    "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f", "i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                    "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f", "i26_sex_hx_given_money_f")
hover_other_tab1 <- CreateTableOne(vars = tab1_other, factorVars = catvars_other, data=othermember, strata = c("h10_hbv_rdt_f"), addOverall = T)

tab1oth_exp <- print(hover_other_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1oth_exp
write.csv(tab1oth_exp, file = "tab1oth_exp.csv")

# median age other hh mem
othermember %>% group_by(h10_hbv_rdt) %>% summarise(quantile(age_combined, probs=c(0.25, 0.5,0.75)))
quantile(othermember$age_combined, c(0.25, 0.5,0.75))

# who are the offspring HIV+
table(othermember$i3b_hiv_medications)

othermember %>% filter(i3_hiv_pos_test==1) %>% summarise(pid, age_combined, hr3_relationship_f, hivhaart)

#median age overall
quantile(inddata1$age_combined, c(0.25, 0.5,0.75))

# table 1 by exposure and 3-cat hh memb type---------
#  all vars
tab1_all <- c("age_combined", "hr4_sex_f","hr3_relationship_f" ,"cpshbvprox","maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f",
                "hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","hivhaart","i4_fever_f", 
                "i5_pregnancy_f")
# num vars
numvars_all <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_all <- c("cpshbvprox","hr4_sex_f","hr3_relationship_f" ,"maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f",
                   "hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","hivhaart","i4_fever_f",
                   "i5_pregnancy_f")

hover_all_tab1 <- CreateTableOne(vars = tab1_all, factorVars = catvars_all, data=inddata1, strata = c("h10_hbv_rdt_f","hhmemcat_f"), addOverall = T)

tab1_exp_memb <- print(hover_all_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1_exp_memb
write.csv(tab1_exp_memb, file = "tab1_exp_memb.csv")

# removed HBV risk factors  - for subsequent analysis
#"i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
# "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f"

# analysis years living at household
inddata1 %>% group_by(hhmemcat_f,h10_hbv_rdt_f) %>% filter(is.na(i6_comb_yr) | i6_comb_yr==0) %>%  count()

# median, IQR time at residence if not missing/0
inddata1 %>% group_by(hhmemcat_f,h10_hbv_rdt_f) %>% filter(!is.na(i6_comb_yr) & i6_comb_yr!=0) %>%  summarise(median(i6_comb_yr), quantile(i6_comb_yr, probs=c(0.25, 0.75)))
# overall
inddata1  %>% filter(!is.na(i6_comb_yr) & i6_comb_yr!=0) %>%  summarise(median(i6_comb_yr), quantile(i6_comb_yr, probs=c(0.25, 0.75)))



## misc for IRB renewal---------
hhsincerenew <- (hhdata1 %>% filter(hdov > '2021-10-11'))
#new members since last approval
nrow(inddata1 %>% filter(hrhhid %in% hhsincerenew$hrhhid))
#children
nrow(inddata1 %>% filter(age_combined <18))
#pregnant
table(inddata1$i5_pregnancy_f)

# misc for CROI abstract------------------------------------------------------------------------
nrow(inddata1 %>% filter(age_combined < 18)) # how many under 18
nrow(inddata1 %>% filter(age_combined < 18))/nrow(inddata1) # %  under 18
addmargins(table(inddata1$hr3_relationship_f)) # how many direct offspring
addmargins(table(inddata1$hr3_relationship_f))/nrow(inddata1) # %  direct offspring
addmargins(table(inddata1$hhmemcat_f)) # how many direct offspring
addmargins(table(inddata1$indexmom)) # how many direct offspring

table(inddata1$i27a_rdt_result_f, inddata1$indexmom)

#how many serostatus changes
inddata1 %>% filter(indexmom=="Mère index") %>% group_by(h10_hbv_rdt_f ,i27a_rdt_result_f) %>% count()
6/195

# how many new positives (non-index mothers)
inddata1 %>% filter(indexmom=="Membre de ménage") %>% group_by(h10_hbv_rdt_f ,i27a_rdt_result_f) %>% count()
19/(359+19+2)
8/(413+8+2)

#clusters of infections
addmargins(table(hhdata1$totalpositive, hhdata1$h10_hbv_rdt_f))
# why are there 5 eexposed households with no cases
noexpcases <- hhdata1 %>% filter(totalpositive==0 & h10_hbv_rdt==1) %>% select(hrhhid)
hhmemnoexpcases <- inddata1 %>% filter(hrhhid %in% noexpcases$hrhhid)

table(hhmemnoexpcases$i27a_rdt_result_f, hhmemnoexpcases$hrhhid,useNA = "always")

nrow(inddata1 %>% group_by(hrhhid) %>% count())

mean(hhdata2$hhsize)
# hiv among the sag+
table(inddata1$i3_hiv_pos_test_f, inddata1$i27a_rdt_result_f)
table(inddata1$i27a_rdt_result_f, inddata1$i3a_hiv_treatment_f, inddata1$i3_hiv_pos_test_f)

#prior HBV test
table(inddata1$i1_hbv_positive_f, inddata1$i27a_rdt_result_f)

## Table 1 without index mothers------------
hhmemb <- inddata1 %>% filter(hhmemcat<2) #hhmemcat 0=other, 1=diroff, 2=indexmom

# all vars
tab1_ind <- c("i27a_rdt_result_f","indexmom","hr3_relationship_f","age_combined","agegrp15_2", "hr4_sex_f", "maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","i4_fever_f", 
              "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
              "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f")

# num vars
numvars_ind <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_ind <- c("i27a_rdt_result_f","indexmom","hr3_relationship_f","agegrp15_2" ,"hr4_sex_f", "maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","i4_fever_f",
                 "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f", "i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                 "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f", "i26_sex_hx_given_money_f")
#first step in create table 1
hhmemonlytab1 <- CreateTableOne(vars = tab1_ind, factorVars = catvars_ind, data=hhmemb, strata = c("i27a_rdt_result_f", "h10_hbv_rdt_f"), addOverall = T)

tab1hhmem <- print(hhmemonlytab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1hhmem
write.csv(tab1hhmem, file = "tab1hhmem.csv")


### Moran's i-------------
library(ape)
unique <- hhdata2[!duplicated(hhdata2[c('hxcoord_edit', 'hycoord_edit')]),]

test.hhdist <- as.matrix(dist(cbind(testmoran$hxcoord_edit, testmoran$hycoord_edit)))
test.hhdist.inv  <- 1/test.hhdist
diag(test.hhdist.inv) <- 0
# view table of distances
test.hhdist.inv[1:5, 1:5]
max(test.hhdist.inv) # check not infinite

table(testmoran$hhprev)
Moran.I(testmoran$hhprev, test.hhdist.inv)

# autocorr for risk factors
#non missing traditiona scars
table(inddata1$i16_traditional_scarring, useNA = "always")

nonmissscar <- inddata1 %>% filter(i16_traditional_scarring != 99)
nonmissscar1 <- nonmissscar %>%
  dplyr::group_by(hrhhid) %>%
  dplyr::summarise(totalscars = sum(i16_traditional_scarring, na.rm=TRUE), nscar=n()) 

table(nonmissscar1$totalscars)

nonmissscar1hh <- left_join(hhdata1, nonmissscar1, by = "hrhhid")
nonmissscar1hh$scarprev <- ifelse(nonmissscar1hh$totalscars==0,0,nonmissscar1hh$totalscars/nonmissscar1hh$nscar)
#check
table(nonmissscar1hh$scarprev)

uniquescar <- nonmissscar1hh[!duplicated(nonmissscar1hh[c('hxcoord_edit', 'hycoord_edit')]),]

test.hhdist <- as.matrix(dist(cbind(uniquescar$hxcoord_edit, uniquescar$hycoord_edit)))
test.hhdist.inv  <- 1/test.hhdist
diag(test.hhdist.inv) <- 0
# view table of distances
test.hhdist.inv[1:5, 1:5]
max(test.hhdist.inv) # check not infinite

Moran.I(uniquescar$scarprev, test.hhdist.inv)

#autocorre of infected hh members
table(hhmemb$i27a_rdt_result, hhmemb$h10_hbv_rdt)
othermember <- hhmemb %>% filter(hhmemcat==0)
table(othermember$i27a_rdt_result, othermember$hrhhid)
othermember$cpshbvprox_rev <- 2 - othermember$cpshbvprox
othermember$cpshbvprox_rev <- as.factor(othermember$cpshbvprox_rev)

othmemprev <- othermember %>%
  dplyr::group_by(hrhhid) %>%
  dplyr::summarise(infhh = sum(i27a_rdt_result, na.rm=TRUE)) 

addmargins(table(othmemprev$infhh))

othmemprev2 <- left_join(hhdata1, othmemprev, by = "hrhhid")
othmemprev2$othermemprev <- ifelse(is.na(othmemprev2$infhh),0,othmemprev2$infhh/othmemprev2$n)
#check
table(othmemprev2$othermemprev, useNA = "always")

uniqueothprev <- othmemprev2[!duplicated(othmemprev2[c('hxcoord_edit', 'hycoord_edit')]),]
table(uniqueothprev$othermemprev, useNA = "always")

test.hhdist <- as.matrix(dist(cbind(uniqueothprev$hxcoord_edit, uniqueothprev$hycoord_edit)))
test.hhdist.inv  <- 1/test.hhdist
diag(test.hhdist.inv) <- 0
# view table of distances
test.hhdist.inv[1:5, 1:5]
max(test.hhdist.inv) # check not infinite

Moran.I(uniqueothprev$othermemprev, test.hhdist.inv)

# Maps--------------------------------------------------------------------------------

library(sf)
library(gstat)
library(stars)
library(tidyverse)
library(patchwork)
library(ggsn)
library(ggspatial)

# merge gps onto individual survey
inddata1 <- merge(inddata1, hhdata1[,c("hrhhid","hycoord_edit","hxcoord_edit")], by = "hrhhid")
# make spatial object
indgps_2 = st_as_sf(inddata1[!is.na(inddata1$hxcoord_edit) &!is.na(inddata1$hycoord_edit),], coords = c("hycoord_edit", "hxcoord_edit"), crs = 4326)  
# order by hbsag result
indgps_2 <- indgps_2[order(indgps_2$i27a_rdt_result_f),]
# jitter points
indgps_2_jitt<- st_jitter(indgps_2,  factor = 0.005)

# surrounding polygons
drc_healthzone_correctkinshasa = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/drc_healthzone_adm2_correctkinshasa/RDC_Zone_de_sante_09092019.shp", stringsAsFactors = F) %>% st_transform(4326)
# keep only Kin prov
drc_healthzone_kinshasa <- subset(drc_healthzone_correctkinshasa, PROVINCE == "Kinshasa")

# brazzaville polygon
congo_br = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/congo_adm0/cog_admbnda_adm0_gaul_20190617.shp", stringsAsFactors = F) %>% st_transform(4326)
# rename brazza labels
congo_br$ADM0_FR <- as.character(congo_br$ADM0_FR) 
congo_br$ADM0_FR[congo_br$ADM0_FR == "Congo (le)"] <- "Congo"
congo_br$ADM0_FR[congo_br$ADM0_FR == "Congo"] <- "Brazzaville"

# health areas
drc_healtharea = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/rdc_aires-de-sante/RDC_Aires de sant‚.shp", stringsAsFactors = F) %>% st_transform(4326)
# restrict to Kin
drc_healtharea_Kin <- subset(drc_healtharea, Province == "Kinshasa")

#provinces
drcprov = st_read("./adm1/GLOBAL_ADM1.shp", stringsAsFactors = F) %>% st_transform(4326)
# restrict to Kin and prov below kin (but prov shp covers river)
drcprov_Kin <- subset(drcprov, ADM1_NAME == "KINSHASA")
drcprov_KinKC <- subset(drcprov, ADM1_NAME == "KINSHASA" | ADM1_NAME == "KONGO CENTRAL" )

##rivers - covered by adm0 and adm1 
# rivers = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/congo_rivers_simp/congo_rivers_simp.shp", stringsAsFactors = F) %>% st_transform(4326)

# read in data
admin0 <- readRDS('./admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 

st_crs(admin0) # view CRS

DRC <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC

## Africa with DRC highlighted and Kinshasa in red box
africa <- st_read("./afr_simp/afr_g2014_2013_0.shp", stringsAsFactors = F) %>% st_transform(4326)
A <- ggplot(africa) +
    geom_sf(alpha=0.75, size = 0.1)+
    geom_sf(data=DRC, fill = "gray75", size=0.2)+
    geom_rect(aes(xmin = 15.2, xmax = 15.6, ymin = -4.48, ymax = -4.07),
              fill = "transparent", color = "red", size = 1.5)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank())
  

## Base map----------
base <- ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf_text(data=congo_br, aes(label = ADM0_FR), size=3)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.15,0.9))

## Map: HH prev--------------------
base +
  geom_sf(data=hover_gps_full, aes(color=hhprev))+
  scale_color_binned(type="viridis", breaks = c(0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9))+
  #scale_color_continuous(type = "RdYlBu")+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  guides(fill=guide_legend(title="Household prevalence (%)"))

## Map: Exp vs Unexp------------------
hover_gps_full <- hover_gps_full %>% 
  dplyr::mutate(h10_hbv_rdt_f=factor(
    hover_gps_full$h10_hbv_rdt, 
    levels = c(0, 1),
    labels = c("Unexposed", "Exposed")))

#B <- 
  ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2,aes(label = ADM0_FR))+
  geom_sf(data=hover_gps_full, aes(fill=h10_hbv_rdt_f, color=h10_hbv_rdt_f), size=0.5)+ #or 3 for standalone
  geom_sf(data=matgps, color = "gray39", fill="cornsilk2", shape = 21, aes(label = centers))+
  #scale_fill_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  scale_color_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  geom_sf_text(data=congo_br, aes(label = ADM0_FR), size=1, hjust = 2, vjust = 44)+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  #coord_sf(xlim = c(14.1, 15.6), ylim = c(-7.1, -3.1), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.12,0.92)#,
        #legend.text=element_text(size=20)
        )+
    guides(color = guide_legend(override.aes = list(size = 2)))
    #annotation_scale(location = "br", plot_unit = "mi")
  
# ggsave('./plots/hh.png', width=9, height=9)

## Map: Hbsag results--------------------
C <- 
  base + 
  geom_sf(data=indgps_2_jitt[!is.na(indgps_2_jitt$i27a_rdt_result_f),], aes(fill=i27a_rdt_result_f, color=i27a_rdt_result_f), size = 0.5)+
  scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.1,0.92))+
  guides(color = guide_legend(override.aes = list(size = 2)))

  

##Map: Traditional scarring---------------------------------------------------------------
indgps_2_jitt <- indgps_2_jitt %>% 
  dplyr::mutate(i16_traditional_scarring_f_map=factor(
    indgps_2_jitt$i16_traditional_scarring, 
    levels = c(0, 1, 99),
    labels = c("No traditional scars", "Has traditional scars", "Refused")))
D <-
  base + 
  geom_sf(data=indgps_2_jitt, aes(fill=i16_traditional_scarring_f_map, color=i16_traditional_scarring_f_map), size=0.5)+
  #scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  #scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#665191',"#F5B24E","#A87323"))+   # purple and yellow and brown (refuse)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.2,0.9))+
    guides(color = guide_legend(override.aes = list(size = 2)))

# past transfusions
indgps_2_jitt <- indgps_2_jitt %>% 
  dplyr::mutate(i8_transfusion_f_map=factor(
    indgps_2_jitt$i8_transfusion, 
    levels = c(0, 1, 99),
    labels = c("No transfusions", "Received transfusions", "Refused")))
E <-
  base + 
  geom_sf(data=indgps_2_jitt[!is.na(indgps_2_jitt$i8_transfusion_f_map),], aes(fill=i8_transfusion_f_map, color=i8_transfusion_f_map), size=0.5)+
  #scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  #scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#665191',"#F5B24E","#A87323"))+   # purple and yellow and brown (refuse)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.213,0.9))+
  guides(color = guide_legend(override.aes = list(size = 2)))

#money exchanged for sex
indgps_2_jitt <- indgps_2_jitt %>% 
  dplyr::mutate(i26_sex_hx_given_money_f_map=factor(
    indgps_2_jitt$i26_sex_hx_given_money, 
    levels = c(0, 1, 99),
    labels = c("Never given money for sex", "Given money for sex", "Refused")))
F <-
  base + 
  geom_sf(data=indgps_2_jitt[!is.na(indgps_2_jitt$i26_sex_hx_given_money_f_map),], aes(fill=i26_sex_hx_given_money_f_map, color=i26_sex_hx_given_money_f_map), size=0.5)+
  #scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  #scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#665191',"#F5B24E","#A87323"))+   # purple and yellow and brown (refuse)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.245,0.9))+
  guides(color = guide_legend(override.aes = list(size = 2)))

# piece plots together using library(patchwork)
A + B + C + D + E + F + plot_layout(nrow=2, ncol = 3) + plot_annotation(tag_levels = 'A')

#A + grid + plot_layout(nrow=1, ncol = 2) + plot_annotation(tag_levels = 'A')

# output
ggsave('./plots/croiabs.png', width=15, height=9)

## Kriging-------------
# kriging using gstat: https://rpubs.com/nabilabd/118172 
# https://mgimond.github.io/Spatial/interpolation-in-r.html#generate-the-variance-and-confidence-interval-maps


output <- indgps_2_jitt %>% 
  #group_by(hrhhid) %>%
  # make variable for hh prev
  mutate(hhprev = (totalpositive/n)*100)

output <- hover_gps_full
# remove points where geometry is outside of DRC outline (geometry=c(0,0))
output_points <- st_join(output, drcprov_Kin, join = st_intersects)
#%>% filter(!is.na(Country)) # from Hill's code

# make variogram
m.vgm <- gstat::variogram(hhprev~1, output_points)

# fit a model to the sample variogram
# https://gisgeography.com/semi-variogram-nugget-range-sill/
m.fit <- gstat::fit.variogram(m.vgm, model=vgm(psill=1,"Exp",range=500, nugget=0))

library(automap)
variogram = autofitVariogram(hhprev~1,output_points)
plot(variogram)

# plot
plot(m.vgm,m.fit)

# simple kriging
spDRC <- as_Spatial(DRC)
grd <- makegrid(spDRC, n = 50000)# making grid of points
colnames(grd) <- c('x','y')
grd_pts <- SpatialPoints(coords = grd, 
                         proj4string=CRS(proj4string(spDRC)))

# find all points in `grd_pts` that fall within DRC outline
grd_pts_in <- grd_pts[spDRC, ]

# transform grd_pts_in back into a data frame
gdf <- as.data.frame(coordinates(grd_pts_in)) 

# conduct kriging: Pf prev
m.kriged <- gstat::krige(prev~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged$var1.pred)

# assign points into bins
krige <- m.kriged %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,80,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))

# conduct kriging: animal ownership
m.kriged.own <- gstat::krige(ownership~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.own$var1.pred)

# assign points into bins
krige_own <- m.kriged.own %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,90,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))
# Stats for proposal----------------------------
table(inddata1$h10_hbv_rdt, inddata1$hhmemcat_f)

table(inddata1$h10_hbv_rdt, inddata1$indexmom)


# Figures-----------------------
## Relation to index mother-----
inddata1 <- inddata1 %>% 
  dplyr::mutate(hr3_relationship_f_eng=factor(
    inddata1$hr3_relationship, 
    levels = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,97,98,99),
    labels = c("No relation", "Index mother","Spouse", "Child", "Son/daughter-in-law", "Grandchild","Parent","Parent-in-law","Sibling","Niece/nephew","Niece/nephew by marriage",
               "Adopted child of spouse","Aunt/uncle","Grandparent","Other","Don't know", "Refused")))    

inddata1 <- inddata1 %>% 
  dplyr::mutate(h10_hbv_rdt_f_2=factor(
    inddata1$h10_hbv_rdt, 
    levels = c(0, 1),
    labels = c("HBsAg-", "HBsAg+")))
#labels = c("Negative", "Positive")))

cases_at <- inddata1 %>%
  dplyr::filter(inddata1$hr3_relationship!=1 & inddata1$i27a_rdt_result==1) %>% 
  dplyr::group_by(hr3_relationship_f_eng, h10_hbv_rdt_f_2) %>% 
  dplyr::summarise(n=n()) %>% 
  ggplot(aes(fill=hr3_relationship_f_eng, x=h10_hbv_rdt_f_2, y=n))+
  geom_col(position = position_dodge2(preserve = "single"))+
  labs(x="Index mother HBV status at prenatal screening", fill="", y="Household members")+
  scale_fill_brewer(palette = "Paired")+
  ylim(0, 15)+
  geom_text(aes(label = ..count..), stat = "count", hjust = 0.5, vjust = -0.5, position = position_dodge(0.9))+
  ggtitle("Intent-to-treat")+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 30), #for presentation
        #legend.text = element_text(size = 15),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 15))

# ggsave('./plots/relation.png', width=9, height=9)

##Perprot relation
cases_pp <- 
  inddata1 %>%
  dplyr::filter(inddata1$hr3_relationship!=1 & inddata1$i27a_rdt_result==1) %>% 
  dplyr::group_by(hr3_relationship_f_eng, perprot_h10_f) %>% 
  dplyr::summarise(n=n()) %>% 
  ggplot(aes(fill=hr3_relationship_f_eng, x=perprot_h10_f, y=n))+
  geom_col(position = position_dodge2(preserve = "single"))+
  labs(x="Index mother HBV status at enrollment", fill="", y="")+ #Household members
  scale_fill_brewer(palette = "Paired")+
   ylim(0, 15)+
  ggtitle("Per protocol")+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 30), #for presentation
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 15))
#ggsave('./plots/relationperprot.png', width=9, height=9)

cases_at + cases_pp + plot_layout(nrow=1, ncol = 2) + plot_annotation(tag_levels = 'A')

# output
ggsave('./plots/relationsAB.png', width=15, height=9)

## Household member broad categories
obs <- ggplot(inddata1, aes(x=h10_hbv_rdt_f, fill=hhmemcat_f))+
  geom_bar(position = "dodge")+
  # labs(x="Index mother HBV status", fill="", y="Individuals enrolled")+
  labs(x="Index mother status", fill="", y="Participants enrolled")+
  scale_fill_manual(values = c('#ff9966','#669999','#ff6666','#ffcc66','#33cccc','#00429d'))+
  #scale_fill_manual(values = c('#73a2c6','#f4777f'))+
  ylim(c(0, 300))+
  geom_text(aes(label = ..count..), stat = "count", hjust = 0.5, vjust = -0.5, position = position_dodge(0.9))+
  ggtitle("Index mothers, offspring, and other household members")+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 15), #for presentation
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

# expected hh distribution
expcounts <- data.frame(groups=c("Other household member","Direct offspring" ,"Index mother"),
                  counts=c(300, 200, 100))

# Assigning colors manually 
#exp <- 
  ggplot(data=expcounts, aes(x=reorder(groups, -counts), y=counts,fill=groups))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#669999",
                             "#ff6666",
                             "#ff9966"))+
  ylim(c(0, 300))+
  labs(x="", fill="", y="Expected enrollment")+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 15), #for presentation
        #legend.text = element_text(size = 15),
        #legend.position = "none",
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 15)) 
exp     
exp + obs + plot_layout(nrow=1, ncol = 2) + plot_annotation(tag_levels = 'A')


# Table 3: Risk factor prevalence------------------------------
#all vars
tab3_vars_all <- c('age_combined', "maritalrisk","hr4_sex_f","cpshbvprox_rev","i6_comb_yr","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
             "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
             "i17_tattoo_f", "i16_traditional_scarring_f",
             'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', 'debutsex_all','debutsex_miss',"debutsex_cat","part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")
# factor vars
tab3_vars_cat <- c( "maritalrisk","hr4_sex_f","cpshbvprox_rev","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
                   "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
                   "i17_tattoo_f", "i16_traditional_scarring_f",
                   'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', 'debutsex_all','debutsex_miss',"debutsex_cat","part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")

#first step in create table 3
tab3 <- CreateTableOne(vars = tab3_vars_all, factorVars = tab3_vars_cat, data=inddata1, strata = c("i27a_rdt_result_f", "hhmemcat_f"), addOverall = T)
tab3_itt <- CreateTableOne(vars = tab3_vars_all, factorVars = tab3_vars_cat, data=moms, strata = c("h10_hbv_rdt_f"), addOverall = T)

tab3_enr <- print(tab3 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(tab3_enr, file = "tab3_enr.csv")

tab3_itt_p <- print(tab3_itt ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(tab3_itt_p, file = "tab3_itt_p.csv")
