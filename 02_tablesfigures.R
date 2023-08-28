# Run 01_datacleaning.R first

# packages for this program
library(tableone)
library(tidyverse)
#Table 1--------------------
##Household--------------------------------------------------------------------------------
# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

table(hhIDs$hhsize)
hhIDs <- merge(hhIDs, hhdata1[,c("hrhhid","h10_hbv_rdt_f")],by = c("hrhhid"), all.x = T)
hhIDs %>% group_by(h10_hbv_rdt_f) %>% reframe(quantile(hhsize, c(0.25, 0.5, 0.75)))
hhIDs %>% reframe(quantile(hhsize, c(0.25, 0.5, 0.75)))

# summary(hhdata2)

#organize variables for table 1
# all vars
tab1varcleaned <- c("n","maternity","modernfloor", "modernroof","modernwindow","modernwalls","modernhousing", "h5_cultivable_land_f",  "privatewater", "h3_hh_wealth_electr","wealth_R", "h8_nail_cutting_f", "h8a_nail_clippers_owned", "h8b_nail_filer_owned","h9_razor_f", "h8a_razer_owned")
# num vars
numericvars <- c("n") # age, under 5s in hh, total hh members
# cat vars
catvars <- c("maternity" ,"modernfloor", "modernroof","modernwindow","modernwalls","modernhousing", "h5_cultivable_land_f", "privatewater","h3_hh_wealth_electr","wealth_R", "h8_nail_cutting_f", "h8a_nail_clippers_owned", "h8b_nail_filer_owned", "h9_razor_f", "h8a_razer_owned")

#first step in create table 1
hover_hh_tab1 <- CreateTableOne(vars = tab1varcleaned, factorVars = catvars, data=hhdata2, strata = "h10_hbv_rdt", addOverall = T )

tab1hhexport <- print(hover_hh_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1hhexport
write.csv(tab1hhexport, file = "tab1hhexport.csv")

# single stats to add to table 1
# median (IQR) direct offspring enrolled
inddata1 %>% group_by(h10_hbv_rdt) %>% reframe(quantile = scales::percent(c(0.25, 0.5, 0.75)),
                                      numdiroff = quantile(numdiroff, c(0.25, 0.5, 0.75)))



##All individuals------------------------------
# use inddata1_dc for main hover paper; or ind1006pids
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
moms <- inddata1 %>% filter(hr3_relationship == 1) 

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
#median age everyone
inddata1 %>% group_by(h10_hbv_rdt,hhmemcat_f ) %>% summarise(quantile(age_combined, probs=c(0.25, 0.5,0.75)))
inddata1 %>%  summarise(quantile(age_combined, probs=c(0.25, 0.5,0.75)))

# self-reported HIV infection
selfrephiv <- moms %>% filter(i3_hiv_pos_test == 1)
table(selfrephiv$i27a_rdt_result_f, selfrephiv$acq_ind, useNA = "always")
table(selfrephiv$i3a_hiv_treatment_f, selfrephiv$acq_ind, useNA = "always")
table(selfrephiv$hivhaart, selfrephiv$acq_ind, useNA = "always")
# who is not taking HIV
selfrephiv %>% filter(i3a_hiv_treatment==0) %>% summarise(i27a_rdt_result_f,acq_ind, hrhhid )


addmargins(table(moms$acq_ind, moms$i3_hiv_pos_test_f, moms$i27a_rdt_result_f,useNA = "always"))
addmargins(table(moms$acq_ind, moms$i3a_hiv_treatment_f, useNA = "always"))
addmargins(table(moms$acq_ind, moms$i3b_hiv_medications, useNA = "always"))

# sexual hx counts
table(moms$i23_sex_hx_part_past3mo,moms$h10_hbv_rdt_f, useNA = "always") # figure out what 95 means
# wealth and education of index moms who refused sex hx questions
inddata1 %>% filter(indexmom_indic==1 &part3mo_cat==9) %>% count(cpn_maternity, paststudymutexcl,wealth_R,hr9_school_gr_f)





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
tab1_all <- c("age_combined", "hr4_sex_f","hr3relat_simp_f" ,"cpshbvprox","maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f",
                "hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","hivhaart","i4_fever_f", 
                "i5_pregnancy_f")
# num vars
numvars_all <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_all <- c("cpshbvprox","hr4_sex_f","hr3relat_simp_f" ,"maritalrisk","educ_simp_f","hr10_occupation_gr_f","religion_simp_f",
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
inddata1  %>%  group_by(h10_hbv_rdt_f) %>% filter(!is.na(i6_comb_yr) & i6_comb_yr!=0) %>%  summarise(median(i6_comb_yr), quantile(i6_comb_yr, probs=c(0.25, 0.75)))

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
table(moms$i27a_rdt_result_f, moms$i3_hiv_pos_test_f, moms$i3a_hiv_treatment_f)


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
# vars of interest: c("malepartpos","i22_sex_hx_age_1st","maritalrisk_f","hr4_sex_f","cpshbvprox_rev","i6_comb_yr","i7_diabetes_f",'i8_transfusion_f', "transfus_num","i10_street_salon_f",
  #            "wealth_R", "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
  #            "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")

# hh member type as stratum var
table(inddata1$hhmemcat_f) # 3 levels
table(inddata1$hhmemcat) # 3 levels
table(inddata1$hhmemcat_4_f) # 4 levels (male partner separate)
#make hbsag variable for output - mother's CPN result and others' enrollment result
inddata1$tab3hbv <- ifelse(inddata1$hhmemcat==2, inddata1$h10_hbv_rdt, inddata1$i27a_rdt_result) # hhmemcat==2 are index moms
table(inddata1$tab3hbv, useNA = "always")

inddata1 <- inddata1 %>% 
  dplyr::mutate(tab3hbv_f=factor(
    inddata1$tab3hbv, 
    levels = c(0, 1),
    labels = c("HBsAg-", "HBsAg+")))

# now restrict to no missing (3 direct offspring weren't tested)
table(inddata1$i27a_rdt_result_f, inddata1$hhmemcat_f, useNA = "always")
inddata1_nomiss <- inddata1 %>% filter(!(is.na(tab3hbv_f)))

# vars for everyone
tab3_all <- c("i22_sex_hx_age_1st","i6_comb_yr","i7_diabetes_f",# individual
              "wealth_R",   "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f', # household
             # comm factors. - note that transfusions could be from family member in hh
               'i8_transfusion_f', "transfus_num","i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")
tab3_num <- c("i22_sex_hx_age_1st","i6_comb_yr")
# factor vars
tab3_cat <- c("i7_diabetes_f",'i8_transfusion_f', "transfus_num", "i10_street_salon_f","i11_manucure_f",
              "wealth_R", "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
              "i17_tattoo_f", "i16_traditional_scarring_f",
              'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f',"debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")
#...................
# vars for specific stratum groups, add to table separate
# index mothers only: malepartpos, maritalrisk_f
tab3_mi_cat_all <- c("malepartpos", "maritalrisk_f")
# direct offspring: "cpshbvprox_rev", "hr4_sex_f"
# other hh memm: cpshbvprox_rev, "hr4_sex_f"
tab3_do_oth_cat <- c("cpshbvprox_rev", "hr4_sex_f")
#...................return to these after main table

#first step in create table 3
tab3_main <- CreateTableOne(vars = tab3_all, factorVars = tab3_cat, data=inddata1_nomiss, strata = c("tab3hbv_f", "hhmemcat_f"), addOverall = T )

tab3_main_p <- print(tab3_main ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(tab3_main_p, file = "tab3_main_p.csv")

# group specific variables
# moms
tab3_moms <- CreateTableOne(vars = tab3_mi_cat_all, factorVars = tab3_mi_cat_all, data=moms, strata = c("h10_hbv_rdt_f"), addOverall = T)
tab3_moms_p <- print(tab3_moms ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(tab3_moms_p, file = "tab3_moms_p.csv")
table(moms$malepartner, moms$malepartpos)
table(moms$malepartner, moms$h10_hbv_rdt_f)

# others
inddata1_nomiss_o <- inddata1_nomiss %>% filter(hhmemcat!=2)

tab3_dooth <- CreateTableOne(vars = tab3_do_oth_cat, factorVars = tab3_do_oth_cat, data=inddata1_nomiss_o, strata = c("tab3hbv_f", "hhmemcat_f"), addOverall = T)
tab3_dooth_p <- print(tab3_dooth ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(tab3_dooth_p, file = "tab3_dooth_p.csv")






#Exploratory data analysis for vertical/horizontal relationships------------------------------
table(directoff$i3_hiv_pos_test, useNA = "always", directoff$paststudymutexcl)

hivdo <- directoff %>% filter(i3_hiv_pos_test==1)

table(hivdo$age_combined, useNA = "always", hivdo$paststudymutexcl)
median(othermemb$age_combined)
table(othermemb$hr3_relationship_f)

inddata1 %>% filter(indexmom_indic==1) %>% count(h10_hbv_rdt_f, i3_hiv_pos_test_f)
# did newly exposed women have new sexual partners? no
inddata1 %>% filter(indexmom_indic==1 &serostatchange==1) %>% count(perprot_h10,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num )
#sexual hx of two women with incident infections
inddata1 %>% filter(indexmom_indic==1 &serochangedir=="incident") %>% count(i3_hiv_pos_test_f,age_combined,i22_sex_hx_age_1st,i23_sex_hx_part_past3mo,i23a_sex_hx_past3mo_num,i24_sex_hx_part_past1yr,i24a_sex_hx_past1yr_num,i25_sex_hx_receive_money_f,i26_sex_hx_given_money_f )

inddata1 %>% filter(hhmemcat_4==1) %>% count(i3_hiv_pos_test_f, i27a_rdt_result_f,h10_hbv_rdt_f,paststudymutexcl)
inddata1 %>% filter(hhmemcat_4==1) %>% count(paststudymutexcl)
inddata1 %>% filter(hhmemcat_4==1 &i3_hiv_pos_test_f=="Yes") %>% count(hrhhid)

# female part hbv/hiv status by those w male partner
inddata1 %>% filter(malepartner==1 & indexmom_indic==1) %>% count(h10_hbv_rdt_f, i3_hiv_pos_test_f)

inddata1 %>% filter(hr3_relationship==2) %>% count(h10_hbv_rdt_f, i27a_rdt_result_f,i3_hiv_pos_test_f,paststudymutexcl)

inddata1 %>% group_by(i27a_rdt_result_f) %>% count(hr3relat_simp_f)
table(inddata1$hr3_relationship_f,inddata1$i27a_rdt_result_f)

inddata1 %>% filter(i27a_rdt_result==1 & hr3_relationship==3) %>% reframe(pid)
inddata1 %>% filter(hrhhid=="HRK2081") %>% reframe(hr3_relationship_f, i27a_rdt_result_f)
ind1006 %>% filter(hrhhid=="HRK2081") %>% reframe(hr3_relationship_f, i27a_rdt_result_f, age_combined)

inddata1 %>% group_by(h10_hbv_rdt) %>% reframe(quantile = scales::percent(c(0.25, 0.5, 0.75)),
                                               numdiroff = quantile(numdiroff, c(0.25, 0.5, 0.75)))

# known vertical relationships
# mothers of index mother
inddata1 %>% group_by(h10_hbv_rdt) %>% filter((hr3_relationship==6 & hr4_sex==1) | indexmom_indic==1) %>% count(hr3_relationship, i27a_rdt_result_f)

# sisters and niece/nephew of index mother
inddata1 %>% group_by(h10_hbv_rdt) %>% filter(hr3_relationship==8 | hr3_relationship==9) %>% count(hr3_relationship, i27a_rdt_result_f)
# make var for if a sibling and if a niece/nephew enrolled and age diff
counts_sibnieceneph <- inddata1 %>% group_by(hrhhid) %>% summarise(sibs = sum(hr3_relationship==8), nieceneph = sum(hr3_relationship==9))
addmargins(table(counts_sibnieceneph$sibs))
addmargins(table(counts_sibnieceneph$nieceneph))
counts_sibnieceneph$both <- ifelse(counts_sibnieceneph$sibs>0 & counts_sibnieceneph$nieceneph >0,1,0)
table(counts_sibnieceneph$both)

atleast1 <- counts_sibnieceneph %>% filter(both==1)
investigate_vert <-  inddata1 %>% filter(hrhhid %in% atleast1$hrhhid) %>% select(c("hrhhid","hr3_relationship","hr3_relationship_f", "hr4_sex_f","age_combined","hrname_last","hrname_post","hrname_first"))

