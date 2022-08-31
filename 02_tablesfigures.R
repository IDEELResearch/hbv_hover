# Run 01_datacleaning.R first

# packages for this program
library(tableone)

# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

hhdata2 <- merge(hhdata1, hhIDs, by = c("hrhhid"), all.x = T)

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


### Table 1 for individuals------------------------------
# all vars
tab1_ind <- c("i27a_rdt_result_f","indexmom","hr3_relationship_f","age_combined","agegrp15_2", "hr4_sex_f","hr9_school_gr_f","hr10_occupation_gr_f","hr11_religion_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f",  "i3_hiv_pos_test_f", "i3a_hiv_treatment_f","i4_fever_f", 
              "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f","i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
              "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f","i26_sex_hx_given_money_f")

# num vars
numvars_ind <- c("age_combined") # age, under 5s in hh, total hh members
# cat vars
catvars_ind <- c("i27a_rdt_result_f","indexmom","hr3_relationship_f","agegrp15_2" ,"hr4_sex_f","hr9_school_gr_f","hr10_occupation_gr_f","hr11_religion_f","hr5_primary_residence_f","hr6_last_night_residence_f","i2_past_hbv_dx_f", "i1_hbv_positive_f", "i3_hiv_pos_test_f","i3a_hiv_treatment_f","i4_fever_f",
                 "i5_pregnancy_f", "i14_shared_razor_f", "i15_shared_nailclippers_f", "i8_transfusion_f", "i9_iv_drug_use_f","i10_street_salon_f","i11_manucure_f", "i12_food_first_chew_f",
                  "i13_shared_toothbrush_f","i16_traditional_scarring_f", "i25_sex_hx_receive_money_f", "i26_sex_hx_given_money_f")
#first step in create table 1
hover_ind_tab1 <- CreateTableOne(vars = tab1_ind, factorVars = catvars_ind, data=inddata1, strata = c("i27a_rdt_result_f", "h10_hbv_rdt_f"), addOverall = T)

tab1ind_exp <- print(hover_ind_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1ind_exp
write.csv(tab1ind_exp, file = "tab1ind_exp.csv")

## To_DO on ind questions
# - education - distinguish those currently in school (eg secondary school) from adults who are out of school but only completed part of secondary school

## misc for CROI abstract------------------------------------------------------------------------
nrow(inddata1 %>% filter(age_combined < 18)) # how many under 18
nrow(inddata1 %>% filter(age_combined < 18))/nrow(inddata1) # %  under 18
addmargins(table(inddata1$hr3_relationship_f)) # how many direct offspring
addmargins(table(inddata1$hr3_relationship_f))/nrow(inddata1) # %  direct offspring
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






