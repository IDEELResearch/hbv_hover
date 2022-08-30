# Run 01_datacleaning.R first

# packages for this program
library(tableone)

# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

hhdata2 <- merge(hhdata1, hhIDs, by = c("hrhhid"), all.x = T)

summary(hhdata2)

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
hover_ind_tab1 <- CreateTableOne(vars = tab1_ind, factorVars = catvars_ind, data=inddata1, strata = c("h10_hbv_rdt_f","i27a_rdt_result_f"), addOverall = T)

tab1ind_exp <- print(hover_ind_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1ind_exp

## To_DO on ind questions
# - education - distinguish those currently in school (eg secondary school) from adults who are out of school but only completed part of secondary school

