# Run 01_datacleaning.R first

# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

hhdata2 <- merge(hhdata1, hhIDs, by = c("hrhhid"), all.x = T)

summary(hhdata2)

#organize variables for table 1
# all vars
tab1varcleaned <- c("n","maternity","modernfloor", "modernroof","modernwindow","modernwall","modernhousing", "h5_cultivable_land_f", "h5_cultivable_land_f", "wealth_R", "h8_nail_cutting_f", "h8a_nail_clippers_owned", "h8b_nail_filer_owned","h9_razor_f", "h8a_razer_owned")
numericvars <- c("n") # age, under 5s in hh, total hh members
catvars <- c("maternity" ,"modernfloor", "modernroof","modernwindow","modernwall","modernhousing", "h5_cultivable_land_f", "wealth_R", "h8_nail_cutting_f", "h8a_nail_clippers_owned", "h8b_nail_filer_owned", "h9_razor_f", "h8a_razer_owned")

#first step in create table 1
hover_hh_tab1 <- CreateTableOne(vars = tab1varcleaned, factorVars = catvars, data=hhdata2, strata = "h10_hbv_rdt", addOverall = T)

tab1export <- print(hover_hh_tab1 ,quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab1export

# TODO
# -make maternity factor
# add factor for exposed vs unexposed
# -add all hh back (check on wealth index)
# - add components of wealth index: private water, electricity, others 