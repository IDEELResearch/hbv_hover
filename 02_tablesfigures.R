# Run 01_datacleaning.R first

# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

hhdata2 <- merge(hhdata1, hhIDs, by = c("hrhhid"), all.x = T)




summary(hhdata2)

# all vars
tab1varcleaned <- c("modernfloor", "modernroof","modernwindow","modernwall","modernhousing", "h5_cultivable_land_f", "h5_cultivable_land_f")

numericvars <- c("hhsize", "hv014","hv009", "≈") # age, under 5s in hh, total hh members

catvars <- c("modernfloor", "modernroof","modernwindow","modernwall","modernhousing", "h5_cultivable_land_f")

hover_hh_tab1 <- CreateTableOne(vars = tab1varcleaned, factorVars = catvars, data=hhdata2, strata = "h10_hbv_rdt_f", addOverall = T)
