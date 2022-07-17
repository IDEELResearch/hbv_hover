# Run 01_datacleaning.R first

# create table 1
hhIDs <- inddata1 %>%
  group_by(hrhhid) %>% summarize(hhsize=n())

hhdata2 <- merge(hhdata1, hhIDs, by = c("hrhhid"), all.x = T)




summary(hhdata1)

# all vars
tab1varcleaned <- c("adultmalaria", "hv009","hv014","hv105","sex",  "treatedbednet", "modernhousing", "hv270_f", "urbanrural", "landown_f", "hv024_f", "animalown", "cattle", "horses", "goats", "sheep", "chickens", "pigs", "ducks")

numericvars <- c("hhsize", "hv014","hv009") # age, under 5s in hh, total hh members

catvars <- c("adultmalaria", "sex",  "treatedbednet", "modernhousing", "hv270_f","urbanrural", "landown_f", "hv024_f", "animalown", "cattle", "horses","goats","sheep","chickens","pigs","ducks")

animalariatab1 <- CreateTableOne(vars = tab1varcleaned, factorVars = catvars, data=dhs, strata = "adultmalaria", addOverall = T)
