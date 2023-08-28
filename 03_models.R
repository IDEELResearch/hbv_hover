## analyze hover data
library(lme4)
library(lmtest)
library(geepack)
library(tidyverse)

#Comparison 1: odds of infection of household members in exposed vs unexposed--------------- 
# using index mother's CPN/recruitment screening result, not result at enrollment
hhmemb <- inddata1 %>% filter(indexmom=="Household member")
#exclude those with missing results
hhmemb_nomiss <- hhmemb %>% filter(i27a_rdt_result==0 | i27a_rdt_result==1)

# 2x2 values
addmargins(table(hhmemb_nomiss$i27a_rdt_result_f, hhmemb_nomiss$h10_hbv_rdt_f))

# basic model not accounting for clustering
comp1 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "log"), data=hhmemb_nomiss)
summary(comp1)
exp(comp1$coefficients)
exp(confint(comp1))

# account for clustering
###GEE using geeglm() does not work 
# comp1gee_ind <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=hhmemb_nomiss, family=binomial, corstr="ind")

###glmer------
# log link for prevalence ratio
comp1mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0
summary(comp1mm)
fixef(comp1mm)
exp(fixef(comp1mm))

# NEED TO FIGURE OUT CI FROM ML MODEL
exp(confint((comp1mm)))
# lmerTest::con
lrtest(comp1, comp1mm)
library(broom)
library(broom.mixed)
tidy(comp1mm, effects="fixed", conf.int=T, confint.method = "boot")

# checking the values of various CI approaches
exp(confint(comp1mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp1mm, method = c("boot"), boot.type=c("perc")))
exp(confint(comp1mm, method = c("Wald"))) # proceeding with Wald
exp(confint(comp1mm, method = c("profile")))

# logit link for OR
comp1mm_logit <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_nomiss)
summary(comp1mm_logit)
fixef(comp1mm_logit)
exp(fixef(comp1mm_logit))

#....................................................................................................
# Comparison 2: odds of infection in exposed households comparing offspring vs non offspring---------
hhmemb_nomiss %>% group_by(h10_hbv_rdt,directoff, i27a_rdt_result)%>%  count()

# original approach from proposal
hhmemb_exp <- hhmemb %>% filter(h10_hbv_rdt==1)
addmargins(table(hhmemb_exp$i27a_rdt_result_f, hhmemb_exp$hhmemcat_f))

### glmer----
comp2mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_exp, nAGQ = 0)
summary(comp2mm)
fixef(comp2mm)
exp(fixef(comp2mm))
exp(confint(comp2mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp2mm, method = c("Wald")))

#....................................................................................................
#Comparison 3: odds of infection in unexposed households comparing offspring vs non offspring--------------------
hhmemb_unexp <- hhmemb_nomiss %>% filter(h10_hbv_rdt==0)

addmargins(table(hhmemb_unexp$directoff, hhmemb_unexp$i27a_rdt_result_f))
inddata1 %>% group_by(h10_hbv_rdt, perprot_h10,i27a_rdt_result_f, directoff ) %>% count()

###glmer-----
comp3mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_unexp, nAGQ = 0)
summary(comp3mm)
fixef(comp3mm)
exp(fixef(comp3mm))
exp(confint(comp3mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp3mm, method = c("Wald")))

#....................................................................................................
#Comparison 4: odds of infection in exposed direct off vs unexposed direct off-----
directoff <- hhmemb_nomiss %>% filter(directoff==1)
addmargins(table(directoff$i27a_rdt_result_f, directoff$h10_hbv_rdt_f))

###glmer-----
comp4mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=directoff, nAGQ = 0)
summary(comp4mm)
fixef(comp4mm)
exp(fixef(comp4mm))
exp(confint(comp4mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp4mm, method = c("Wald")))

#....................................................................................................
#Comparison 5: odds of infection in exposed other mem vs unexposed other mem------
othermemb <- hhmemb_nomiss %>% filter(directoff==0)
addmargins(table(othermemb$i27a_rdt_result_f, othermemb$h10_hbv_rdt_f))
nrow(othermemb)
###glmer-----
comp5mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=othermemb, nAGQ = 0)
summary(comp5mm)
fixef(comp5mm)
exp(fixef(comp5mm))
exp(confint(comp5mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp5mm, method = c("Wald")))

#..................................................................
# male partners
men <- hhmemb %>% filter(hr3_relationship==2)

comp_husb <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=men, nAGQ = 0)
summary(comp_husb)
fixef(comp_husb)
exp(fixef(comp_husb))
exp(confint(comp_husb, method = c("boot"), boot.type=c("basic")))
exp(confint(comp_husb, method = c("Wald")))

# dont need to account for clustering since only one per hh
comp_husb <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "log"), data=men)
summary(comp_husb)
exp(comp_husb$coefficients)
exp(confint(comp_husb, method = c("boot"), boot.type=c("basic")))
exp(confint(comp_husb, method = c("Wald")))


##Kim's suggestion--interaction term--------
# maybe best from SAS
comp2_int <- glmer(i27a_rdt_result ~ h10_hbv_rdt +directoff+h10_hbv_rdt*directoff +(1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss)
summary(comp2_int)
fixef(comp2_int)
exp(fixef(comp2_int))

# Risk factor anlaysis -------------------
## clean up the below------------------------------------------------------------------------------------------
#Risk factor identification---------------
# run for: moms; directoff; othermemb or othermember
# moms - no hh clustering
# DO and other members- still clustering

# risk factor grouping: risk_ind, risk_hh, risk_comm
# subpopulations: moms, directoff, othermember, men

# individual level - different subset of variables relevant to the different subpopulations
# breaking down by ind/hh/comm to organize the figure - might be a way to do this within code
# all individual variables:  c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev", "malepartpos","wealth_R") 
# remove those with cell counts in 2x2 < 5 (eg "malepartpos")

# changes 6 June: trans_bin, debutsex_indic, partner3mo_bin, newpartner3mo_indic, transactionalsex
allmivar <- c("age_combined","maritalrisk_f","wealth_R_lowestv","sharedhhobj",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin","newpartner3mo_indic")
alldovar <- c("age_combined","hr4_sex_f","cpshbvprox_rev", "wealth_R_lowestv","sharedhhobj",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin","newpartner3mo_indic")
allmenvar <- c("age_combined","wealth_R_lowestv","sharedhhobj",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin", "newpartner3mo_indic")
allothvar <- c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev","sharedhhobj",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin", "newpartner3mo_indic")

# when i try to run glm/glmer for one that has <2 levels, gives an error - how to add a step in the function to check how many levels?
# for now running separately by subgroup
risk_ind_mi <-  c("age_combined","maritalrisk_f","malepartpos") # "hr4_sex_f","cpshbvprox_rev", <- figure out to have single group of ind vars 
risk_ind_do <-  c("age_combined","hr4_sex_f","cpshbvprox_rev") #"maritalrisk_f","malepartpos",
risk_ind_men <-  c("age_combined","wealth_R_lowestv") #"maritalrisk_f","malepartpos",
risk_ind_oth <-  c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev") #"malepartpos",
risk_ind_oth_sub <-  c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev") #, "wealth_R" leads to non-positive variance

# household
risk_hh_all <-  c( "sharedhhobj",'i12_food_first_chew_f',"wealth_R_lowestv") 
# previous list:
#risk_hh_all <-  c( "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f') 
# might not need divisions by type

# community
risk_comm_all <-  c("trans_bin","i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin","newpartner3mo_indic")
# might not need divisions by type

# moms function (no clustering)
# original
itt_model_moms <- function(var){ # glm function
  m <- glm(as.formula(paste0('h10_hbv_rdt ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

itt_model_moms99 <- function(var){ # glm function
  m <- glm(as.formula(paste0('h10_hbv_rdt ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"), level=0.99)) %>% filter(stringr::str_detect(term, var))}

view(glm_test)


options(scipen = 999) # remove scientific notation

# from above: directoff <- hhmemb %>% filter(directoff==1)
# glmer CIs have a row for sig01 and intercept - need to skip these
# revised model to get correct CIs
itt_model_do <- function(var){ # glm function
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=directoff, family=binomial("logit"))
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis <- as.data.frame(confint(m, method = c("Wald")))
  cis <- tibble::rownames_to_column(cis, "term") %>% setNames(c("term", "LCI", "UCI"))
  cis <- cis %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis) 
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}


# from above : men <- hhmemb %>% filter(hr3_relationship==2)
# cluster model not important
itt_model_men <- function(var){ # glm function
  m <- glm(as.formula(paste0('h10_hbv_rdt ~', var)), data=men, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

# other hh members, excluding men
othermemb_nomen <- inddata1 %>% filter(hhmemcat_4==0)

# adding men back
itt_model_oth <- function(var){ # glmer function
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=othermember, family=binomial("logit"))
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis <- as.data.frame(confint(m, method = c("Wald")))
  cis <- tibble::rownames_to_column(cis, "term") %>% setNames(c("term", "LCI", "UCI"))
  cis <- cis %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis) 
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}

# view results
# index mothers
library(broom)
glm_mi <- map_dfr(allmivar,itt_model_moms) 
glm_test99 <- map_dfr(allmivar,itt_model_moms99) 

glm_mi <- mutate(glm_mi, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
# mutate(glm_mi, across(where(is.numeric), round, 3),na.rm = TRUE) # better way to print/view results and restrict decimals without going into sci not
colnames(glm_mi) <- c('term','logodds','std.error','statistic','p.value','LCI_95','UCI_95')
# could combine these tables before the figure
rownames(glm_mi) <- 1:nrow(glm_mi)

glm_test99 <- mutate(glm_test99, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
colnames(glm_test99) <- c('term','logodds','std.error','statistic','p.value','LCI_99','UCI_99')
rownames(glm_test99) <- 1:nrow(glm_test99)

glm_mi <- merge(glm_mi, glm_test99[, c('term','LCI_99','UCI_99')], by = c('term'))

glm_mi$group <- "Index mothers"
glm_mi <- glm_mi %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_mi, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))
glm_mi <- glm_mi %>% arrange(desc(level))
view(glm_mi)
write.csv(glm_mi, file = "glm_mi.csv" )

# index mom counts
table(moms$h10_hbv_rdt_f,useNA = "always" )
table(moms$h10_hbv_rdt_f,moms$age_combined, useNA = "always" )
table(moms$maritalrisk_f,moms$h10_hbv_rdt_f, useNA = "always" )
table(moms$wealth_R_lowestv, moms$h10_hbv_rdt_f,useNA = "always" ) #  #lowest (=0) vs upper 3 (=1)
table(moms$sharedhhobj, moms$h10_hbv_rdt_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(moms$i12_food_first_chew_f,moms$h10_hbv_rdt_f, useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(moms$trans_bin, moms$h10_hbv_rdt_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(moms$i10_street_salon_bin, moms$h10_hbv_rdt_f,useNA = "always" ) # 1 street salon use
table(moms$i11_manucure_f, moms$h10_hbv_rdt_f,useNA = "always" )
table(moms$i17_tattoo_bin,moms$h10_hbv_rdt_f, useNA = "always" ) 
table(moms$i16_traditional_scarring_f,moms$h10_hbv_rdt_f, useNA = "always" )
table(moms$transactionalsex,moms$h10_hbv_rdt_f, useNA = "always" )
table(moms$debutsex_indic,moms$h10_hbv_rdt_f, useNA = "always" )
table(moms$partner3mo_bin,moms$h10_hbv_rdt_f, useNA = "always" )
table(moms$newpartner3mo_indic,moms$h10_hbv_rdt_f, useNA = "always" )

fisher.test(moms$h10_hbv_rdt_f,moms$i17_tattoo_bin)
fisher.test(moms$h10_hbv_rdt_f,moms$partner3mo_bin)


# direct offspring
glmer_do <- map_dfr(alldovar,itt_model_do) 

mutate(glmer_do, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
colnames(glmer_do) <- c('term','logodds','std.error','statistic','p.value','LCI','UCI')
# glmer_do_ind <- glmer_do_ind %>% select(-c("effect", "glmrgroup"))
# rownames(glmer_do_ind) <- 1:nrow(glmer_do_ind)
glmer_do$group <- "Direct offspring"
glmer_do <- glmer_do %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_do, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))

# skipping
# men
glmer_men <- map_dfr(allmenvar,itt_model_men) 
mutate(glmer_men, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
colnames(glmer_men) <- c('term','logodds','std.error','statistic','p.value','LCI','UCI')
rownames(glmer_men) <- 1:nrow(glmer_men)
glmer_men$group <- "Male partners"
glmer_men <- glmer_men %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_men, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))
#--------#

# other hh members
glmer_oth <- map_dfr(allothvar,itt_model_oth) 
mutate(glmer_oth, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
colnames(glmer_oth) <- c('term','logodds','std.error','statistic','p.value','LCI','UCI')
#glmer_oth_ind <- glmer_oth_ind %>% select(-c("effect", "glmrgroup"))
#rownames(glmer_oth_ind) <- 1:nrow(glmer_oth_ind)
glmer_oth$group <- "Other household members"
glmer_oth <- glmer_oth %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_oth_sub, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))

# combined results from individual stats
ind_all <- rbind(glm_mi, glmer_do, glmer_oth) # glmer_men
ind_all$level <- as.factor(ind_all$level)
ind_all$group <- as.factor(ind_all$group)
ind_all <- tibble::rownames_to_column(ind_all, "index") # make integer index to call on later
nrow(ind_all)
view(ind_all)

# rename variables for plot
ind_all$term[ind_all$term == "wealth_R_lowestv"] <- "Upper wealth quartiles (Q4-Q2) vs lowest (Q1)"
#ind_all$term[ind_all$term == "wealth_R2"] <- "Higher wealth (Q3) vs lowest (Q1)"
#ind_all$term[ind_all$term == "wealth_R1"] <- "Lower wealth (Q2) vs lowest (Q1)"
ind_all$term[ind_all$term == "maritalrisk_fNot married"] <- "Marriage: Never vs married"
ind_all$term[ind_all$term == "maritalrisk_fDivorced/Widowed"] <- "Marriage: Divorced/widowed vs married"
# ind_all$term[ind_all$term == "malepartpos"] <- "Male partner HBsAg pos vs neg"
ind_all$term[ind_all$term == "hr4_sex_fFemale"] <- "Female vs male"
ind_all$term[ind_all$term == "cpshbvprox_revProbably not vaccinated"] <- "HBV vaccination: likely not vs probably"
ind_all$term[ind_all$term == "cpshbvprox_revPossibly vaccinated"] <- "HBV vaccination: possibly vs probably"
ind_all$term[ind_all$term == "age_combined"] <- "Age (continuous, years)"
ind_all$term[ind_all$term == "sharedhhobj1"] <- "Shares razors/nail clippers/toothbrushes in household"
#ind_all$term[ind_all$term == "i13_shared_toothbrush_fYes"] <- "Shares toothbrushes in house"
#ind_all$term[ind_all$term == "i14_shared_razor_fYes"] <- "Shares razors in house vs not"
#ind_all$term[ind_all$term == "i15_shared_nailclippers_fYes"] <- "Shares nail clippers in house vs not"
ind_all$term[ind_all$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
#transfusions changed to binary: 1+ or refused vs none/missing
#ind_all$term[ind_all$term == "i8_transfusion_fYes"] <- "Past transfusion vs never"
#ind_all$term[ind_all$term == "transfus_num9"] <- "≥4 transfusions vs none"
#ind_all$term[ind_all$term == "transfus_num3"] <- "3 transfusions vs none"

ind_all$term[ind_all$term == "i10_street_salon_fYes"] <- "Uses street salons vs not" #i10_street_salon_bin
ind_all$term[ind_all$term == "i11_manucure_fYes"] <- "Manicures outside home vs not"

ind_all$term[ind_all$term == "i17_tattoo_fYes"] <- "Tattoos: Yes vs none" # i17_tattoo_bin change
#ind_all$term[ind_all$term == "i17_tattoo_fRefused"] <- "Tattoos: refused vs none" # i17_tattoo_bin change
ind_all$term[ind_all$term == "i16_traditional_scarring_fYes"] <- "Traditional scarring vs none"

ind_all$term[ind_all$term == "trans_bin1"] <- "1+ past transfusion vs none"
ind_all$term[ind_all$term == "transactionalsex1"] <- "Has engaged in transactional sex or refused to answer vs no"
ind_all$term[ind_all$term == "debutsex_indic1"] <- "Sexual debut <18 yrs or refused vs ≥18 yrs"
ind_all$term[ind_all$term == "partner3mo_bin1"] <- "≥2 Sexual partners in last 3 months or refuse vs ≤1"
ind_all$term[ind_all$term == "newpartner3mo_indic1"] <- "≥1 New sexual partner in last 3 months or refuse vs none new"

# others
max(ind_all_nomiss_abs$logodds)
min(ind_all_nomiss_abs$LCI)
max(ind_all_nomiss_abs$UCI)

# restrict to those with reasonable conf ints (some in 1000s)
ind_all_nomiss <- ind_all %>% filter_at(vars(logodds,LCI, UCI),all_vars(!is.na(.)))
nrow(ind_all_nomiss)
ind_all_nomiss_abs <- ind_all_nomiss %>% filter_at(vars(logodds,LCI, UCI),all_vars(abs(.)<50))
# others to exclude: refuse tattoo (so small)
ind_all_nomiss_abs <- ind_all_nomiss_abs %>% filter(term != "i17_tattoo_fRefused")

nrow(ind_all_nomiss_abs)

# look through those with really wide CLRs and decide any coding changes to improve counts
riskfactelim <- ind_all %>% filter(!(index %in% ind_all_nomiss_abs$index))
riskfactelim <- riskfactelim %>% select(-c("statistic", "p.value"))

view(ind_all_nomiss_abs)
write.csv(ind_all_nomiss_abs, file = "ind_jun7_red.csv" )
write.csv(ind_all, file = "ind_all_jun7.csv" )


# to plot
#p<- 
ind_all_nomiss_abs %>% filter(UCI<=10) %>% 
  mutate(term = (fct_reorder(fct_rev(term), as.integer(level)))) %>%
  ggplot(aes(x=term, y=estimate)) + #group = interaction(level); alpha = level
  geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=term, y=logodds, ymin=LCI, ymax=UCI), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=logodds, group=group, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#A6DEAE","#020E54","deeppink3","#FF700A"))+ #deeppink3
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_discrete(breaks=test$id,labels=labels) + #, trans = "reverse"
  coord_flip() + #theme_bw() + 
  #ylim(0, 30)+
  labs(x="", y="Log(OR) of HBsAg+") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20)) +
  #ggtitle("Log(OR) of HBsAg by individual-level") +
  #facet_grid(~group)
  facet_wrap(~ factor(group, levels = c("Index mothers", "Direct offspring", "Other household members")),nrow = 1) #"Male partners", 

  ggsave('./plots/fig4.png', width=25, height=10)
  
# check significant unadjusted 

# marriage + age index mothers
marage <- glm(h10_hbv_rdt ~ maritalrisk_f + age_combined, data=moms, family=binomial("logit"))
summary(marage)  
confint(marage, method = c("Wald"))

# sex partners last 3 mos
thremo <- glm(h10_hbv_rdt ~  partner3mo_bin, data=moms, family=binomial("logit"))
summary(thremo)
confint(thremo, method = c("Wald"))

# wealth direct offspring
wedo <- glmer(i27a_rdt_result ~  (1 | hrhhid) + wealth_R_lowestv, data=directoff, family=binomial("logit"),REML = FALSE)
wedo <- glmer(i27a_rdt_result ~  (1 | hrhhid) + wealth_R_lowestv, data=directoff, family=binomial("logit"))
wedo <- glm(i27a_rdt_result ~   wealth_R_lowestv, data=directoff, family=binomial("logit"))
summary(wedo)
confint(wedo)
(confint(wedo,parm="beta_",method="Wald"))


table(directoff$i12_food_first_chew_f, directoff$i27a_rdt_result)


table(directoff$wealth_R_lowestv, directoff$i27a_rdt_result)

# age
doage <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_combined, data=directoff, family=binomial("logit"))
wedo <- glm(i27a_rdt_result ~   wealth_R_lowestv, data=directoff, family=binomial("logit"))
summary(doage)
confint(doage)

cc <- confint(doage,parm="beta_")  ## slow (~ 11 seconds)
ctab <- cbind(est=fixef(doage),cc)

exp(confint(doage,parm="beta_",method="Wald"))

do_vacc <- glmer(i27a_rdt_result ~  (1 | hrhhid) + cpshbvprox_rev, data=directoff, family=binomial("logit"))
summary(do_vacc)
(confint(do_vacc,parm="beta_",method="Wald"))

# other members
oth_age <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_combined, data=othermember, family=binomial("logit"))
summary(oth_age)
(confint(oth_age,parm="beta_",method="Wald"))


#"Risk factor" with exp/unexp separated------------------
# updated model

# make separate exposed/unexposed datasets
table(inddata1$h10_hbv_rdt, inddata1$directoff)
directoffexp <-directoff %>% filter(h10_hbv_rdt == 1)
nrow(directoffexp)
directoffunexp <-directoff %>% filter(h10_hbv_rdt == 0)
nrow(directoffunexp)
library(lme4) 
library(tidyr)

## Direct offspring exposed---------
alldovar <- c("age_combined","hr4_sex_f","cpshbvprox_rev", "wealth_R_lowestv","sharedhhobj",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin","newpartner3mo_indic")

itt_model_doexp <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=directoffexp, family=binomial("logit"))
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis95 <- as.data.frame(confint(m, method = c("Wald")))
  cis95 <- tibble::rownames_to_column(cis95, "term") %>% setNames(c("term", "LCI_95", "UCI_95"))
  cis95 <- cis95 %>%  filter(stringr::str_detect(term, var))
  cis99 <- as.data.frame(confint(m, method = c("Wald"), level = 0.99))
  cis99 <- tibble::rownames_to_column(cis99, "term") %>% setNames(c("term", "LCI_99", "UCI_99"))
  cis99 <- cis99 %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis95,cis99)
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}

glmer_doexp <- map_dfr(alldovar,itt_model_doexp) 

view(glmer_doexp)

glmer_doexp_rd <- mutate(glmer_doexp, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
#colnames(glmer_do) <- c('term','logodds','std.error','statistic','p.value','LCI','UCI')
# glmer_do_ind <- glmer_do_ind %>% select(-c("effect", "glmrgroup"))
# rownames(glmer_do_ind) <- 1:nrow(glmer_do_ind)
glmer_doexp_rd$group <- "Direct offspring"
glmer_doexp_rd <- glmer_doexp_rd %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_do, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))
glmer_doexp_rd <- glmer_doexp_rd %>% relocate(group, level)
glmer_doexp_rd <- subset(glmer_doexp_rd, select = -c(term.1))

write.csv(glmer_doexp_rd, file = "glmer_doexp_rd.csv" )

# add counts
addmargins(table(directoffexp$i27a_rdt_result,directoffexp$age_combined, useNA = "always" ))
addmargins(table(directoffexp$hr4_sex_f,directoffexp$i27a_rdt_result_f, useNA = "always" )) 
addmargins(table(directoffexp$wealth_R_lowestv, directoffexp$i27a_rdt_result_f,useNA = "always" )) #  #lowest (=0) vs upper 3 (=1)
addmargins(table(directoffexp$sharedhhobj, directoffexp$i27a_rdt_result_f,useNA = "always" )) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
addmargins(table(directoffexp$i12_food_first_chew_f,directoffexp$i27a_rdt_result_f, useNA = "always" )) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
addmargins(table(directoffexp$trans_bin, directoffexp$i27a_rdt_result_f,useNA = "always" )) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
addmargins(table(directoffexp$i10_street_salon_bin, directoffexp$i27a_rdt_result_f,useNA = "always" )) # 1 street salon use
addmargins(table(directoffexp$i11_manucure_f, directoffexp$i27a_rdt_result_f,useNA = "always" )) 
addmargins(table(directoffexp$i17_tattoo_bin,directoffexp$i27a_rdt_result_f, useNA = "always" )) 
addmargins(table(directoffexp$i16_traditional_scarring_f,directoffexp$i27a_rdt_result_f, useNA = "always" )) 
addmargins(table(directoffexp$transactionalsex,directoffexp$i27a_rdt_result_f, useNA = "always" )) 
addmargins(table(directoffexp$debutsex_indic,directoffexp$i27a_rdt_result_f, useNA = "always" )) 
addmargins(table(directoffexp$partner3mo_bin,directoffexp$i27a_rdt_result_f, useNA = "always" )) 
addmargins(table(directoffexp$newpartner3mo_indic,directoffexp$i27a_rdt_result_f, useNA = "always" )) 


#misc
addmargins(table(directoffexp$i27a_rdt_result_f,directoffexp$hr11_religion_f, useNA = "always" )) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share

### linear model
m_glm <- glm(i27a_rdt_result ~ hr4_sex_f, data=directoffexp, family=binomial("logit"))
summary(m_glm)
exp(m_glm$coefficients)
exp(confint(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

m_glm <- glmer(i27a_rdt_result ~ (1 | hrhhid) + age_combined, data=directoffexp, family=binomial("logit"))
summary(m_glm)
fixef(m_glm)
exp(fixef(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.95))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

### fisher exact--repeat for all with low counts?
fisher.test(directoffexp$i27a_rdt_result,directoffexp$hr4_sex_f, conf.level = 0.99)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$cpshbvprox_rev)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$wealth_R_lowestv)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$sharedhhobj)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$i12_food_first_chew)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$trans_bin)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$i10_street_salon_bin)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$i11_manucure)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$i17_tattoo_bin)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$i16_traditional_scarring)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$transactionalsex)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$debutsex_indic)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$partner3mo_bin)
fisher.test(directoffexp$i27a_rdt_result,directoffexp$newpartner3mo_indic)

## Direct offspring unexposed---------
# repeat the above
# add counts
directoffunexp <- directoffunexp %>% filter(!(is.na(i27a_rdt_result)))
table(directoffunexp$i27a_rdt_result,useNA = "always" )
table(directoffunexp$i27a_rdt_result,directoffunexp$age_combined, useNA = "always" )
table(directoffunexp$hr4_sex_f,directoffunexp$i27a_rdt_result_f, useNA = "always" )
table(directoffunexp$cpshbvprox_rev,directoffunexp$i27a_rdt_result_f, useNA = "always" )
table(directoffunexp$wealth_R_lowestv, directoffunexp$i27a_rdt_result_f,useNA = "always" ) #  #lowest (=0) vs upper 3 (=1)
table(directoffunexp$sharedhhobj, directoffunexp$i27a_rdt_result_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(directoffunexp$i12_food_first_chew_f,directoffunexp$i27a_rdt_result_f, useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(directoffunexp$trans_bin, directoffunexp$i27a_rdt_result_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(directoffunexp$i10_street_salon_bin, directoffunexp$i27a_rdt_result_f,useNA = "always" ) # 1 street salon use
table(directoffunexp$i11_manucure_f, directoffunexp$i27a_rdt_result_f,useNA = "always" )
table(directoffunexp$i17_tattoo_bin,directoffunexp$i27a_rdt_result_f, useNA = "always" ) 
table(directoffunexp$i16_traditional_scarring_f,directoffunexp$i27a_rdt_result_f, useNA = "always" )
table(directoffunexp$transactionalsex,directoffunexp$i27a_rdt_result_f, useNA = "always" )
table(directoffunexp$debutsex_indic,directoffunexp$i27a_rdt_result_f, useNA = "always" )
table(directoffunexp$partner3mo_bin,directoffunexp$i27a_rdt_result_f, useNA = "always" )
table(directoffunexp$newpartner3mo_indic,directoffunexp$i27a_rdt_result_f, useNA = "always" )

m_glm <- glmer(i27a_rdt_result ~ (1 | hrhhid) + age_combined, data=directoffunexp, family=binomial("logit"))
summary(m_glm)
fixef(m_glm)
exp(fixef(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

#Fisher
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$hr4_sex_f, conf.level = 0.99)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$cpshbvprox_rev)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$wealth_R_lowestv)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$sharedhhobj)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$i12_food_first_chew)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$trans_bin)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$i10_street_salon_bin)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$i11_manucure)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$i17_tattoo_bin)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$i16_traditional_scarring)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$transactionalsex)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$debutsex_indic)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$partner3mo_bin)
fisher.test(directoffunexp$i27a_rdt_result,directoffunexp$newpartner3mo_indic)

## Other hh mem exposed---------
othermember

table(othermember$h10_hbv_rdt, othermember$i27a_rdt_result_f)
othmemexp <-othermember %>% filter(h10_hbv_rdt == 1)
nrow(othmemexp)

table(othmemexp$i27a_rdt_result,useNA = "always" )
table(othmemexp$i27a_rdt_result,othmemexp$age_combined, useNA = "always" )
table(othmemexp$hr4_sex_f,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$maritalrisk_f,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$cpshbvprox_rev,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$wealth_R_lowestv, othmemexp$i27a_rdt_result_f,useNA = "always" ) #  #lowest (=0) vs upper 3 (=1)
table(othmemexp$sharedhhobj, othmemexp$i27a_rdt_result_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(othmemexp$i12_food_first_chew_f,othmemexp$i27a_rdt_result_f, useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(othmemexp$trans_bin, othmemexp$i27a_rdt_result_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(othmemexp$i10_street_salon_bin, othmemexp$i27a_rdt_result_f,useNA = "always" ) # 1 street salon use
table(othmemexp$i11_manucure_f, othmemexp$i27a_rdt_result_f,useNA = "always" )
table(othmemexp$i17_tattoo_bin,othmemexp$i27a_rdt_result_f, useNA = "always" ) 
table(othmemexp$i16_traditional_scarring_f,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$transactionalsex,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$debutsex_indic,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$partner3mo_bin,othmemexp$i27a_rdt_result_f, useNA = "always" )
table(othmemexp$newpartner3mo_indic,othmemexp$i27a_rdt_result_f, useNA = "always" )

m_glm <- glmer(i27a_rdt_result ~ (1 | hrhhid) + age_combined, data=othmemexp, family=binomial("logit"))
fixef(m_glm)
exp(fixef(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

# Fisher exact
fisher.test(othmemexp$i27a_rdt_result,othmemexp$age_combined, conf.level = 0.99)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$hr4_sex_f, conf.level = 0.99)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$maritalrisk_f) 
fisher.test(othmemexp$i27a_rdt_result,othmemexp$cpshbvprox_rev) 
fisher.test(othmemexp$i27a_rdt_result,othmemexp$wealth_R_lowestv)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$sharedhhobj)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$i12_food_first_chew)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$trans_bin)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$i10_street_salon_bin)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$i11_manucure)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$i17_tattoo_bin)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$i16_traditional_scarring)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$transactionalsex)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$debutsex_indic)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$partner3mo_bin)
fisher.test(othmemexp$i27a_rdt_result,othmemexp$newpartner3mo_indic)



## Other hh mem unexposed---------
othmemunexp <-othermember %>% filter(h10_hbv_rdt == 0)
nrow(othmemunexp)

table(othmemunexp$i27a_rdt_result,useNA = "always" )
table(othmemunexp$i27a_rdt_result,othmemunexp$age_combined, useNA = "always" )
table(othmemunexp$hr4_sex_f,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$maritalrisk_f,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$cpshbvprox_rev,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$wealth_R_lowestv, othmemunexp$i27a_rdt_result_f,useNA = "always" ) #  #lowest (=0) vs upper 3 (=1)
table(othmemunexp$sharedhhobj, othmemunexp$i27a_rdt_result_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(othmemunexp$i12_food_first_chew_f,othmemunexp$i27a_rdt_result_f, useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(othmemunexp$trans_bin, othmemunexp$i27a_rdt_result_f,useNA = "always" ) # 1= shares razors, toothbrushes, or nail clippers vs 0=doesn't share
table(othmemunexp$i10_street_salon_bin, othmemunexp$i27a_rdt_result_f,useNA = "always" ) # 1 street salon use
table(othmemunexp$i11_manucure_f, othmemunexp$i27a_rdt_result_f,useNA = "always" )
table(othmemunexp$i17_tattoo_bin,othmemunexp$i27a_rdt_result_f, useNA = "always" ) 
table(othmemunexp$i16_traditional_scarring_f,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$transactionalsex,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$debutsex_indic,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$partner3mo_bin,othmemunexp$i27a_rdt_result_f, useNA = "always" )
table(othmemunexp$newpartner3mo_indic,othmemunexp$i27a_rdt_result_f, useNA = "always" )

m_glm <- glmer(i27a_rdt_result ~ (1 | hrhhid) + age_combined, data=othmemunexp, family=binomial("logit"))
summary(m_glm)
exp(m_glm$coefficients)
exp(confint(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$age_combined, conf.level = 0.99)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$hr4_sex_f, conf.level = 0.99)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$maritalrisk_f) 
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$cpshbvprox_rev) 
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$wealth_R_lowestv)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$sharedhhobj)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$i12_food_first_chew)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$trans_bin)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$i10_street_salon_bin)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$i11_manucure)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$i17_tattoo_bin)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$i16_traditional_scarring)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$transactionalsex)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$debutsex_indic)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$partner3mo_bin)
fisher.test(othmemunexp$i27a_rdt_result,othmemunexp$newpartner3mo_indic)

# visualize only those with cell counts over 5 - almost all vars for index moms and 3 vars for exp DO

# drop from moms: i17_tattoo_bin1, i17_tattoo_bin1
glm_mi <- glm_mi %>% relocate(group, level)
glm_mi_cell5 <- glm_mi %>% filter(term != "i17_tattoo_bin1" & term != "partner3mo_bin1" & term != "i16_traditional_scarring_fRefused")
view(glm_mi_cell5)


view(glmer_doexp_rd)
glmer_doexp_cell5 <- glmer_doexp_rd %>% filter(term == "age_combined" | term == "cpshbvprox_revProbably not vaccinated" | term == "i10_street_salon_fYes")
view(glmer_doexp_cell5)
glmer_doexp_cell5 <- glmer_doexp_cell5 %>% rename(logodds = estimate)

fig3_cell5 <- rbind(glm_mi_cell5,glmer_doexp_cell5)
#fig3_cell5 <- bind_rows(glm_mi_cell5,glmer_doexp_cell5)
view(fig3_cell5)
# rename estimate to logodds
# vars with cell counts over 5: term = age_combined, cpshbvprox_revProbably not vaccinated, i10_street_salon_fYes


# rename terms
fig3_cell5$group[fig3_cell5$group == "Direct offspring"] <- "Exposed direct offspring"

fig3_cell5$term[fig3_cell5$term == "age_combined"] <- "Age (1-year increase)"
fig3_cell5$term[fig3_cell5$term == "cpshbvprox_revProbably not vaccinated"] <- "HBV vaccination: Unvaccinated vs vaccinated"
fig3_cell5$term[fig3_cell5$term == "maritalrisk_fNot married"] <- "Marriage: Never vs married"
fig3_cell5$term[fig3_cell5$term == "maritalrisk_fDivorced/Widowed"] <- "Marriage: Divorced/widowed vs married"
fig3_cell5$term[fig3_cell5$term == "wealth_R_lowestv"] <- " Upper wealth quartiles (Q4-Q2) vs lowest (Q1)"
fig3_cell5$term[fig3_cell5$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
fig3_cell5$term[fig3_cell5$term == "sharedhhobj1"] <- "Shares razors/nail clippers/toothbrushes in household"
fig3_cell5$term[fig3_cell5$term == "debutsex_indic1"] <- "Sexual debut <18 yrs or refused vs ≥18 yrs"
fig3_cell5$term[fig3_cell5$term == "i10_street_salon_fYes"] <- "Uses street salons vs not" #i10_street_salon_bin
fig3_cell5$term[fig3_cell5$term == "i10_street_salon_bin1"] <- "Uses street salons vs not" #i10_street_salon_bin
fig3_cell5$term[fig3_cell5$term == "i11_manucure_fYes"] <- "Manicures outside home vs not"
fig3_cell5$term[fig3_cell5$term == "i16_traditional_scarring_fYes"] <- "Traditional scarring vs none"
fig3_cell5$term[fig3_cell5$term == "trans_bin1"] <- "1+ past transfusion vs none"
fig3_cell5$term[fig3_cell5$term == "transactionalsex1"] <- "Has engaged in transactional sex or refused to answer vs no"
fig3_cell5$term[fig3_cell5$term == "newpartner3mo_indic1"] <- "≥1 New sexual partner in last 3 months or refuse vs none new"

fig3_cell5 <- fig3_cell5 %>% arrange(desc(level))

fig3_cell5$term <- as.factor(fig3_cell5$term)

#plot - with 95%
fig3_cell5 %>% filter(group=="Index mothers") %>% 
  mutate(term = (fct_reorder(fct_rev(term), (level)))) %>%
  ggplot(aes(x=(fct_rev(term)), y=logodds)) + #group = interaction(level); alpha = level
  geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=term, y=logodds, ymin=LCI_95, ymax=UCI_95), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=logodds, group=group, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#A6DEAE","#020E54","deeppink3","#FF700A"))+ #deeppink3
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_discrete(breaks=test$id,labels=labels) + #, trans = "reverse"
  #scale_x_discrete(limits = rev(levels(fig3_cell5$term)))+
  coord_flip() + #theme_bw() + 
  #ylim(0, 30)+
  labs(x="", y="Log(OR) of HBsAg+") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20)) +
  #ggtitle("Log(OR) of HBsAg by individual-level") +
  #facet_grid(~group)
  facet_wrap(~ factor(group, levels = c("Index mothers", "Exposed direct offspring")),nrow = 1) #"Male partners", , "Other household members"

ggsave('./plots/fig4_95.png', width=25, height=8)

#plot - with 99%
fig3_cell5 %>% #filter(level=="Individual") %>% 
  mutate(term = (fct_reorder(fct_rev(term), (level)))) %>%
  ggplot(aes(x=(fct_rev(term)), y=logodds)) + #group = interaction(level); alpha = level
  geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=term, y=logodds, ymin=LCI_99, ymax=UCI_99), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=logodds, group=group, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#A6DEAE","#020E54","deeppink3","#FF700A"))+ #deeppink3
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_discrete(breaks=test$id,labels=labels) + #, trans = "reverse"
  #scale_x_discrete(limits = rev(levels(fig3_cell5$term)))+
  coord_flip() + #theme_bw() + 
  labs(x="", y="Log(OR) of HBsAg+") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20)) +
  #ggtitle("Log(OR) of HBsAg by individual-level") +
  #facet_grid(~group)
  facet_wrap(~ factor(group, levels = c("Index mothers", "Exposed direct offspring" )),nrow = 1) # "Other household members", "Male partners", 

ggsave('./plots/fig4_99.png', width=25, height=8)

view(fig3_cell5)
# retreat poster------
#plot - with 95%
fig3_cell5 %>% filter(group=="Exposed direct offspring") %>% # filter(group=="Index mothers") %>% 
  mutate(term = (fct_reorder(fct_rev(term), (level)))) %>%
  ggplot(aes(x=(fct_rev(term)), y=logodds)) + #group = interaction(level); alpha = level
  geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=term, y=logodds, ymin=LCI_95, ymax=UCI_95), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=logodds, group=group, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#A6DEAE","#020E54","deeppink3","#FF700A"))+ #deeppink3
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_discrete(breaks=test$id,labels=labels) + #, trans = "reverse"
  #scale_x_discrete(limits = rev(levels(fig3_cell5$term)))+
  coord_flip() + #theme_bw() + 
  ylim(-2, 5.02)+
  labs(x="", y="Log(OR) of HBsAg+") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 40),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 40)) +
  #ggtitle("Log(OR) of HBsAg by individual-level") +
  #facet_grid(~group)
  facet_wrap(~ factor(group, levels = c("Index mothers", "Exposed direct offspring")),nrow = 1) #"Male partners", , "Other household members"

ggsave('./plots/fig4_miret.png', width=25, height=10)
ggsave('./plots/fig4_edo.png', width=20, height=4)
