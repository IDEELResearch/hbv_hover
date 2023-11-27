library(lme4)
library(lmtest)
library(geepack)
library(tidyverse)
library(broom)
library(broom.mixed)
library(cowplot)
library(patchwork)

# Factors assoc with HbsAg+ anlaysis -------------------
# Figure 3, supp figs XX
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


#Sept 2023 update-----------------------------------
##moms

# updated model
allmivar <- c("age_combined","maritalrisk_f","wealth_R_lowestv","i13_shared_toothbrush_f","i14_shared_razor_f", "i15_shared_nailclippers_f",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_cat", "partner3mo_bin","newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic") #"sharedhhobj removed
# for subgroups
risk_ind_mi <-  c("age_combined","maritalrisk_f") # "malepartpos", <- figure out to have single group of ind vars 
# household
risk_hh_all <-  c("i13_shared_toothbrush_f","i14_shared_razor_f", "i15_shared_nailclippers_f",'i12_food_first_chew_f',"wealth_R_lowestv") #  "sharedhhobj" removed
# community
risk_comm_all <-  c("trans_bin","i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_cat", "partner3mo_bin","newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic")

# mothers need to have OR reported (case-control design); check prev ratio for other subgroups
# recruitment test
itt_model_moms <- function(var){ # glm function
  m <- glm(as.formula(paste0('h10_hbv_rdt ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}
# enrollment test
perprot_model_moms <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}
#pos at any point
anypos_model_moms <- function(var){ # glm function
  m <- glm(as.formula(paste0('anypos ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}
# pos at both points
onlypos_model_moms <- function(var){ # glm function
  m <- glm(as.formula(paste0('onlypos ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

# run 4 models
glm_mi_recr <- map_dfr(allmivar,itt_model_moms) 
glm_mi_pp <- map_dfr(allmivar,perprot_model_moms) 
glm_mi_anypos <- map_dfr(allmivar,anypos_model_moms) 
glm_mi_onlypos <- map_dfr(allmivar,onlypos_model_moms) 

glm_mi_recr$time <- "Recruitment"
glm_mi_pp$time <- "Enrollment"
glm_mi_anypos$time <- "Either positive"
glm_mi_onlypos$time <- "Always positive"

# for reporting in text
glm_mi_recr_res <- glm_mi_recr %>% 
  mutate(est_exp = exp(estimate), lowerci_exp = exp(`2.5 %`), upperci_exp = exp(`97.5 %`), clr = upperci_exp/lowerci_exp )
view(glm_mi_recr_res)

glm_mi_4 <- rbind(glm_mi_recr, glm_mi_pp, glm_mi_anypos,glm_mi_onlypos)
view(glm_mi_recr)

glm_mi_4$or <- exp(glm_mi_4$estimate)
glm_mi_4$lowerci <- exp(glm_mi_4$`2.5 %`)
glm_mi_4$upperci <- exp(glm_mi_4$`97.5 %`)

glm_mi_4_rd <- glm_mi_4 %>% mutate_if(is.numeric, ~round(., 2))
colnames(glm_mi_4_rd) <- c('term','logodds','std.error','statistic','p.value','LCI_95','UCI_95','time','or','lowerci','upperci')
rownames(glm_mi_4_rd) <- 1:nrow(glm_mi_4_rd)
glm_mi_4_rd$group <- "Index mothers"

glm_mi_4_rd <- glm_mi_4_rd %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_mi, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))
glm_mi_4_rd <- glm_mi_4_rd %>% relocate(group, level, time)
view(glm_mi_4_rd)

### forest plot with annotations----
# rename variables for plot
glm_mi_4_rd$term[glm_mi_4_rd$term == "wealth_R_lowestv"] <- "Upper wealth quartiles (Q4-Q2) vs lowest (Q1)"
#glm_mi_4_rd$term[glm_mi_4_rd$term == "wealth_R2"] <- "Higher wealth (Q3) vs lowest (Q1)"
#glm_mi_4_rd$term[glm_mi_4_rd$term == "wealth_R1"] <- "Lower wealth (Q2) vs lowest (Q1)"
glm_mi_4_rd$term[glm_mi_4_rd$term == "maritalrisk_fNot married"] <- "Marriage: Never vs married"
glm_mi_4_rd$term[glm_mi_4_rd$term == "maritalrisk_fDivorced/Widowed"] <- "Marriage: Divorced/widowed vs married"
# glm_mi_4_rd$term[glm_mi_4_rd$term == "malepartpos"] <- "Male partner HBsAg pos vs neg"
glm_mi_4_rd$term[glm_mi_4_rd$term == "age_combined"] <- "Age (1-year increase)"
#glm_mi_4_rd$term[glm_mi_4_rd$term == "sharedhhobj1"] <- "Shares razors/nail clippers/toothbrushes in household"
glm_mi_4_rd$term[glm_mi_4_rd$term == "i13_shared_toothbrush_fYes"] <- "Shares toothbrushes in household vs not"
glm_mi_4_rd$term[glm_mi_4_rd$term == "i14_shared_razor_fYes/Refused"] <- "Shares razors in household vs not"
glm_mi_4_rd$term[glm_mi_4_rd$term == "i15_shared_nailclippers_fYes"] <- "Shares nail clippers in household vs not"
glm_mi_4_rd$term[glm_mi_4_rd$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
#transfusions changed to binary: 1+ or refused vs none/missing
#glm_mi_4_rd$term[glm_mi_4_rd$term == "i8_transfusion_fYes"] <- "Past transfusion vs never"
#glm_mi_4_rd$term[glm_mi_4_rd$term == "transfus_num9"] <- "≥4 transfusions vs none"
#glm_mi_4_rd$term[glm_mi_4_rd$term == "transfus_num3"] <- "3 transfusions vs none"
glm_mi_4_rd$term[glm_mi_4_rd$term == "i10_street_salon_fYes"] <- "Uses street salons vs not" #i10_street_salon_bin
glm_mi_4_rd$term[glm_mi_4_rd$term == "i10_street_salon_bin1"] <- "Uses street salons vs not" #i10_street_salon_bin
glm_mi_4_rd$term[glm_mi_4_rd$term == "i11_manucure_fYes"] <- "Manicures outside home vs not"

glm_mi_4_rd$term[glm_mi_4_rd$term == "i17_tattoo_bin1"] <- "Tattoos: Yes vs none" # i17_tattoo_bin change
#glm_mi_4_rd$term[glm_mi_4_rd$term == "i17_tattoo_fRefused"] <- "Tattoos: refused vs none" # i17_tattoo_bin change
glm_mi_4_rd$term[glm_mi_4_rd$term == "i16_traditional_scarring_fYes/Refused"] <- "Traditional scarring: Yes/Refused vs none"
glm_mi_4_rd$term[glm_mi_4_rd$term == "trans_bin1"] <- "1+ past transfusion vs none"
glm_mi_4_rd$term[glm_mi_4_rd$term == "transactionalsex1"] <- "Has engaged in transactional sex or refused to answer vs no"
glm_mi_4_rd$term[glm_mi_4_rd$term == "debutsex_cat<18"] <- "Age of sexual debut: <18 yrs vs ≥18 yrs"
glm_mi_4_rd$term[glm_mi_4_rd$term == "debutsex_catRefused/don't know"] <- "Age of sexual debut: Refused/don't know vs ≥18 yrs"
glm_mi_4_rd$term[glm_mi_4_rd$term == "partner3mo_bin1"] <- "Sexual partners in last 3 months: ≥2 or refused vs ≤1"
glm_mi_4_rd$term[glm_mi_4_rd$term == "partner12mo_bin1"] <- "Sexual partners in last 12 months: ≥2 or refused vs ≤1"
glm_mi_4_rd$term[glm_mi_4_rd$term == "newpartner3mo_indic1"] <- "New sexual partners in last 3 months: ≥1 or refuse vs none new"
glm_mi_4_rd$term[glm_mi_4_rd$term == "newpartner12mo_indic1"] <- "New sexual partners in last 12 months: ≥1 or refuse vs none new"

# rename nov 2023
glm_mi_4_rd$term[glm_mi_4_rd$term == "Uses street salons vs not"] <- "Uses street salons" #i10_street_salon_bin
glm_mi_4_rd$term[glm_mi_4_rd$term == "Uses street salons vs not"] <- "Uses street salons" #i10_street_salon_bin
glm_mi_4_rd$term[glm_mi_4_rd$term == "Shares toothbrushes in household vs not"] <- "Shares toothbrushes in household"
glm_mi_4_rd$term[glm_mi_4_rd$term == "Shares razors in household vs not"] <- "Shares razors in household"
glm_mi_4_rd$term[glm_mi_4_rd$term == "Shares nail clippers in household vs not"] <- "Shares nail clippers in household"
glm_mi_4_rd$term[glm_mi_4_rd$term == "Tattoos: Yes vs none"] <- "Tattoos" # i17_tattoo_bin change
glm_mi_4_rd$term[glm_mi_4_rd$term == "Traditional scarring: Yes/Refused vs none"] <- "Traditional scarring"
glm_mi_4_rd$term[glm_mi_4_rd$term == "Manicures outside home vs not"] <- "Manicures/pedicures outside home"
glm_mi_4_rd$term[glm_mi_4_rd$term == "1+ past transfusion vs none"] <- "≥1 past transfusion vs none"


glm_mi_4_rd <- glm_mi_4_rd %>% mutate(relsize = case_when(
  time == "Recruitment" ~ 1.5,
  TRUE ~ 1))
table(glm_mi_4_rd$relsize)

glm_mi_4_rd <- glm_mi_4_rd %>% group_by(time) %>%  mutate(desorder=1:(nrow(glm_mi_4_rd)/4))

write.csv(glm_mi_4, file = "glm_mi_4.csv" )
view(glm_mi_4_rd)
write.csv(glm_mi_4_rd, file = "glm_mi_4_rd.csv" )

glm_mi_4_rd %>% 
  ## these mutate steps were to change order - redone with desorder step directly before
  #mutate(term = (fct_reorder(fct_rev(term), (glm_mi_4_rd$level)))) %>%
  #mutate(desiredorder = fct_relevel(term, levels = c()))
  #ggplot(aes(x=(fct_rev(term)), y=logodds, color = time)) + #group = interaction(level); alpha = level
  ggplot() +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=or, ymin=lowerci, ymax=upperci, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  labs(y = "Odds ratio", x = "Effect") +
  # geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  # geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=logodds, ymin=LCI_95, ymax=UCI_95, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  #geom_point( aes(x=term, y=logodds, group=group, color=time),shape=15, size=7, position=position_dodge2(width = 1.0) ,alpha=0.9) + # this was place on incorrect line if multiple groups
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#9DC6E9","#5B85C4", "#28499E","#16246D" ))+  #deeppink3, "#E7B800" "#16246D", "#28499E", "#4676C5",  "#020E54","#C0D9F5"
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20), minor_breaks = NULL) +
  coord_flip() + 
  # labs(x="", y="Log(OR) of HBsAg+") + # old version
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20))+
  guides(size = "none",color = guide_legend(override.aes = list(size = 1), reverse = T, title = "Test timepoint"))

ggsave('./plots/fig_mi_4grp_95.png', width=25, height=12)

# plot version to add logodds to (no axis labels, etc.)
plot <- 
  glm_mi_4_rd %>% 
  #mutate(term = (fct_reorder(fct_rev(term), (glm_mi_4_rd$level)))) %>%
  #mutate(desiredorder = fct_relevel(term, levels = c()))
  #ggplot(aes(x=(fct_rev(term)), y=logodds, color = time)) + #group = interaction(level); alpha = level
  ggplot() +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=or, ymin=lowerci, ymax=upperci, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  #geom_point( aes(x=term, y=logodds, group=group, color=time),shape=15, size=7, position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#9DC6E9","#5B85C4", "#28499E","#16246D" ))+  #deeppink3, "#E7B800" "#16246D", "#28499E", "#4676C5",  "#020E54","#C0D9F5"
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20), minor_breaks = NULL) +
  coord_flip()+ 
  labs(y = "Odds ratio", x = "") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),# for labeled forest plot 
    axis.ticks.y=element_blank(), # for labeled forest plot 
    ###
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    legend.text=element_text(size=20),
    legend.title = element_text(size=20),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    panel.grid.minor=element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 20))+
  guides(size = "none",color = guide_legend(override.aes = list(size = 1), reverse = T, title = "Test timepoint"))

# add point est and CIs to figure - use cowplot to combine two parts
res_plot <- glm_mi_4_rd |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(c("or", "lowerci", "upperci"), ~ str_pad(round(.x, 2), width = 4, pad = "0", side = "right")),
         # add an "-" between logodds estimate confidence intervals
         estimate_lab = paste0(or, " (", lowerci, ", ", upperci, ")")) |>
  filter(time == "Recruitment") |>
  select(term, level, time, estimate_lab,desorder) 
view(res_plot)
# add a row of data that are actually column names which will be shown on the plot in the next step
#add extra decimal for those that are in the thousandths place
res_plot$estimate_lab <- ifelse(res_plot$estimate_lab=="1000 (0.39, 2.55)", "1.00 (0.39, 2.55)",res_plot$estimate_lab)
res_plot$estimate_lab <- ifelse(res_plot$estimate_lab=="1000 (0.35, 2.83)", "1.00 (0.35, 2.83)",res_plot$estimate_lab)
res_plot$estimate_lab <- ifelse(res_plot$estimate_lab=="1000 (0.53, 1.89)", "1.00 (0.53, 1.89)",res_plot$estimate_lab)

estimate_lab <- "OR (95% CI)*"
time <- ""
term <- ""
level <- ""
desorder <- 0
titles <- data.frame(estimate_lab, time, term, level,desorder)
view(titles)

res_plot <-rbind(res_plot, titles)
view(res_plot)

p_left <-
  res_plot  |>
  #mutate(term = fct_rev(fct_reorder(term, desorder))) %>%
  ggplot(aes(y = fct_rev(fct_reorder(term, desorder))))+
  geom_text(aes(x = 0, label = term), hjust = 0, size = 5, fontface = "bold")+
  geom_text(aes(x = 1, label = estimate_lab), hjust = 0 , size = 5,
            fontface = ifelse(res_plot$estimate_lab == "OR (95% CI)*", "bold", "plain")
  )+
  theme_void() +
  #xlim(0,2)+
  coord_cartesian(xlim = c(0, 1.5))

layout <- c(
  area(t = 0, l = 0, b = 30, r = 5), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 3, l = 6, b = 30, r = 12) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
)
p_left + plot + plot_layout(design=layout)
ggsave('./plots/fig_mi_4grp_95_numb.png', width=25, height=12)

# Main text figure with number labels
# plot version to add logodds to (no axis labels, etc.)
view(glm_mi_4_rd)
plot_r <- 
  glm_mi_4_rd %>% filter(time=="Recruitment") %>% 
  ggplot() +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=or, ymin=lowerci, ymax=upperci, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  #geom_point( aes(x=term, y=logodds, group=group, color=time),shape=15, size=7, position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#16246D" ))+  #deeppink3, "#E7B800" "#16246D", "#28499E", "#4676C5",  "#020E54","#C0D9F5"
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20), minor_breaks = NULL) +
  coord_flip()+ 
  labs(x="", y="Odds ratio") + 
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),# for labeled forest plot 
    axis.ticks.y=element_blank(), # for labeled forest plot 
    ###
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    legend.position = "none",
    panel.grid.minor=element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 20))
p_left + plot_r + plot_layout(design=layout)
ggsave('./plots/fig_mi_rec_95_numb.png', width=25, height=12)


## now do this for exposed direct offspring (probably not needed for other subgroups bc of low counts)

# random explore for moms------------------------------------------------------------
agemom <- glm(h10_hbv_rdt ~ debutsex_cat, data=moms, family=binomial("logit"))
summary(agemom)
confint(agemom, level = 0.95)
confint(agemom, level = 0.99)
# diff between current age and age of sexual debut
table(moms$debutsex_cat, moms$h10_hbv_rdt_f, useNA = "always")
table(moms$debutsex_cat, moms$maternity, useNA = "always")
table(moms$i25_sex_hx_receive_money, moms$maternity, useNA = "always")

## Mantel-Haenszel OR for mothers explore-----------------------------------------------------------------------
install.packages("epiDisplay")
library(epiDisplay)

mht1 <- with(moms, table(moms$h10_hbv_rdt_f, moms$debutsex_cat))
dim(mht1)
mhor(mhtable=mht1, design = "case-control") 

mantelhaen.test(mht1)
install.packages("samplesizeCMH")
library(samplesizeCMH)

partial_tables <- margin.table(moms, c(2,4,1))
marginal_table <- margin.table(moms, c("h10_hbv_rdt","wealth_R_lowestv"))

odds.ratio(marginal_table)
apply(partial_tables, 3, odds.ratio)
mantelhaen.test(partial_tables)

#Exp Direct offspring---------
table(directoff$age_cat, directoff$h10_hbv_rdt_f, useNA = "always") 
# collapse version of age category var that corresponds with vaccination 
directoff <- directoff %>% mutate(age_cat_cons = case_when(
  age_cat =="2" ~ 1, # if person was likely not vaccinated or possibly not, group together
  age_cat =="1" ~ 1, # if person was likely not vaccinated or possibly not, group together
  age_cat == "0" ~ 0)
  %>% as.factor(),
  age_cat_highrisk = case_when(
    age_cat == "2" ~ 1,
    age_cat == "1" ~ 0,
    age_cat == "0" ~ 0) %>% as.factor()) 
class(directoff$age_cat_cons)
class(directoff$age_cat_highrisk)
table(directoff$age_cat_highrisk, directoff$age_cat)
# make separate exposed/unexposed datasets
# primary def of exposed, plus 3 sensitivity analyses
# remove the 3 offspring on which test was not done
doexp_recr <-directoff %>% filter(h10_hbv_rdt == 1 & !is.na(i27a_rdt_result))
doexp_enr <-directoff %>% filter(perprot_h10 == 1 & !is.na(i27a_rdt_result)) 
doexp_any <-directoff %>% filter(anypos == 1 & !is.na(i27a_rdt_result))
doexp_only <-directoff %>% filter(onlypos == 1 & !is.na(i27a_rdt_result))

# all DO vars
alldovar <- c("age_combined","hr4_sex_f","age_cat", "wealth_R_lowestv","i13_shared_toothbrush_f","i14_shared_razor_f", "i15_shared_nailclippers_f",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_cat", "partner3mo_bin","newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic")
# individual
risk_ind_do <-  c("age_combined","hr4_sex_f","age_cat_cons") #"maritalrisk_f","malepartpos",
# household
risk_hh_all <-  c("i13_shared_toothbrush_f","i14_shared_razor_f", "i15_shared_nailclippers_f",'i12_food_first_chew_f',"wealth_R_lowestv") #  "sharedhhobj" removed
# community
risk_comm_all <-  c("trans_bin","i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_cat", "partner3mo_bin","newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic")

library(crosstable)
library(flextable)
o <- crosstable(doexp_recr, alldovar, by=i27a_rdt_result_f, total="both") %>%
  as_flextable(keep_id=TRUE)
# figure out how to export
save_as_docx(
  "exp DO freq tab" = o, 
  path = "./plots/expdofreq.docx")

# Questions for Jess/methods people - better to present prevalence ratios not accounting for clustering (glm()) or odds ratios from glmer()?

# vars that don't converge with glmer log model
m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_cat_cons, data=doexp_recr, family=binomial("log"), nAGQ = 0) # doesn't converge
m <- glm(i27a_rdt_result ~ age_cat_cons, data=doexp_recr, family=binomial("log")) # PR model not accounting for clustering
m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_cat_cons, data=doexp_recr, family=binomial("logit"), nAGQ = 0) # OR model accounting for hh clustering
summary(m)
confint(m, method = c("Wald"))

doexp_recr_nomis <- doexp_recr %>% filter(!is.na(debutsex_indic))

# ran below for the following variables: debutsex_indic (not debutsex_cat), partner3mo_bin, newpartner3mo_indic, partner12mo_bin
m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + newpartner12mo_indic, data=doexp_recr_nomis, family=binomial("log"), nAGQ = 0) # doesn't converge
summary(m)
confint(m, method = c("Wald"))
# logit glmer
m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + newpartner12mo_indic, data=doexp_recr_nomis, family=binomial("logit"), nAGQ = 0) # doesn't converge
summary(m)
confint(m, method = c("Wald"))
# log glm
m <- glm(i27a_rdt_result ~ newpartner12mo_indic, data=doexp_recr_nomis, family=binomial("log")) # PR model not accounting for clustering
summary(m)
confint(m, method = c("Wald"))

# response from Jess was try modified Poisson or COPY method (copies dataset and switches outcome in one observation in each iteration)
library(clubSandwich)
library(sandwich)
library("lmtest")
install.packages("robustlmm", dependencies = TRUE)
library(robustlmm)

# robust SE/cov deep dive
# for robust SE : The basic so-called "robust" standard errors are available in function sandwich(). To apply these to the usual marginal Wald tests you can use the coeftest() function from the lmtest package:
# source: https://stats.stackexchange.com/questions/520662/how-to-add-robust-error-variances-in-glm-poisson-model-in-r

m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_cat_cons, data=doexp_recr, family=poisson("log"))
# m <- glm(i27a_rdt_result ~   age_cat_cons, data=doexp_recr, family=poisson("log"))
summary(m)

coeftest(m, vcov = sandwich)
vcov(m)

# https://cran.r-project.org/web/packages/clubSandwich/clubSandwich.pdf
V_CR2 <- vcovCR(m, cluster = doexp_recr$hrhhid, type = "CR2") # this function does not like what i am putting for clusters
clubSandwich::conf_int(m,  vcov = rse)
# rlmer() not solving either
rse <- bread(m, full = TRUE, ranpar = "var") # how to integrate
# try confint.merMod
confint.merMod(m, method = "Wald")
# and compare 
confint(m, method = "Wald")
# seems very close in range, report output from confint.merMod()
# these estimates from poisson also are similar range of glm log and glmer logit model so seem appropriate

# now for age, age_cat_cons
m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_cat_cons, data=doexp_any, family=poisson("log"))
summary(m)
confint(m, method = "Wald")

m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + age_cat_cons, data=doexp_any, family=binomial("logit"))
summary(m)
confint(m, method = "Wald")



# vars that converge
alldovar <- c("hr4_sex_f", "wealth_R_lowestv","i14_shared_razor_f", "i15_shared_nailclippers_f",'trans_bin', "i10_street_salon_bin", "i11_manucure_f",  "partner3mo_bin")
# don't report bc too low counts "i13_shared_toothbrush_f",'i12_food_first_chew_f',"i17_tattoo_bin","i16_traditional_scarring_f","transactionalsex", 

# consider glmer() logit or glm log: "age_combined","age_cat", use subset of those who answer: "debutsex_cat","newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic"

recr_doexp <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_recr, family=binomial("log"), nAGQ = 0)
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
# modified poisson for age_cat_cons, age_combined - family=poisson("log"))
# logit for OR to PR approx
recr_doexp_logit <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_recr, family=binomial("logit"))
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

enr_doexp <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_enr, family=binomial("log"), nAGQ = 0)
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
# logit approximation for age_cat_cons, age_combined
enr_doexp_logit <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_enr, family=binomial("logit"))
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

any_doexp <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_any, family=binomial("log"), nAGQ = 0)
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
# logit approx for age_cat_cons, age_combined
any_doexp_logit <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_any, family=binomial("logit"))
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

only_doexp <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_only, family=binomial("log"), nAGQ = 0)
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
# logit approx for age_cat_cons, age_combined
only_doexp_logit <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=doexp_only, family=binomial("logit"))
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

# check convergence for each variable, for all 4 models
alldovar <- c("hr4_sex_f", "wealth_R_lowestv", "i14_shared_razor_f", "i15_shared_nailclippers_f", 'trans_bin', "i10_street_salon_bin", "i11_manucure_f",  "partner3mo_bin")
# datasets: doexp_recr, doexp_enr, doexp_any, doexp_only

m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + i11_manucure_f, data=doexp_only, family=binomial("log"), nAGQ = 0) # 
summary(m)
m <- glmer(i27a_rdt_result ~  (1 | hrhhid) + i11_manucure_f, data=doexp_only, family=binomial("log")) # 
summary(m)

# conclusions:  c("hr4_sex_f", "wealth_R_lowestv", "i14_shared_razor_f", "i15_shared_nailclippers_f", 'trans_bin') converge for all 4 
# "i10_street_salon_bin", "i11_manucure_f",  "partner3mo_bin" do not converge with a subset of models - run individually and append

do_conv_4 <- c("hr4_sex_f", "wealth_R_lowestv", "i14_shared_razor_f", "i15_shared_nailclippers_f", 'trans_bin')
do_sub_recr <- c("i10_street_salon_bin", "i11_manucure_f","partner3mo_bin")
do_sub_onlypos <- c("i10_street_salon_bin", "i11_manucure_f") # run together for recruitment df, then run individually : i10_street_salon_bin, i11_manucure_f for glmer_doexp_only; partner3mo_bin for glmer_doexp_any
do_sub_anypos <-  "partner3mo_bin"  #partner3mo_bin for glmer_doexp_any
# for those that don't converge, consider showing glm PR or glmer OR
do_modpoiss <- c("age_cat_cons", "age_combined")
do_logit <- c("age_cat_cons", "age_combined")

glmer_doexp_recr <- map_dfr(do_conv_4, recr_doexp) 
glmer_doexp_recr_2 <- map_dfr(do_sub_recr, recr_doexp) 
glmer_doexp_recr_3 <- map_dfr(do_logit, recr_doexp_logit) 
glmer_doexp_enr <- map_dfr(do_conv_4, enr_doexp) 
glmer_doexp_enr_3 <- map_dfr(do_logit, enr_doexp_logit) 
glmer_doexp_any <- map_dfr(do_conv_4, any_doexp) 
glmer_doexp_any_2 <- map_dfr(do_sub_anypos, any_doexp) 
glmer_doexp_any_3 <- map_dfr(do_logit, any_doexp_logit) 
glmer_doexp_only <- map_dfr(do_conv_4, only_doexp) 
glmer_doexp_only_2 <- map_dfr(do_sub_onlypos, only_doexp) 
glmer_doexp_only_3 <- map_dfr(do_logit, only_doexp_logit) 

glmer_doexp_recr$time <- "Recruitment"
glmer_doexp_recr_2$time <- "Recruitment"
glmer_doexp_recr_3$time <- "Recruitment"
glmer_doexp_enr$time <- "Enrollment"
glmer_doexp_enr_3$time <- "Enrollment"
glmer_doexp_any$time <- "Either positive"
glmer_doexp_any_2$time <- "Either positive"
glmer_doexp_any_3$time <- "Either positive"
glmer_doexp_only$time <- "Always positive"
glmer_doexp_only_2$time <- "Always positive"
glmer_doexp_only_3$time <- "Always positive"
addmargins(table(directoff$i27a_rdt_result, directoff$h10_hbv_rdt, useNA = "always"))
# for reporting in text
glmer_doexp_allrec <- rbind(glmer_doexp_recr_3,glmer_doexp_recr, glmer_doexp_recr_2)
view(glmer_doexp_allrec)

glmer_doexp_allrec <- glmer_doexp_allrec %>% 
  mutate(est_exp = exp(estimate), lowerci_exp = exp(LCI_95), upperci_exp = exp(UCI_95), clr = upperci_exp/lowerci_exp )
view(glmer_doexp_allrec)

glmer_doexp_4 <- rbind(glmer_doexp_recr_3, glmer_doexp_recr, glmer_doexp_recr_2, glmer_doexp_enr_3, glmer_doexp_enr, glmer_doexp_any_3, glmer_doexp_any, glmer_doexp_any_2, glmer_doexp_only_3, glmer_doexp_only, glmer_doexp_only_2)
view(glmer_doexp_4)

#exponentiate for writing out
glmer_doexp_4$pr <- exp(glmer_doexp_4$estimate)
glmer_doexp_4$lowerci <- exp(glmer_doexp_4$LCI_95)
glmer_doexp_4$upperci <- exp(glmer_doexp_4$UCI_95)

glmer_doexp_4_rd <- glmer_doexp_4 %>% mutate_if(is.numeric, ~round(., 2))
glmer_doexp_4_rd <- glmer_doexp_4_rd %>% select(-c(term.1)) # drop the term.1 - should do above

colnames(glmer_doexp_4_rd) <- c('term','logpr','std.error','statistic','p.value','LCI_95','UCI_95','LCI_99','UCI_99','time','pr','lowerci','upperci')
rownames(glmer_doexp_4_rd) <- 1:nrow(glmer_doexp_4_rd)
glmer_doexp_4_rd$group <- "Exposed direct offspring"
view(glmer_doexp_4_rd)

glmer_doexp_4_rd <- glmer_doexp_4_rd %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_do, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))
glmer_doexp_4_rd <- glmer_doexp_4_rd %>% relocate(group, level, time)

### forest plot with annotations----
# rename variables for plot

glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "age_cat_cons1"] <- "Born before HBV vaccination vs born since*"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "age_combined"] <- "1-year increase in age*"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "hr4_sex_fFemale"] <- "Female vs male"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "wealth_R_lowestv"] <- "Upper wealth quartiles (Q4-Q2) vs lowest (Q1)"
#glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "wealth_R2"] <- "Higher wealth (Q3) vs lowest (Q1)"
#glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "wealth_R1"] <- "Lower wealth (Q2) vs lowest (Q1)"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "maritalrisk_fNot married"] <- "Marriage: Never vs married"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "maritalrisk_fDivorced/Widowed"] <- "Marriage: Divorced/widowed vs married"
# glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "malepartpos"] <- "Male partner HBsAg pos vs neg"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "age_combined"] <- "Age (1-year increase)"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i13_shared_toothbrush_fYes"] <- "Shares toothbrushes in household vs not"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i14_shared_razor_fYes/Refused"] <- "Shares razors in household vs not"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i15_shared_nailclippers_fYes"] <- "Shares nail clippers in household vs not"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i10_street_salon_fYes"] <- "Uses street salons vs not" #i10_street_salon_bin
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i10_street_salon_bin1"] <- "Uses street salons vs not" #i10_street_salon_bin
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i11_manucure_fYes"] <- "Manicures outside home vs not"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i17_tattoo_bin1"] <- "Tattoos: Yes vs none" # i17_tattoo_bin change
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "i16_traditional_scarring_fYes/Refused"] <- "Traditional scarring: Yes/Refused vs none"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "trans_bin1"] <- "1+ past transfusion vs none"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "transactionalsex1"] <- "Has engaged in transactional sex or refused to answer vs no"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "debutsex_cat<18"] <- "Age of sexual debut: <18 yrs vs ≥18 yrs"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "debutsex_catRefused/don't know"] <- "Age of sexual debut: Refused/don't know vs ≥18 yrs"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "partner3mo_bin1"] <- "Sexual partners in last 3 months: ≥2 or refused vs ≤1"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "partner12mo_bin1"] <- "Sexual partners in last 12 months: ≥2 or refused vs ≤1"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "newpartner3mo_indic1"] <- "New sexual partners in last 3 months: ≥1 or refuse vs none new"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "newpartner12mo_indic1"] <- "New sexual partners in last 12 months: ≥1 or refuse vs none new"

# requested changes to labels:
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Born before HBV vaccination vs born since*"] <- "Born before HBV vaccination*"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Born before HBV vaccination*"] <- "Born before HBV vaccination†"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "1-year increase in age*"] <- "1-year increase in age†"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Female vs male"] <- "Female sex"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Shares toothbrushes in household vs not"] <- "Shares toothbrushes in household"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Shares razors in household vs not"] <- "Shares razors in household"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Shares nail clippers in household vs not"] <- "Shares nail clippers in household"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "1+ past transfusion vs none"] <- "≥1 past transfusion vs none"
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Uses street salons vs not"] <- "Uses street salons" #i10_street_salon_bin
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Uses street salons vs not"] <- "Uses street salons" #i10_street_salon_bin
glmer_doexp_4_rd$term[glmer_doexp_4_rd$term == "Manicures outside home vs not"] <- "Manicures outside home"


glmer_doexp_4_rd <- glmer_doexp_4_rd %>% mutate(relsize = case_when(
  time == "Recruitment" ~ 1.5,
  TRUE ~ 1))
table(glmer_doexp_4_rd$relsize)

glmer_doexp_4_rd <- glmer_doexp_4_rd %>% group_by(time) %>%  mutate(desorder=row_number())
view(glmer_doexp_4_rd)

glmer_doexp_4_rd %>% 
  ## these mutate steps were to change order - redone with desorder step directly before
  #mutate(term = (fct_reorder(fct_rev(term), (glm_mi_4_rd$level)))) %>%
  #mutate(desiredorder = fct_relevel(term, levels = c()))
  #ggplot(aes(x=(fct_rev(term)), y=logodds, color = time)) + #group = interaction(level); alpha = level
  ggplot() +
  geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=logpr, ymin=LCI_95, ymax=UCI_95, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  #geom_point( aes(x=term, y=logodds, group=group, color=time),shape=15, size=7, position=position_dodge2(width = 1.0) ,alpha=0.9) + # this was place on incorrect line if multiple groups
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#CAE0AB","#A6CB72", "#88B253","#618A3D" ))+ 
  coord_flip() + 
  labs(x="", y="Log(PR) of HBsAg+") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = c(.95, .85),
        legend.justification = c("right", "top"),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20))+
  guides(size = "none",color = guide_legend(override.aes = list(size = 1), reverse = T, title = "Test timepoint"))

ggsave('./plots/fig_expdo_4grp_95.png', width=25, height=9)

# add point est and CIs to figure - use cowplot to combine two parts
# update to plot axis on log scale

plot <- 
  glmer_doexp_4_rd %>% 
  ggplot() +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=pr, ymin=lowerci, ymax=upperci, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  labs(y = "Prevalence ratio", x = "Effect") +
    
  #geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  #geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=logpr, ymin=LCI_95, ymax=UCI_95, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#CAE0AB","#A6CB72", "#88B253","#618A3D" ))+ 
  coord_flip()+ 
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 100), minor_breaks = NULL) +
  labs(y = "Prevalence ratio", x = "Effect") +
  #labs(x="", y="Log(PR) of HBsAg+") + # old version when plotting log(PR)
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),# for labeled forest plot 
    axis.ticks.y=element_blank(), # for labeled forest plot 
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    legend.text=element_text(size=20),
    legend.title = element_text(size=20),
    legend.position = c(.95, .85),
    legend.justification = c("right", "top"),
    panel.grid.minor=element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 20))+
  guides(size = "none",color = guide_legend(override.aes = list(size = 1), reverse = T, title = "Test timepoint"))

res_plot_do <- glmer_doexp_4_rd |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(c("pr", "lowerci", "upperci"), ~ str_pad(round(.x, 2), width = 4, pad = "0", side = "right")),
         # add an "-" between logodds estimate confidence intervals
         estimate_lab = paste0(pr, " (", lowerci, ", ", upperci, ")")) |>
  filter(time == "Recruitment") |>
  select(term, level, time, estimate_lab, desorder) 
view(res_plot_do)
# add a row of data that are actually column names which will be shown on the plot in the next step
#add extra decimal for those that are in the thousandths place

estimate_lab <- "PR (95% CI)*"
time <- ""
term <- ""
level <- ""
desorder <- 0
titles_do <- data.frame(estimate_lab, time, term, level,desorder)
view(titles_do)

res_plot_do <-rbind(res_plot_do, titles_do)
view(res_plot_do)

p_left <-
  res_plot_do  |>
  #mutate(term = fct_rev(fct_reorder(term, desorder))) %>%
  ggplot(aes(y = fct_rev(fct_reorder(term, desorder))))+
  geom_text(aes(x = 0, label = term), hjust = 0, size = 5, fontface = "bold")+
  geom_text(aes(x = 1, label = estimate_lab), hjust = 0 , size = 5,
            fontface = ifelse(res_plot_do$estimate_lab == "PR (95% CI)*", "bold", "plain")
  )+
  theme_void() +
  #xlim(0,2)+
  coord_cartesian(xlim = c(0, 1.5))

layout <- c(
  area(t = 0, l = 0, b = 30, r = 5), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 3, l = 6, b = 30, r = 12) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
)
p_left + plot + plot_layout(design=layout)
ggsave('./plots/fig_expdo_4grp_95_numb.png', width=25, height=9)

# plot for main text with just recruitment definition with numbers
view(glmer_doexp_4_rd)
# new version to put axis on log scale - nov 2023
plot_do <- 
  glmer_doexp_4_rd %>% filter(time=="Recruitment") %>% 
  ggplot()+
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=pr, ymin=lowerci, ymax=upperci, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 100),
                minor_breaks = NULL) +
  labs(y = "Prevalence ratio", x = "Effect") +
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#618A3D" ))+ 
  coord_flip(ylim = c(0.1, 100)) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),# for labeled forest plot 
    axis.ticks.y=element_blank(), # for labeled forest plot 
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    legend.position = "none",
    panel.grid.minor=element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 20))

# old version plotting log(PR)
plot_do <- 
  glmer_doexp_4_rd %>% filter(time=="Recruitment") %>% 
  ggplot() +
  geom_hline(yintercept=0, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=logpr, ymin=LCI_95, ymax=UCI_95, color=time, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  scale_size(range = c(30,45))+
  scale_color_manual(values=c("#618A3D" ))+ 
  coord_flip()+ 
  scale_y_continuous(breaks = c(-2, 0, 2, 4))+
  labs(x="", y="Log(PR) of HBsAg+") + 
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),# for labeled forest plot 
    axis.ticks.y=element_blank(), # for labeled forest plot 
    ###
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    legend.position = "none",
    panel.grid.minor=element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 20))

p_left + plot_do + plot_layout(design=layout)
ggsave('./plots/fig_expdo_rec_95_numb.png', width=25, height=6)


# previous version july 2023----------
glmer_doexp_recr_rd <- mutate(glmer_doexp_recr, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
view(glmer_doexp_recr_rd)
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
m_glm <- glm(i27a_rdt_result ~ hr4_sex_f, data=directoffexp, family=binomial("log"))
summary(m_glm)
confint(m_glm)
exp(m_glm$coefficients)
exp(confint(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

m_glm <- glmer(i27a_rdt_result ~ (1 | hrhhid) + hr4_sex_f, data=directoffexp, family=binomial("log"))
summary(m_glm)
fixef(m_glm)
exp(fixef(m_glm))
exp(confint(m_glm, method = c("Wald"), level = 0.95))
exp(confint(m_glm, method = c("Wald"), level = 0.99))

directoffexp$wealth_R_lowestv
m_glm <- glmer(i27a_rdt_result ~ (1 | hrhhid) + wealth_R_lowestv, data=directoffexp, family=binomial("log"))
summary(m_glm)
fixef(m_glm)
confint((m_glm))
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

#Unexp Direct offspring---------------------------------------------------------------
dounexp_recr <-directoff %>% filter(h10_hbv_rdt == 0)
dounexp_recr <- dounexp_recr %>% filter(!(is.na(i27a_rdt_result)))

alldovar <- c("age_combined","hr4_sex_f","age_cat", "wealth_R_lowestv","i13_shared_toothbrush_f","i14_shared_razor_f", "i15_shared_nailclippers_f",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_cat", "partner3mo_bin","newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic")

library(crosstable)
library(flextable)
ou <- crosstable(dounexp_recr, alldovar, by=i27a_rdt_result_f, total="both") %>%
  as_flextable(keep_id=TRUE)
ou
# figure out how to export
save_as_docx(
  "Unexp DO freq tab" = o, 
  path = "./plots/unexpdofreq.docx")


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
allothvar <- c("age_combined","maritalrisk_f","hr4_sex_f","age_cat","i13_shared_toothbrush_f","i14_shared_razor_f", "i15_shared_nailclippers_f",'i12_food_first_chew_f','trans_bin', "i10_street_salon_bin", "i11_manucure_f","i17_tattoo_bin", "i16_traditional_scarring_f","transactionalsex", "debutsex_indic", "partner3mo_bin", "newpartner3mo_indic","partner12mo_bin","newpartner12mo_indic")

table(othermember$h10_hbv_rdt, othermember$i27a_rdt_result_f)
othmemexp_recr <-othermember %>% filter(h10_hbv_rdt == 1)

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
