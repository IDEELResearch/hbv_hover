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
comp1 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=hhmemb_nomiss)
summary(comp1)
exp(comp1$coefficients)
exp(confint(comp1))

# account for clustering
###GEE using geeglm() does not work 
# comp1gee_ind <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=hhmemb_nomiss, family=binomial, corstr="ind")

###glmer------
comp1mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_nomiss)
summary(comp1mm)
fixef(comp1mm)
exp(fixef(comp1mm))
# NEED TO FIGURE OUT CI FROM ML MODEL
exp(confint((comp1mm)))
lmerTest::con
lrtest(comp1, comp1mm)
library(broom)
tidy(comp1mm, effects="fixed", conf.int=T, confint.method = "boot")

# checking the values of various CI approaches
exp(confint(comp1mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp1mm, method = c("boot"), boot.type=c("perc")))
exp(confint(comp1mm, method = c("Wald"))) # proceeding with Wald
exp(confint(comp1mm, method = c("profile")))

#....................................................................................................
# Comparison 2: odds of infection in exposed households comparing offspring vs non offspring---------
hhmemb_nomiss %>% group_by(h10_hbv_rdt,directoff, i27a_rdt_result)%>%  count()

# original approach from proposal
hhmemb_exp <- hhmemb %>% filter(h10_hbv_rdt==1)
addmargins(table(hhmemb_exp$i27a_rdt_result_f, hhmemb_exp$hhmemcat_f))

### glmer----
comp2mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_exp)
summary(comp2mm)
fixef(comp2mm)
exp(fixef(comp2mm))
exp(confint(comp2mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp2mm, method = c("Wald")))

#....................................................................................................
#Comparison 3: odds of infection in unexposed households comparing offspring vs non offspring--------------------
hhmemb_unexp <- hhmemb %>% filter(h10_hbv_rdt==0)

addmargins(table(hhmemb_unexp$directoff, hhmemb_unexp$i27a_rdt_result_f))
inddata1 %>% group_by(h10_hbv_rdt, perprot_h10,i27a_rdt_result_f, directoff ) %>% count()

###glmer-----
comp3mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_unexp)
summary(comp3mm)
fixef(comp3mm)
exp(fixef(comp3mm))
exp(confint(comp3mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp3mm, method = c("Wald"), boot.type=c("basic")))

#....................................................................................................
#Comparison 4: odds of infection in exposed direct off vs unexposed direct off-----
directoff <- hhmemb %>% filter(directoff==1)
addmargins(table(directoff$i27a_rdt_result_f, directoff$h10_hbv_rdt_f))

###glmer-----
comp4mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=directoff)
summary(comp4mm)
fixef(comp4mm)
exp(fixef(comp4mm))
exp(confint(comp4mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp4mm, method = c("Wald")))

#....................................................................................................
#Comparison 5: odds of infection in exposed other mem vs unexposed other mem------
othermemb <- hhmemb %>% filter(directoff==0)
addmargins(table(othermemb$i27a_rdt_result_f, othermemb$h10_hbv_rdt_f))

###glmer-----
comp5mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=othermemb)
summary(comp5mm)
fixef(comp5mm)
exp(fixef(comp5mm))
exp(confint(comp5mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp5mm, method = c("Wald")))

#..................................................................
# male partners
men <- hhmemb %>% filter(hr3_relationship==2)

comp_husb <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=men)
summary(comp5mm)
fixef(comp5mm)
exp(fixef(comp5mm))
exp(confint(comp5mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp5mm, method = c("Wald")))

# dont need to account for clustering since only one per hh
comp_husb <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=men)
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

allmivar <- c("age_combined","maritalrisk_f","malepartpos","wealth_R","i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f','i8_transfusion_f', "transfus_num","i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")
alldovar <- c("age_combined","hr4_sex_f","cpshbvprox_rev", "wealth_R","i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f','i8_transfusion_f', "transfus_num","i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")
allmenvar <- c("age_combined","wealth_R","i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f','i8_transfusion_f', "transfus_num","i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")
allothvar <- c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev","i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f','i8_transfusion_f', "transfus_num","i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")

# when i try to run glm/glmer for one that has <2 levels, gives an error - how to add a step in the function to check how many levels?
# for now running separately by subgroup
risk_ind_mi <-  c("age_combined","maritalrisk_f","malepartpos","wealth_R") # "hr4_sex_f","cpshbvprox_rev", <- figure out to have single group of ind vars 
risk_ind_do <-  c("age_combined","hr4_sex_f","cpshbvprox_rev", "wealth_R") #"maritalrisk_f","malepartpos",
risk_ind_men <-  c("age_combined","wealth_R") #"maritalrisk_f","malepartpos",
risk_ind_oth <-  c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev", "wealth_R") #"malepartpos",
risk_ind_oth_sub <-  c("age_combined","maritalrisk_f","hr4_sex_f","cpshbvprox_rev") #, "wealth_R" leads to non-positive variance

# household
risk_hh_all <-  c( "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f') 
# might not need divisions by type

# community
risk_comm_all <-  c('i8_transfusion_f', "transfus_num","i10_street_salon_f", "i11_manucure_f","i17_tattoo_f", "i16_traditional_scarring_f",'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat", "part3mo_cat", "i23a_sex_hx_past3mo_num_f", "part12mo_cat", "partnew12mo_cat")
# might not need divisions by type

# moms function (no clustering)
itt_model_moms <- function(var){ # glm function
  m <- glm(as.formula(paste0('h10_hbv_rdt ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}
# figure out to add a step to check number of levels around var so same risk_ind can be used for moms/DO/other

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

itt_model_oth <- function(var){ # glmer function
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=othermemb_nomen, family=binomial("logit"))
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
mutate(glm_mi, across(where(is.numeric), round, 3)) # better way to print/view results and restrict decimals without going into sci not
# mutate(glm_mi, across(where(is.numeric), round, 3),na.rm = TRUE) # better way to print/view results and restrict decimals without going into sci not
colnames(glm_mi) <- c('term','logodds','std.error','statistic','p.value','LCI','UCI')
# could combine these tables before the figure
rownames(glm_mi) <- 1:nrow(glm_mi)
glm_mi$group <- "Index mothers"
glm_mi <- glm_mi %>% mutate(level = case_when(
  str_detect(term, paste(risk_ind_mi, collapse="|")) ~ "Individual",
  str_detect(term, paste(risk_hh_all, collapse="|")) ~ "Household",
  str_detect(term, paste(risk_comm_all, collapse="|")) ~ "Community",
))

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
ind_all <- rbind(glm_mi, glmer_do, glmer_men,glmer_oth)
ind_all$level <- as.factor(ind_all$level)
ind_all$group <- as.factor(ind_all$group)
ind_all <- tibble::rownames_to_column(ind_all, "index") # make integer index to call on later
nrow(ind_all)

# rename variables for plot
ind_all$term[ind_all$term == "wealth_R3"] <- "Highest wealth (Q4) vs lowest (Q1)"
ind_all$term[ind_all$term == "wealth_R2"] <- "Higher wealth (Q3) vs lowest (Q1)"
ind_all$term[ind_all$term == "wealth_R1"] <- "Lower wealth (Q2) vs lowest (Q1)"
ind_all$term[ind_all$term == "maritalrisk_fNot married"] <- "Marriage: Never vs married"
ind_all$term[ind_all$term == "maritalrisk_fDivorced/Widowed"] <- "Marriage: Divorced/widowed vs married"
ind_all$term[ind_all$term == "malepartpos"] <- "Male partner HBsAg pos vs neg"
ind_all$term[ind_all$term == "hr4_sex_fFemale"] <- "Female vs male"
ind_all$term[ind_all$term == "cpshbvprox_revProbably not vaccinated"] <- "HBV vaccination: likely not vs probably"
ind_all$term[ind_all$term == "cpshbvprox_revPossibly vaccinated"] <- "HBV vaccination: possibly vs probably"
ind_all$term[ind_all$term == "age_combined"] <- "Age (continuous, years)"
ind_all$term[ind_all$term == "i13_shared_toothbrush_fYes"] <- "Shares toothbrushes in house"
ind_all$term[ind_all$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
ind_all$term[ind_all$term == "i8_transfusion_fYes"] <- "Past transfusion vs never"
ind_all$term[ind_all$term == "transfus_num9"] <- "≥4 transfusions vs none"
ind_all$term[ind_all$term == "transfus_num3"] <- "3 transfusions vs none"
ind_all$term[ind_all$term == "transfus_num1"] <- "1 transfusion vs none"
ind_all$term[ind_all$term == "i10_street_salon_fYes"] <- "Uses street salons vs not"
ind_all$term[ind_all$term == "i11_manucure_fYes"] <- "Manicures outside home vs not"
ind_all$term[ind_all$term == "i14_shared_razor_fYes"] <- "Shares razors in house vs not"
ind_all$term[ind_all$term == "i15_shared_nailclippers_fYes"] <- "Shares nail clippers in house vs not"
ind_all$term[ind_all$term == "i17_tattoo_fYes"] <- "Tattoos: Yes vs none"
ind_all$term[ind_all$term == "i17_tattoo_fRefused"] <- "Tattoos: refused vs none"
ind_all$term[ind_all$term == "i16_traditional_scarring_fYes"] <- "Traditional scarring vs none"
ind_all$term[ind_all$term == "i25_sex_hx_receive_money_fYes"] <- "Received money for sex vs never"
ind_all$term[ind_all$term == "i26_sex_hx_given_money_fYes"] <- "Given money for sex vs never"
ind_all$term[ind_all$term == "i26_sex_hx_given_money_fRefused"] <- "Refused: given money for sex vs never"
ind_all$term[ind_all$term == "debutsex_cat≥18"] <- "Age (years) of sexual debut: <18 vs ≥18"
#ind_all$term[ind_all$term == "debutsex_cat<18"] <- "Sexual debut <18 yrs vs ≥ 18"
ind_all$term[ind_all$term == "debutsex_catRefused/don't know"] <- "Age (years) of sexual debut: Refused vs ≥18"

ind_all$term[ind_all$term == "part3mo_catMore than 1"] <- "Sexual partners in last 3 months: ≥2 vs none"
ind_all$term[ind_all$term == "part3mo_catRefused"] <- "Sexual partners in last 3 months: Refused vs none"
ind_all$term[ind_all$term == "part3mo_cat1"] <- "Sexual partners in last 3 months: 1 vs none"
ind_all$term[ind_all$term == "partnew3mo_cat1"] <- "Sexual partners in last 3 months: ≥1 vs no new"
ind_all$term[ind_all$term == "partnew3mo_cat1"] <- "New sexual partners in last 3 months: ≥1 vs no new"
ind_all$term[ind_all$term == "partnew3mo_cat9"] <- "New sexual partners in last 3 months: Refused vs no new"
ind_all$term[ind_all$term == "i23a_sex_hx_past3mo_num_fMore than 1/Don't know"] <- "New sexual partners in last 3 months: >1 vs no new"
ind_all$term[ind_all$term == "i23a_sex_hx_past3mo_num_f1 new"] <- "New sexual partners in last 3 months: 1 vs no new"
ind_all$term[ind_all$term == "part12mo_catMore than 1/Don't know"] <- "Sexual partners in last 12 months: ≥2 vs ≤1"
ind_all$term[ind_all$term == "part12mo_cat1"] <- "Sexual partners in last 12 months: 1 vs ≤1"
ind_all$term[ind_all$term == "part12mo_catRefused"] <- "Sexual partners in last 12 months: Refused vs ≤ 1"
ind_all$term[ind_all$term == "partnew12mo_catAt least 1/ Don't know"] <- "New sexual partners in last 12 months: ≥1 vs no new"
ind_all$term[ind_all$term == "partnew12mo_catRefused"] <- "New sexual partners in last 12 months: Refused vs no new"
#ind_all$term[ind_all$term == "Refused: # sexual partners in last 3mo vs ≤ 1"] <- " Refused: # sexual partners in last 3mo vs ≤ 1"
#ind_all$term[ind_all$term == "Sexual debut <18 yrs vs ≥ 18"] <- "  Sexual debut <18 yrs vs ≥ 18"
#ind_all$term[ind_all$term == "Refused to answer age of sexual debut vs ≥ 18 yrs"] <- "   Refused to answer age of sexual debut vs ≥ 18 yrs"
# others
max(ind_all_nomiss_abs$logodds)
min(ind_all_nomiss_abs$LCI)
max(ind_all_nomiss_abs$UCI)

# restrict to those with reasonable conf ints (some in 1000s)
ind_all_nomiss <- ind_all %>% filter_at(vars(logodds,LCI, UCI),all_vars(!is.na(.)))
nrow(ind_all_nomiss)
ind_all_nomiss_abs <- ind_all_nomiss %>% filter_at(vars(logodds,LCI, UCI),all_vars(abs(.)<50))
nrow(ind_all_nomiss_abs)

# look through those with really wide CLRs and decide any coding changes to improve counts
riskfactelim <- ind_all %>% filter(!(index %in% ind_all_nomiss_abs$index))
riskfactelim <- riskfactelim %>% select(-c("statistic", "p.value"))

# to plot
#p<- 
ind_all_nomiss_abs %>% filter(UCI<=10) %>% 
  mutate(term = fct_reorder(term, as.integer(level))) %>%
  ggplot(aes(x=term, y=estimate)) + #group = interaction(level); alpha = level
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=logodds, ymin=LCI, ymax=UCI), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=logodds, group=group, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#A6DEAE","#020E54","#FF700A","deeppink3"))+ #deeppink3
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
  facet_wrap(~ factor(group, levels = c("Index mothers", "Male partners", "Direct offspring", "Other household members")),nrow = 1)

  ggsave('./plots/fig4.png', width=25, height=18)
  
# get image on axis-------------  
test <- ind_all_nomiss_abs %>%  filter(grepl("transfus_num",term))
test <- test %>% tibble::rownames_to_column("id") 
library(ggtext)

rm(labels)
labels <- c()

for (i in 1:length(test$term)){
    img.name <- test$term[i]
    labels <- c(labels, paste0("<img src='./images/", test$term[i], ".png", "' width='25' /><br>*", img.name,"*"))
  }

test %>% filter(UCI<=10) %>% 
    #mutate(term = fct_reorder(term, as.integer(level))) %>%
    ggplot(aes(x=id, y=estimate)) +
    geom_hline(yintercept=1, linetype='dashed') +
    geom_pointrange(aes(x=id, y=logodds, ymin=LCI, ymax=UCI), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
    geom_point(shape=15, size=7, aes(x=id, y=logodds, group=group, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
    scale_color_manual(values=c("#A6DEAE","#020E54","#FF700A","deeppink3"))+ #deeppink3
    #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
    scale_x_discrete(breaks=test$id,labels=labels) + #, trans = "reverse"
    coord_flip() + #theme_bw() + 
    #ylim(0, 30)+
    labs(x="", y="Log(OR) of HBsAg+") + 
    theme(axis.text.y = ggtext::element_markdown(color = "black", size = 0),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.grid.minor=element_blank(),
          panel.background = element_blank(),
          strip.text = element_text(size = 20)) +
    #ggtitle("Log(OR) of HBsAg by individual-level") +
    #facet_grid(~group)
    facet_wrap(~ factor(group, levels = c("Index mothers", "Male partners", "Direct offspring", "Other household members")),nrow = 1)
  
  
