## analyze hover data
library(survey)
library(lme4)
library(lmtest)
library(geepack)
library(tidyverse)
library(broom)
library(broom.mixed)
library(cowplot)
library(patchwork)

#Datasets for each comparison--------
hhmemb <- inddata1 %>% filter(indexmom=="Household member")
#for hbsag analysis, exclude those with missing results
hhmemb_nomiss <- hhmemb %>% filter(i27a_rdt_result==0 | i27a_rdt_result==1)

##Comp1: Exp/Unexp Overall-----
#exposed household members (non missing)
hhmemb_exp <- hhmemb_nomiss %>% filter(h10_hbv_rdt==1)
#unexposed household members (non missing)
hhmemb_unexp <- hhmemb_nomiss %>% filter(h10_hbv_rdt==0)

##Comp2: Exposed hh members-----
### use hhmemb_exp for PR calc
### for prev CI, subset into exposed DO and exposed other
hhmemb_expdo <- hhmemb_exp %>% filter(directoff==1)
hhmemb_expoth <- hhmemb_exp %>% filter(directoff==0)

##Comp3: Unexposed hh members-----
### use hhmemb_unexp
### for prev CI, subset into unexposed DO and unexposed other
hhmemb_unexpdo <- hhmemb_unexp %>% filter(directoff==1)
hhmemb_unexpoth <- hhmemb_unexp %>% filter(directoff==0)

##Comp4: Direct offspring-----
directoff <- hhmemb_nomiss %>% filter(directoff==1)

##Comp5: Other household members------
othermemb <- hhmemb_nomiss %>% filter(directoff==0)

# men
men_exp <- hhmemb_exp %>% filter(hr3_relationship==2)
men_unexp <- hhmemb_unexp %>% filter(hr3_relationship==2)

# prevalence and CI for each subgroup
# accounting for household clustering - incorporate survey design, with household ID variable (hrhhid) as the cluster ID. no fpc
# cluster svy design for exposed household members
hhmemb_exp1<-svydesign(id=~hrhhid, data=hhmemb_exp) #warning that no weights were applied and equal prob assumed - this is correct
# cluster svy design for unexposed household members
hhmemb_unexp1<-svydesign(id=~hrhhid, data=hhmemb_unexp) #warning that no weights were applied and equal prob assumed - this is correct

# cluster svy design for exposed direct offspring
hhmemb_expdo1<-svydesign(id=~hrhhid, data=hhmemb_expdo) #warning that no weights were applied and equal prob assumed - this is correct
# cluster svy design for unexposed other household members
hhmemb_expoth1<-svydesign(id=~hrhhid, data=hhmemb_expoth) #warning that no weights were applied and equal prob assumed - this is correct

# cluster svy design for unexposed direct offspring
hhmemb_unexpdo1<-svydesign(id=~hrhhid, data=hhmemb_unexpdo) #warning that no weights were applied and equal prob assumed - this is correct
# cluster svy design for unexposed other household members
hhmemb_unexpoth1<-svydesign(id=~hrhhid, data=hhmemb_unexpoth) #warning that no weights were applied and equal prob assumed - this is correct

# exposed men
menexp1<-svydesign(id=~hrhhid, data=men_exp) #warning that no weights were applied and equal prob assumed - this is correct
# unexposed men
menunexp1<-svydesign(id=~hrhhid, data=men_unexp) #warning that no weights were applied and equal prob assumed - this is correct

options(digits=10)
#methods for CI calc: c("logit", "likelihood", "asin", "beta", "mean","xlogit")
# use "logit" method (="lo"), per documentation (https://cran.r-project.org/web/packages/survey/survey.pdf): The "logit" method fits a logistic regression model and computes a Wald-type interval on thelog-odds scale, which is then transformed to the probability scale.
# use "mean" method (="me"), per documentation (https://cran.r-project.org/web/packages/survey/survey.pdf): The "mean" method is a Wald-type interval on the probability scale, the same as confint(svymean())
# exposed overall
svyciprop(~I(i27a_rdt_result), hhmemb_exp1, method = "me", level = 0.95)
# unexposed overall
svyciprop(~I(i27a_rdt_result), hhmemb_unexp1, method = "me", level = 0.95)
# exposed direct off
svyciprop(~I(i27a_rdt_result), hhmemb_expdo1, method = "me", level = 0.95)
# exposed other hh memb
svyciprop(~I(i27a_rdt_result), hhmemb_expoth1, method = "me", level = 0.95)
# unexposed direct off
svyciprop(~I(i27a_rdt_result), hhmemb_unexpdo1, method = "me", level = 0.95)
# unexposed other hh memb
svyciprop(~I(i27a_rdt_result), hhmemb_unexpoth1, method = "me", level = 0.95)
# exposed male partners
svyciprop(~I(i27a_rdt_result), menexp1, method = "me", level = 0.95)
# unexposed male partners
svyciprop(~I(i27a_rdt_result), menunexp1, method = "me", level = 0.95)

#Comparison 1: prev ratio of hbsag positivity of household members in exposed vs unexposed--------------- 
# using index mother's CPN/recruitment screening result, not result at enrollment

# 2x2 values
addmargins(table(hhmemb_nomiss$i27a_rdt_result_f, hhmemb_nomiss$h10_hbv_rdt_f))

# basic model not accounting for clustering
comp1 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "log"), data=hhmemb_nomiss)
summary(comp1)
exp(comp1$coefficients)
exp(confint(comp1, method = c("Wald")))
exp(confint(comp1, method = c("boot"), boot.type=c("basic")))
exp(confint(comp1, method = c("boot"), boot.type=c("perc")))


# account for clustering
###GEE using geeglm() does not work 
# comp1gee_ind <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=hhmemb_nomiss, family=binomial, corstr="ind")

###glmer------
# log link for prevalence ratio
comp1mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0
summary(comp1mm)
fixef(comp1mm)
exp(fixef(comp1mm))

#compare to OR (logit link)
comp1mm_or <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0
summary(comp1mm_or)
fixef(comp1mm_or)
exp(fixef(comp1mm_or))
exp(confint(comp1mm_or, method = c("Wald"))) # proceeding with Wald

#unadjusted OR
comp1mm_oru <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=hhmemb_nomiss) # does not converge without nAGQ=0
summary(comp1mm_oru)
exp(comp1mm_oru$coefficients)
exp(confint(comp1mm_oru, method = c("Wald")))

# lmerTest::con
lrtest(comp1, comp1mm)

tidy(comp1mm, effects="fixed", conf.int=T, confint.method = "boot")

# checking the values of various CI approaches
exp(confint(comp1mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp1mm, method = c("boot"), boot.type=c("perc")))
exp(confint(comp1mm, method = c("Wald"))) # proceeding with Wald
exp(confint(comp1mm, method = c("profile")))

#....................................................................................................
# Comparison 2: odds of infection in exposed (index+) households comparing offspring vs non offspring---------
hhmemb_nomiss %>% group_by(h10_hbv_rdt,directoff, i27a_rdt_result)%>%  count()

# use hhmemb_exp 
addmargins(table(hhmemb_exp$i27a_rdt_result_f, hhmemb_exp$hhmemcat_f))

# unadjusted (no hh clustering)
comp2 <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "log"), data=hhmemb_exp)
summary(comp2)
exp(comp2$coefficients)
exp(confint(comp2, method = c("Wald")))
# comparison of fixed and mixed
lrtest(comp2, comp2mm)


### glmer----
comp2mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_exp, nAGQ = 0)
summary(comp2mm)
fixef(comp2mm)
exp(fixef(comp2mm))
exp(confint(comp2mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp2mm, method = c("Wald")))

#compare to OR
comp2mm_or <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_exp, nAGQ = 0) # does not converge without nAGQ=0
summary(comp2mm_or)
fixef(comp2mm_or)
exp(fixef(comp2mm_or))
exp(confint(comp2mm_or, method = c("Wald"))) # proceeding with Wald

#unadjusted OR
comp2_oru <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "logit"), data=hhmemb_exp)
summary(comp2_oru)
exp(comp2_oru$coefficients)
exp(confint(comp2_oru, method = c("Wald")))


#....................................................................................................
#Comparison 3: odds of infection in unexposed households comparing offspring vs non offspring--------------------
#use df hhmemb_unexp 
addmargins(table(hhmemb_unexp$directoff, hhmemb_unexp$i27a_rdt_result_f))

# not accounting for clustering
comp3 <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "log"), data=hhmemb_unexp)
summary(comp3)
exp(comp3$coefficients)
exp(confint(comp3, method = c("Wald")))

###glmer-----
comp3mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_unexp, nAGQ = 0)
summary(comp3mm)
fixef(comp3mm)
exp(fixef(comp3mm))
exp(confint(comp3mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp3mm, method = c("Wald")))

#compare to OR
comp3mm_or <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_unexp, nAGQ = 0) # does not converge without nAGQ=0
summary(comp3mm_or)
exp(fixef(comp3mm_or))
exp(confint(comp3mm_or, method = c("Wald"))) # proceeding with Wald

#unadjusted OR
comp3_oru <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "logit"), data=hhmemb_unexp)
summary(comp3_oru)
exp(comp3_oru$coefficients)
exp(confint(comp3_oru, method = c("Wald")))

# comparison of fixed and mixed
lrtest(comp3, comp3mm)
#....................................................................................................
#Comparison 4: odds of infection in exposed direct off vs unexposed direct off-----
addmargins(table(directoff$i27a_rdt_result_f, directoff$h10_hbv_rdt_f))

# unadjusted
comp4 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "log"), data=directoff)
summary(comp4)
exp(comp4$coefficients)
exp(confint(comp4, method = c("Wald")))

###glmer-----
comp4mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=directoff, nAGQ = 0)
summary(comp4mm)
fixef(comp4mm)
exp(fixef(comp4mm))
exp(confint(comp4mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp4mm, method = c("Wald")))

#compare to OR
comp4mm_or <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=directoff, nAGQ = 0) # does not converge without nAGQ=0
summary(comp4mm_or)
fixef(comp4mm_or)
exp(fixef(comp4mm_or))
exp(confint(comp4mm_or, method = c("Wald"))) 

#unadjusted OR
comp4mm_oru <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=directoff)
summary(comp4mm_oru)
exp(comp4mm_oru$coefficients)
exp(confint(comp4mm_oru, method = c("Wald")))

# comparison of fixed and mixed
lrtest(comp4, comp4mm)

#....................................................................................................
#Comparison 5: odds of infection in exposed other mem vs unexposed other mem------
addmargins(table(othermemb$h10_hbv_rdt_f, othermemb$i27a_rdt_result_f ))

# not accounting for clustering
comp5 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "log"), data=othermemb)
summary(comp5)
exp(comp5$coefficients)
exp(confint(comp5, method = c("Wald")))

###glmer-----
comp5mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=othermemb, nAGQ = 0)
summary(comp5mm)
fixef(comp5mm)
exp(fixef(comp5mm))
exp(confint(comp5mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp5mm, method = c("Wald")))

# comparison of fixed and mixed
lrtest(comp5, comp5mm)

#compare to OR
comp5mm_or <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=othermemb, nAGQ = 0) # does not converge without nAGQ=0
summary(comp5mm_or)
exp(fixef(comp5mm_or))
exp(confint(comp5mm_or, method = c("Wald"))) 

#unadjusted OR
comp5mm_oru <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=othermemb)
summary(comp5mm_oru)
exp(comp5mm_oru$coefficients)
exp(confint(comp5mm_oru, method = c("Wald")))


#..................................................................
# male partners
# use df men

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

# ORs
# dont need to account for clustering since only one per hh
comp_husb_or <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=men)
summary(comp_husb_or)
exp(comp_husb_or$coefficients)
exp(confint(comp_husb_or, method = c("Wald")))


# # Sensitivity analyses for definitions of "exposed"------------------------------------------------------------------------------------------------
# recruitment def from above : comp1mm
comp1mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0
comp1mm_enr <- glmer(i27a_rdt_result ~ perprot_h10 + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0
comp1mm_any <- glmer(i27a_rdt_result ~ anypos + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0
comp1mm_only <- glmer(i27a_rdt_result ~ onlypos + (1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss, nAGQ = 0) # does not converge without nAGQ=0

exp_def <- c("h10_hbv_rdt","perprot_h10", "anypos","onlypos" )
# comparisons 1,4,5 the predictor variable changes for different exposure definitions; 
# comparisons 2/3 the dataset changes for different exposure defs

comp1 <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=hhmemb_nomiss, family=binomial("log"), nAGQ = 0)
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis95 <- as.data.frame(confint(m, method = c("Wald")))
  cis95 <- tibble::rownames_to_column(cis95, "term") %>% setNames(c("term", "LCI_95", "UCI_95"))
  cis95 <- cis95 %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis95)
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}

comp1_run <- map_dfr(exp_def, comp1) 
view(comp1_run)
comp1_run$comp <- "Comparison 1"
comp1_run$sens <- c("Recruitment", "Enrollment", "Either positive", "Always positive")
comp1_run$kimorder <- 1

comp4 <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=directoff, family=binomial("log"), nAGQ = 0)
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis95 <- as.data.frame(confint(m, method = c("Wald")))
  cis95 <- tibble::rownames_to_column(cis95, "term") %>% setNames(c("term", "LCI_95", "UCI_95"))
  cis95 <- cis95 %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis95)
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}
comp4_pos <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=directoff, family=poisson("log"), nAGQ = 0)
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis95 <- as.data.frame(confint(m, method = c("Wald")))
  cis95 <- tibble::rownames_to_column(cis95, "term") %>% setNames(c("term", "LCI_95", "UCI_95"))
  cis95 <- cis95 %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis95)
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}

exp_def_3 <- c("h10_hbv_rdt","perprot_h10", "onlypos" ) #"anypos" doesn't converge for log-binomial
comp4_test3 <- map_dfr(exp_def_3, comp4) 
comp4_test4 <- map_dfr(exp_def, comp4_pos) 
#comp4combo <- rbind(comp4_test3, comp4_test4)
view(comp4_test4) # poisson doesn't change estimate too much
comp4_test4$comp <- "Comparison 4"
comp4_test4$sens <- c("Recruitment", "Enrollment", "Either positive", "Always positive")
comp4_test4$kimorder <- 2

comp5 <- function(var){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~  (1 | hrhhid) +', var)), data=othermemb, family=binomial("log"), nAGQ = 0)
  est <- tidy(m) %>% filter(stringr::str_detect(term, var))
  cis95 <- as.data.frame(confint(m, method = c("Wald")))
  cis95 <- tibble::rownames_to_column(cis95, "term") %>% setNames(c("term", "LCI_95", "UCI_95"))
  cis95 <- cis95 %>%  filter(stringr::str_detect(term, var))
  m <- cbind(est,cis95)
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))}
comp5 <- map_dfr(exp_def, comp5) 
comp5$comp <- "Comparison 5"
comp5$sens <- c("Recruitment", "Enrollment", "Either positive", "Always positive")
comp5$kimorder <- 3

# datasets for comp 2
hhmemb_exp <- hhmemb_nomiss %>% filter(h10_hbv_rdt==1) # recruitment
hhmemb_expenr <- hhmemb_nomiss %>% filter(perprot_h10==1) # enrollment
hhmemb_expany <- hhmemb_nomiss %>% filter(anypos==1) # any pos
hhmemb_exponly <- hhmemb_nomiss %>% filter(onlypos==1) # only pos
# for comp2
exp_dfs <- list(hhmemb_exp, hhmemb_expenr, hhmemb_expany, hhmemb_exponly)

comp23 <- function(dfs){
  m <- glmer(as.formula(paste0('i27a_rdt_result ~ directoff + (1 | hrhhid)')),  data=data.frame(dfs), family=binomial("log"), nAGQ = 0)
  est <- tidy(m) %>% filter(stringr::str_detect(term, 'directoff'))
  cis95 <- as.data.frame(confint(m, method = c("Wald")))
  cis95 <- tibble::rownames_to_column(cis95, "term") %>% setNames(c("term", "LCI_95", "UCI_95"))
  cis95 <- cis95 %>%  filter(stringr::str_detect(term, "directoff"))
  m <- cbind(est,cis95)
  m <- m[,-8]
  m <- m %>% select(-c('effect','group'))
  return(m)}

comp2_out <- map_dfr(exp_dfs, comp23) 
view(comp2_out)
#exp_dfs_names <- c("hhmemb_exp", "hhmemb_expenr", "hhmemb_expany", "hhmemb_exponly")
exp_dfs_names <- c("Recruitment", "Enrollment", "Either positive", "Always positive")
comp2_out$sens <- exp_dfs_names
comp2_out$comp <- "Comparison 2"
comp2_out$kimorder <- 4
# comp 3

# datasets for comp 3
hhmemb_unexp <- hhmemb_nomiss %>% filter(h10_hbv_rdt==0) # recruitment
hhmemb_unexpenr <- hhmemb_nomiss %>% filter(perprot_h10==0) # enrollment
hhmemb_unexpany <- hhmemb_nomiss %>% filter(anypos==0) # any pos
hhmemb_unexponly <- hhmemb_nomiss %>% filter(onlypos==0) # only pos
# list of dfs for comp3
unexp_dfs <- list(hhmemb_unexp, hhmemb_unexpenr, hhmemb_unexpany, hhmemb_unexponly)

comp3_out <- map_dfr(unexp_dfs, comp23) 
view(comp3_out)
comp3_out$comp <- "Comparison 3"
comp3_out$sens <- c("Recruitment", "Enrollment", "Either positive", "Always positive")
comp3_out$kimorder <- 5

all_comps <- rbind(comp1_run, comp4_test4, comp5, comp2_out, comp3_out)
view(all_comps)

all_comps <- all_comps %>% 
  mutate(est_exp = exp(estimate), lowerci_exp = exp(LCI_95), upperci_exp = exp(UCI_95), clr = upperci_exp/lowerci_exp )
view(all_comps)

all_comps <- all_comps %>% mutate(relsize = case_when(
  sens == "Recruitment" ~ 1.5,
  TRUE ~ 1))
table(all_comps$relsize)
view(all_comps)

all_comps <- all_comps %>% mutate(desorder = substr(comp, nchar(comp)-1+1, nchar(comp)) %>% as.numeric()) # desorder = original order
  #colnames(glmer_doexp_4_rd) <- c('term','logpr','std.error','statistic','p.value','LCI_95','UCI_95','LCI_99','UCI_99','time')
all_comps <-   all_comps %>% mutate(labl = case_when(
  comp == "Comparison 1" ~ "All household members: index+ vs index-",
  comp == "Comparison 4" ~ "Direct offspring:  index+ vs index-",
  comp == "Comparison 5" ~ "Other: index+ vs index-",
  comp == "Comparison 2" ~ "Index+: direct offspring vs other",
  comp == "Comparison 3" ~ "Index-: direct offspring vs other"
  #comp == "All household members: exposed vs unexposed" ~ "All household members: index+ vs index-",
  #comp == "Direct offspring: exposed vs unexposed" ~ "Direct offspring:  index+ vs index-",
  #comp == "Other: exposed vs unexposed" ~ "Other: index+ vs index-",
  #comp == "Exposed: direct offspring vs other" ~ "Index+: direct offspring vs other",
  #comp == "Unexposed: direct offspring vs other" ~ "Index-: direct offspring vs other"
))
view(all_comps)
all_comps %>% filter(abs(estimate) < 10) %>% 
  ggplot() +
  geom_hline(yintercept=1, linetype='dashed') + # use 0 if logodds, 1 if odds
#  geom_pointrange(aes(x=fct_rev(fct_reorder(labl, kimorder)), y=estimate, ymin=LCI_95, ymax=UCI_95, color=sens, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  geom_pointrange(aes(x=fct_rev(fct_reorder(labl, kimorder)), y=est_exp, ymin=lowerci_exp, ymax=upperci_exp, color=sens, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  #geom_point( aes(x=term, y=logodds, group=group, color=time),shape=15, size=7, position=position_dodge2(width = 1.0) ,alpha=0.9) + # this was place on incorrect line if multiple groups
  scale_size(range = c(30,45))+
  #scale_color_brewer(palette = "Reds")+
  scale_color_manual(values=c( "#d19999", "#b96666", "#9c1526","#6f0f1b" ))+ # "#8b0000" , "#971919", "#a23333", "#ae4d4d", "#b96666", "#d19999"
  coord_flip() + 
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0), minor_breaks = NULL) +
  labs(x="", y="Prevalence ratio") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = c(.95, .75),
        legend.justification = c("right", "top"),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20))+
  guides(size = "none",color = guide_legend(override.aes = list(size = 1), reverse = T, title = "Test timepoint"))
ggsave('./plots/prev_sens.png', width=25, height=9)

p_prev <-
  all_comps %>% filter(abs(estimate) < 10) %>% 
  ggplot() +
  geom_hline(yintercept=1, linetype='dashed') + # use 0 if logodds, 1 if odds
  geom_pointrange(aes(x=fct_rev(fct_reorder(labl, kimorder)), y=est_exp, ymin=lowerci_exp, ymax=upperci_exp, color=sens, size=relsize), shape=15,   position=position_dodge2(width=0.8),fatten=0.1) + #size=0.8,  #show.legend=F,  color=timepoint
  #geom_point( aes(x=term, y=logodds, group=group, color=time),shape=15, size=7, position=position_dodge2(width = 1.0) ,alpha=0.9) + # this was place on incorrect line if multiple groups
  scale_size(range = c(30,45))+
  #scale_color_brewer(palette = "Reds")+
  scale_color_manual(values=c( "#d19999", "#b96666", "#9c1526","#6f0f1b" ))+ # "#8b0000" , "#971919", "#a23333", "#ae4d4d", "#b96666", "#d19999"
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0), minor_breaks = NULL) +
  coord_flip() + 
  labs(x="", y="Prevalence ratio") + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),# for labeled forest plot 
        axis.ticks.y=element_blank(), # for labeled forest plot 
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = c(.95, .75),
        legend.justification = c("right", "top"),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 20))+
  guides(size = "none",color = guide_legend(override.aes = list(size = 1), reverse = T, title = "Test timepoint"))


res_plot_prev <- all_comps |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(c("est_exp", "lowerci_exp", "upperci_exp"), ~ str_pad(round(.x, 2), width = 4, pad = "0", side = "right")),
         # add an "-" between logodds estimate confidence intervals
         estimate_lab = paste0(est_exp, " (", lowerci_exp, ", ", upperci_exp, ")")) |>
  filter(sens == "Recruitment") |>
  select(labl, sens, estimate_lab, kimorder) 
view(res_plot_prev)
# add a row of data that are actually column names which will be shown on the plot in the next step
#add extra decimal for those that are in the thousandths place

estimate_lab <- "PR (95% CI)*"
sens <- ""
labl <- ""
kimorder <- 0
titles_prev <- data.frame(estimate_lab, sens, labl,kimorder)
view(titles_prev)

res_plot_prev <-rbind(res_plot_prev, titles_prev)
view(res_plot_prev)
res_plot_prev$estimate_lab <- ifelse(res_plot_prev$estimate_lab=="-0.1 (-1.35, 1.15)", "-0.10 (-1.35, 1.15)",res_plot_prev$estimate_lab)

p_left_prev <-
  res_plot_prev  |>
  ggplot(aes(y = fct_rev(fct_reorder(labl, kimorder))))+
  geom_text(aes(x = 0, label = labl), hjust = 0, size = 10, fontface = "bold")+
  geom_text(aes(x = 1, label = estimate_lab), hjust = 0 , size = 10,
            fontface = ifelse(res_plot_prev$estimate_lab == "PR (95% CI)*", "bold", "plain")
  )+
  theme_void() +
  coord_cartesian(xlim = c(0, 1.5))

layout <- c(
  area(t = 0, l = 0, b = 30, r = 5), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 5, l = 6, b = 30, r = 12) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
)
p_left_prev + p_prev + plot_layout(design=layout)
ggsave('./plots/prev_sens_label.png', width=38, height=9)

