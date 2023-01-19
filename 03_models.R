## analyze hover data
library(lme4)
library(lmtest)
library(geepack)
library(tidyverse)
#Comparison 1: odds of infection of household members in exposed vs unexposed--------------- 
hhmemb <- inddata1 %>% filter(indexmom=="Household member")
#exclude those with missing results
hhmemb_nomiss <- hhmemb %>% filter(i27a_rdt_result==0 | i27a_rdt_result==1)

# basic model not accounting for clustering
comp1 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=hhmemb_nomiss)
summary(comp1)
exp(comp1$coefficients)
exp(confint(comp1))

# account for clustering
##ITT--------
###GEE----
comp1gee_ind <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=hhmemb_nomiss, family=binomial, corstr="ind")
summary(comp1gee_ind)
exp(comp1gee_ind$coefficients)
(tidy(comp1gee_ind, conf.int = T)) # not working

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

exp(confint(comp1mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp1mm, method = c("boot"), boot.type=c("perc")))
exp(confint(comp1mm, method = c("Wald")))
exp(confint(comp1mm, method = c("profile")))

##PP------
addmargins(table(hhmemb_nomiss$perprot_h10, hhmemb_nomiss$i27a_rdt_result_f, useNA = "always"))
# CI of prev
prev_pp_exp <- 24/381
nexp <- 381
prev_pp_unexp <- 3/422
nunexp <- 422
z <- 1.96

prev_pp_exp - z*sqrt((prev_pp_exp*(1-prev_pp_exp))/nexp)
prev_pp_exp + z*sqrt((prev_pp_exp*(1-prev_pp_exp))/nexp)

prev_pp_unexp - z*sqrt((prev_pp_unexp*(1-prev_pp_unexp))/nunexp) # --> below 0 so use 0
prev_pp_unexp + z*sqrt((prev_pp_unexp*(1-prev_pp_unexp))/nunexp)

# exposed
addmargins(table(hhmemb_nomiss$perprot_h10, hhmemb_nomiss$i27a_rdt_result_f, useNA = "always"))

# unexposed
addmargins(table(hhmemb_nomiss$perprot_h10, hhmemb_nomiss$i27a_rdt_result_f, useNA = "always"))

###GEE----
comp1gee_pp <- geeglm(i27a_rdt_result ~ as.factor(perprot_h10), id=hrhhid, data=hhmemb_nomiss, family=binomial, corstr="ind")
summary(comp1gee_pp)
exp(comp1gee_pp$coefficients)
(tidy(comp1gee_pp, conf.int = T)) # not working

# Comparison 2: odds of infection in exposed households comparing offspring vs non offspring---------
hhmemb_nomiss %>% group_by(h10_hbv_rdt,directoff, i27a_rdt_result)%>%  count()

# original approach from proposal
hhmemb_exp <- hhmemb %>% filter(h10_hbv_rdt==1)
##ITT--------
### GEE-----
comp2_gee <- geeglm(i27a_rdt_result ~ as.factor(directoff), id=hrhhid, data=hhmemb_exp, family=binomial)
summary(comp2_gee$std.err)
exp(comp2_gee$coefficients)
(tidy(comp2_gee, conf.int = T)) # no SE value in output--why

#another way to get CI from SE
confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(lwr=Estimate-mult*Std.err,
                      upr=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}
### glmer----
comp2mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_exp)
summary(comp2mm)
fixef(comp2mm)
exp(fixef(comp2mm))
exp(confint(comp2mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp2mm, method = c("Wald")))

##PP----
hhmemb_exp_pp <- hhmemb %>% filter(perprot_h10==1)
addmargins(table(hhmemb_exp_pp$directoff, hhmemb_exp_pp$i27a_rdt_result_f))

comp2_gee_pp <- geeglm(i27a_rdt_result ~ as.factor(directoff), id=hrhhid, data=hhmemb_exp_pp, family=binomial)
exp(comp2_gee_pp$coefficients)

#Comparison 3: odds of infection in unexposed households comparing offspring vs non offspring-----
hhmemb_unexp <- hhmemb %>% filter(h10_hbv_rdt==0)

addmargins(table(hhmemb_unexp$directoff, hhmemb_unexp$i27a_rdt_result_f))
inddata1 %>% group_by(h10_hbv_rdt, perprot_h10,i27a_rdt_result_f, directoff ) %>% count()



##ITT--------
###GEE------
comp3_gee <- geeglm(i27a_rdt_result ~ as.factor(directoff), id=hrhhid, data=hhmemb_unexp, family=binomial)
summary(comp3_gee$std.err)
exp(comp3_gee$coefficients)
(tidy(comp3_gee, conf.int = T)) # no SE value in output--why


###glmer-----
comp3mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_unexp)
summary(comp3mm)
fixef(comp3mm)
exp(fixef(comp3mm))
exp(confint(comp3mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp3mm, method = c("Wald"), boot.type=c("basic")))

##PP----
hhmemb_unexp_pp <- hhmemb %>% filter(perprot_h10==0)



#Comparison 4: odds of infection in exposed direct off vs unexposed direct off-----
directoff <- hhmemb %>% filter(directoff==1)
##ITT--------
###GEE------
comp4_gee <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=directoff, family=binomial)
summary(comp4_gee$std.err)
exp(comp4_gee$coefficients)
(tidy(comp4_gee, conf.int = T)) # no SE value in output--why

###glmer-----
comp4mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=directoff)
summary(comp4mm)
fixef(comp4mm)
exp(fixef(comp4mm))
exp(confint(comp4mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp4mm, method = c("Wald")))

#Comparison 5: odds of infection in exposed other mem vs unexposed other mem------
othermemb <- hhmemb %>% filter(directoff==0)
##ITT--------
###GEE------
comp5_gee <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=othermemb, family=binomial)
summary(comp5_gee$std.err)
exp(comp5_gee$coefficients)
(tidy(comp5_gee, conf.int = T)) # no SE value in output--why

###glmer-----
comp5mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=othermemb)
summary(comp5mm)
fixef(comp5mm)
exp(fixef(comp5mm))
exp(confint(comp5mm, method = c("boot"), boot.type=c("basic")))
exp(confint(comp5mm, method = c("Wald")))



##Kim's suggestion--interaction term--------
comp2_int <- glmer(i27a_rdt_result ~ h10_hbv_rdt +directoff+h10_hbv_rdt*directoff +(1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss)
summary(comp2_int)
fixef(comp2_int)
exp(fixef(comp2_int))


#Risk factor identification---------------
# run for: moms; directoff; othermemb or othermember
# moms - no hh clustering
# DO and other members- still clustering

vars_mi <- c('age_combined', "maritalrisk","i6_comb_yr","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
              "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
             "i17_tattoo_f", "i16_traditional_scarring_f", "wealth_R",
             'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', 'debutsex_all','debutsex_miss',"debutsex_cat","part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")

vars_do <-  c('age_combined', "hr4_sex_f","cpshbvprox_rev","i6_comb_yr","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
                            "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
                            "i17_tattoo_f", "i16_traditional_scarring_f", "wealth_R")

vars_oth <- c('age_combined',"hr4_sex_f", "cpshbvprox_rev","i6_comb_yr","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
                             "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
                             "i17_tattoo_f", "i16_traditional_scarring_f", "wealth_R",
                             'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', 'debutsex_all','debutsex_miss',"debutsex_cat",
                              "part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")

dfs <- c("moms", "directoff", "othermember")

# function for enrollment RDT result model
enr_model <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}

# function for ITT for index mothers
itt_model <- function(var){ # glm function
  m <- glm(as.formula(paste0('h10_hbv_rdt ~', var)), data=moms, family=binomial("logit"))
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}


# run
options(scipen = 999) # remove scientific notation

#moms only - not clustered
library(broom)
# enrollment HBV test (per protocol)
glmresults_mi_enr <- map_dfr(vars_mi, enr_model) 
glmresults_mi_enr %>% print(noSpaces=T) 

colnames(glmresults_mi_enr) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')

# save df to add to table 3
library(writexl)
write_xlsx(glmresults_mi_enr,"glmresults_mi_enr.xlsx")

glmresults_mi_enr <- glmresults_mi_enr %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_mi_enr <- glmresults_mi_enr %>% mutate_if(is.numeric, round, digits=3)


# order by estimate, label numerically
glmresults_mi_enr <- glmresults_mi_enr %>% arrange(estimate)
 glmresults_mi_enr$ID <- row_number(rev(glmresults_mi_enr$estimate))
##glmresults_mi %>% print(noSpaces=T) 
 
# plot index mother risk factors by enrollment status
glmresults_mi_enr %>% filter(UCI < 50) %>% # refuse to answer sex hx has really large CIs
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black",size=0.2) + #show.legend=F,  , fatten=0.2
  geom_point(shape=15, size=3, aes(color=term), show.legend=F, alpha=0.6) + #
  coord_flip() + theme_bw() + 
  #ylim(0, 10)+
  #scale_x_continuous(breaks=glmresults_mi_enr$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank())+
  ggtitle("A. Index mothers, OR of HBV by recruitment HBV status")


# recruitment HBV test (ITT)
glmresults_mi <- map_dfr(vars_mi,itt_model) 
glmresults_mi %>% print(noSpaces=T) 
colnames(glmresults_mi) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
glmresults_mi <- glmresults_mi %>% mutate_if(is.numeric, round, digits=3)
glmresults_mi$group <- "Index mother"
# save df to add to table
library(writexl)
write_xlsx(glmresults_mi,"glmresults_mi.xlsx")
#only plot - move to plot filter
#glmresults_mi <- glmresults_mi %>% filter(!(is.na(LCI) | is.na(UCI)))

# plot index mother risk factors by enrollment status
glmresults_mi %>% filter(UCI < 50 & !(is.na(LCI) & !(is.na(UCI)))) %>% # refuse to answer sex hx has really large CIs
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black",size=0.2) + #show.legend=F,  , fatten=0.2
  geom_point(shape=15, size=3, aes(color=term), show.legend=F, alpha=0.6) + #
  coord_flip() + theme_bw() + 
  #ylim(0, 10)+
  #scale_x_continuous(breaks=glmresults_mi_enr$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank())+
  ggtitle("B. Index mothers, OR of HBV by enrollment HBV status")


###
##Direct offspring--------
vars_do <-  c('age_combined', "hr4_sex_f","cpshbvprox_rev","i6_comb_yr","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
              "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
              "i17_tattoo_f", "i16_traditional_scarring_f", "wealth_R")

# factor var only: tab3_vars_cat
# need to remove those not on DO dataset
vars_do_cat <-  c( "hr4_sex_f","cpshbvprox_rev","i7_diabetes_f",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
              "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
              "i17_tattoo_f", "i16_traditional_scarring_f", "wealth_R")
vars_do_cat_orig <-  c( "hr4_sex","cpshbvprox_rev","i7_diabetes",'i8_transfusion', "i10_street_salon","i11_manucure",
                   "i14_shared_razor","i15_shared_nailclippers","i13_shared_toothbrush",'i12_food_first_chew',
                   "i17_tattoo", "i16_traditional_scarring", "wealth_R")

# offspring and other members are clustered - use geeglm()
comp5_gee <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=othermemb, family=binomial)
summary(comp5_gee)

itt_gee_do <- function(var){ # trying geeglm function
  m <- geeglm(as.formula(paste0('i27a_rdt_result ~', var)),id=hrhhid, data=directoff, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}
# neither are working
glmresults_do <- map_dfr(vars_do_cat,itt_gee_do) 
glmresults_do <- map_dfr(vars_do_cat_orig,itt_gee_do) 


# using GLM for prelim results instead
itt_glm_do <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~', var)), data=directoff, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}

glmresults_do <- map_dfr(vars_do,itt_glm_do) 
glmresults_do %>% print(noSpaces=T) 
colnames(glmresults_do) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
#glmresults_do <- glmresults_do %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_do <- glmresults_do %>% mutate_if(is.numeric, round, digits=3)
glmresults_do$group <- "Direct offspring"
write_xlsx(glmresults_do,"glmresults_do.xlsx")

glmresults_do %>% filter(UCI < 100 & !(is.na(LCI) & !(is.na(UCI)))) %>% # refuse to answer sex hx has really large CIs
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black",  fatten=0.2) + #show.legend=F, size=0.8,
  geom_point(shape=15, size=5, aes(color=term), show.legend=F, alpha=0.7) + 
  coord_flip() + theme_bw() + 
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  ggtitle("C. Direct offspring, OR of HBV by enrollment HBV status")


## other household members--------
itt_model_oth <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~', var)), data=othermember, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}

glmresults_oth <- map_dfr(vars_oth,itt_model_oth) 
glmresults_oth %>% print(noSpaces=T) 
colnames(glmresults_oth) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
#glmresults_oth <- glmresults_oth %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_oth <- glmresults_oth %>% mutate_if(is.numeric, round, digits=3)
glmresults_oth$group <- "Other household members"
write_xlsx(glmresults_oth,"glmresults_oth.xlsx")

glmresults_oth %>% filter(UCI < 100 & !(is.na(LCI) & !(is.na(UCI)))) %>% # refuse to answer sex hx has really large CIs
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black",  fatten=0.2) + #show.legend=F, size=0.8,
  geom_point(shape=15, size=5, aes(color=term), show.legend=F, alpha=0.7) + 
  coord_flip() + theme_bw() + 
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  ggtitle("D. Other household members, OR of HBV by enrollment HBV status")

# combine model results of all subgroups
glm_all <- rbind(glmresults_mi, glmresults_do, glmresults_oth)

# rename each risk factor
glm_all$term[glm_all$term == "age_combined"] <- "Age (linear)"
glm_all$term[glm_all$term == "maritalrisk1"] <- "Never married"
glm_all$term[glm_all$term == "maritalrisk2"] <- "Divorced, separated, widowed"
glm_all$term[glm_all$term == "i6_comb_yr"] <- "Years living in household (linear)"
glm_all$term[glm_all$term == "i7_diabetes_fYes"] <- "Reports diabetes"
glm_all$term[glm_all$term == "i7_diabetes_fRefused"] <- "Refuses diabetes question"
glm_all$term[glm_all$term == "i8_transfusion_fYes"] <- "Has received transfusion"
glm_all$term[glm_all$term == "i10_street_salon_fYes"] <- "Uses street salons"
glm_all$term[glm_all$term == "i11_manucure_fYes"] <- "Manicures outside home"
glm_all$term[glm_all$term == "i14_shared_razorYes"] <- "Shares razors in house"
glm_all$term[glm_all$term == "i15_shared_nailclippers_fYes"] <- "Shares nail clippers in house"
glm_all$term[glm_all$term == "i13_shared_toothbrush_fYes"] <- "Shares toothbrushes in house"
glm_all$term[glm_all$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
glm_all$term[glm_all$term == "i17_tattoo_fYes"] <- "Has tattoos"
glm_all$term[glm_all$term == "i7_diabetes_fYes"] <- "Reports diabetes"
glm_all$term[glm_all$term == "i16_traditional_scarring_fYes"] <- "Received traditional scars"
glm_all$term[glm_all$term == "i25_sex_hx_receive_money_fYes"] <- "Received money for sex"
glm_all$term[glm_all$term == "i26_sex_hx_given_money_fYes"] <- "Given money for sex"
glm_all$term[glm_all$term == "i26_sex_hx_given_money_fRefused"] <- "Refused to answer given money for sex"
glm_all$term[glm_all$term == "debutsex_cat1"] <- "Sexual debut before 18 yrs"
glm_all$term[glm_all$term == "debutsex_cat2"] <- "Refused to answer age of sexual debut"
glm_all$term[glm_all$term == "part3mo_cat1"] <- "More than 1 sexual partner in last 3 months"
glm_all$term[glm_all$term == "part3mo_cat2"] <- "Refused/DK num of sexual partners in last 3mo"
glm_all$term[glm_all$term == "partnew3mo_cat1"] <- "More than 1 new sexual partner in last 3 months"
glm_all$term[glm_all$term == "part12mo_cat1"] <- "More than 1 sexual partner in last year"
glm_all$term[glm_all$term == "part12mo_cat2"] <- "Refused/DK num of partners in last year"


# plot all
glm_all %>% filter(UCI < 100 & !(is.na(LCI) & !(is.na(UCI)))) %>% # refuse to answer sex hx has really large CIs
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI, color=group), shape=15,  color="black", position=position_dodge2(width=1.0), fatten=0.1) + #show.legend=F, size=0.8,
  geom_point(shape=15, size=3, aes(color=group), position=position_dodge(width = 1.0) ,alpha=0.7) + 
  #scale_color_manual(values=c("#999999", "#E69F00"))+
  coord_flip() + theme_bw() + 
  scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  ggtitle("OR of HBV by enrollment HBV status")



# add wealth
wealth <- geeglm(i27a_rdt_result ~ as.factor(wealth_R), id=hrhhid, data=directoff, family=binomial(link="logit"))
wealth <- glm(i27a_rdt_result ~ as.factor(wealth_R),  data=othermember, family=binomial(link="logit"))
exp(coef(wealth))
exp(confint(wealth))
table(directoff$wealth_R, directoff$i27a_rdt_result_f)

# TO-DO
# cluster model for DO and hhmemb
# decide on which wide CIs to include for each group
# order by estimate
table(directoff$i14_shared_razor_f, directoff$i27a_rdt_result_f, directoff$h10_hbv_rdt_f)


## INLA explore-----------------------
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library()
