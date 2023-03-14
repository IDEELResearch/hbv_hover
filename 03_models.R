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

vars_mi <- c("maritalrisk",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
              "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
              "i16_traditional_scarring_f", 
             'i25_sex_hx_receive_money_f', "debutsex_cat","part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")
# removed: "i6_comb_yr", "i7_diabetes_f","i17_tattoo_f",'i26_sex_hx_given_money_f','debutsex_all',
# slimmed for CROI: "wealth_R",'age_combined','debutsex_miss',
# separate hh and comm
vars_mi_hh_croi <- c("maritalrisk", "i14_shared_razor_f","i15_shared_nailclippers_f") #"i16_traditional_scarring_f" 
vars_mi_com_croi <- c('i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
                'i25_sex_hx_receive_money_f', "debutsex_cat","part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")


vars_do <-  c('age_combined', "hr4_sex_f","cpshbvprox_rev",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
                            "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
                             "i16_traditional_scarring_f", "wealth_R")
# removed: "i6_comb_yr","i7_diabetes_f","i17_tattoo_f",

vars_oth <- c('age_combined',"hr4_sex_f", "cpshbvprox_rev",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
                             "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
                            "i16_traditional_scarring_f", "wealth_R",
                             'i25_sex_hx_receive_money_f','i26_sex_hx_given_money_f', "debutsex_cat",
                              "part3mo_cat","partnew3mo_cat","part12mo_cat","partnew12mo_cat")
# removed: "i6_comb_yr","i7_diabetes_f", "i17_tattoo_f", 'debutsex_all','debutsex_miss',
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
# household exposures
glmresults_mi_enr <- map_dfr(vars_mi_hh_croi, enr_model) 
glmresults_mi_enr %>% print(noSpaces=T) 
colnames(glmresults_mi_enr) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')

glmresults_mi_enr <- glmresults_mi_enr %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_mi_enr <- glmresults_mi_enr %>% mutate_if(is.numeric, round, digits=3)

# order by estimate, label numerically
glmresults_mi_enr <- glmresults_mi_enr %>% arrange(estimate)
glmresults_mi_enr$ID <- row_number(rev(glmresults_mi_enr$estimate))
glmresults_mi %>% print(noSpaces=T) 

# community exposures
glmresults_mi_enr_comm <- map_dfr(vars_mi_com_croi, enr_model) 
glmresults_mi_enr_comm %>% print(noSpaces=T) 
colnames(glmresults_mi_enr_comm) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')

glmresults_mi_enr_comm <- glmresults_mi_enr_comm %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_mi_enr_comm <- glmresults_mi_enr_comm %>% mutate_if(is.numeric, round, digits=3)

# order by estimate, label numerically
glmresults_mi_enr_comm <- glmresults_mi_enr_comm %>% arrange(estimate)
glmresults_mi_enr_comm$ID <- row_number(rev(glmresults_mi_enr_comm$estimate))
glmresults_mi_enr_comm %>% print(noSpaces=T) 


# save df to add to table 3
library(writexl)
write_xlsx(glmresults_mi_enr,"glmresults_mi_enr.xlsx")

require(scales)
# plot index mother risk factors by enrollment status
glmresults_mi_enr %>% #filter(UCI < 50) %>% # refuse to answer sex hx has really large CIs
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black",size=0.2) + #show.legend=F,  , fatten=0.2
  geom_point(shape=15, size=3, aes(color=term), show.legend=F, alpha=0.6) + #
  coord_flip() + theme_bw() + 
  #ylim(0, 10)+
  #scale_x_continuous(breaks=glmresults_mi_enr$ID,  labels=term, trans = "reverse") + 
  labs(x="Household exposures", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank())+
  ggtitle("A. Index mothers, Crude OR of HBV by recruitment HBV status")
#


# recruitment HBV test (ITT)
glmresults_mi <- map_dfr(vars_mi,itt_model) 
glmresults_mi %>% print(noSpaces=T) 
colnames(glmresults_mi) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
#only plot - move to plot filter
glmresults_mi <- glmresults_mi %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_mi <- glmresults_mi %>% mutate_if(is.numeric, round, digits=3)

# save df to add to table
library(writexl)
write_xlsx(glmresults_mi,"glmresults_mi.xlsx")


# order by estimate, label numerically
glmresults_mi <- glmresults_mi %>% arrange(estimate)
glmresults_mi$ID <- row_number(rev(glmresults_mi$estimate))
glmresults_mi %>% print(noSpaces=T) 

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

# combine index mother figure
glmresults_mi_enr$subset <- "Index mothers: household"
glmresults_mi_enr_comm$subset <- "Index mothers: community"

glm_mother_div <- rbind(glmresults_mi_enr, glmresults_mi_enr_comm)

# rename each risk factor
glm_mother_div$term[glm_mother_div$term == "maritalrisk1"] <- "Never married vs married"
glm_mother_div$term[glm_mother_div$term == "maritalrisk2"] <- "Divorced, separated, widowed vs married"
glm_mother_div$term[glm_mother_div$term == "i8_transfusion_fOui"] <- "Past transfusion vs never"
glm_mother_div$term[glm_mother_div$term == "i10_street_salon_fOui"] <- "Uses street salons vs not"
glm_mother_div$term[glm_mother_div$term == "i11_manucure_fOui"] <- "Manicures outside home vs not"
glm_mother_div$term[glm_mother_div$term == "i14_shared_razor_fOui"] <- "Shares razors in house vs not"
glm_mother_div$term[glm_mother_div$term == "i15_shared_nailclippers_fOui"] <- "Shares nail clippers in house vs not"
glm_mother_div$term[glm_mother_div$term == "i25_sex_hx_receive_money_fOui"] <- "Received money for sex vs never"
glm_mother_div$term[glm_mother_div$term == "i26_sex_hx_given_money_fOui"] <- "Given money for sex vs never"
glm_mother_div$term[glm_mother_div$term == "i26_sex_hx_given_money_fRefused"] <- "Refused to answer given money for sex vs never"
glm_mother_div$term[glm_mother_div$term == "debutsex_cat1"] <- "Sexual debut <18 yrs vs ≥ 18"
glm_mother_div$term[glm_mother_div$term == "debutsex_cat2"] <- "Refused to answer age of sexual debut vs ≥ 18 yrs"
glm_mother_div$term[glm_mother_div$term == "part3mo_cat1"] <- "≥1 sexual partner in last 3 months vs ≤ 1"
glm_mother_div$term[glm_mother_div$term == "part3mo_cat2"] <- "Refused: # sexual partners in last 3mo vs ≤ 1"
glm_mother_div$term[glm_mother_div$term == "partnew3mo_cat1"] <- "≥1 new sexual partner in last 3 months vs no new"
glm_mother_div$term[glm_mother_div$term == "partnew12mo_cat1"] <- "≥1 new sexual partner in last 12 months vs no new"
glm_mother_div$term[glm_mother_div$term == "part12mo_cat1"] <- "≥1 sexual partner in last year vs no new"
glm_mother_div$term[glm_mother_div$term == "part12mo_cat2"] <- "Refused: # sexual partners in 12 months vs ≤ 1"

glm_mother_div$term[glm_mother_div$term == "Refused: # sexual partners in last 3mo vs ≤ 1"] <- " Refused: # sexual partners in last 3mo vs ≤ 1"
glm_mother_div$term[glm_mother_div$term == "Sexual debut <18 yrs vs ≥ 18"] <- "  Sexual debut <18 yrs vs ≥ 18"
glm_mother_div$term[glm_mother_div$term == "Refused to answer age of sexual debut vs ≥ 18 yrs"] <- "   Refused to answer age of sexual debut vs ≥ 18 yrs"


# plot both timepoints for mother


glm_mother_div %>% filter(UCI < 30 & subset == "Index mothers: household") %>% # & !(is.na(LCI) & !(is.na(UCI))): refuse to answer sex hx has really large CIs
  filter(term != "i13_shared_toothbrush_fOui" & term != "i12_food_first_chew_fOui" & term != "i16_traditional_scarring_fOui" &
           term != "maritalrisk2" ) %>% 
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=estimate), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#020E54"))+ #deeppink3
  coord_flip() + #theme_bw() + 
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  ylim(0, 30)+
  labs(x="Household exposures", y="Crude Odds ratio of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank()) +
  ggtitle("Crude OR of HBV by enrollment HBV status")

ggsave('./plots/croi_1a.png', width=15, height=2.5)


# comm exposures
glm_mother_div %>% filter(UCI < 30 & subset == "Index mothers: community") %>% # & !(is.na(LCI) & !(is.na(UCI))): refuse to answer sex hx has really large CIs
  filter(term != "i13_shared_toothbrush_fOui" & term != "i12_food_first_chew_fOui" & term != "i16_traditional_scarring_fOui" &
           term != "maritalrisk2"& term != "≥1 new sexual partner in last 12 months vs no new") %>% 
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=estimate, color=subset), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#020E54"))+ #deeppink3
  coord_flip() + #theme_bw() + 
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  ylim(0, 30)+
  labs(x="Community exposures", y="Crude Odds ratio of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 10),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank()) +
  ggtitle("Crude OR of HBV by enrollment HBV status")

ggsave('./plots/croi_1b.png', width=15, height=4)

#combine mothers
glm_mother_div$group <- "Index mothers"

glm_mother_div %>% filter(UCI < 30) %>% # & !(is.na(LCI) & !(is.na(UCI))): refuse to answer sex hx has really large CIs
  filter(term != "i13_shared_toothbrush_fOui" & term != "i12_food_first_chew_fOui" & term != "i16_traditional_scarring_fOui" &
           term != "maritalrisk2"& term != "≥1 new sexual partner in last 12 months vs no new" &
           term != "Divorced, separated, widowed vs married" & term !="Received money for sex vs never") %>% 
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="grey2", position=position_dodge2(width=1.0),size=0.8, fatten=0.1) + #show.legend=F,  color=timepoint
  geom_point(shape=15, size=7, aes(x=term, y=estimate, color=group), position=position_dodge2(width = 1.0) ,alpha=0.9) + 
  scale_color_manual(values=c("#020E54"))+ #deeppink3
  coord_flip() + #theme_bw() + 
  #scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  ylim(0, 30)+
  labs(x="Community exposures", y="Crude Odds ratio of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.background = element_blank()) +
  ggtitle("Crude OR of HBV by enrollment HBV status")

ggsave('./plots/croi_1.png', width=15, height=5)


###
glmresults_mi$group <- "Index mother"

##Direct offspring--------
vars_do <-  c('age_combined', "hr4_sex_f","cpshbvprox_rev",'i8_transfusion_f', "i10_street_salon_f","i11_manucure_f",
              "i14_shared_razor_f","i15_shared_nailclippers_f","i13_shared_toothbrush_f",'i12_food_first_chew_f',
              "i17_tattoo_f", "i16_traditional_scarring_f", "wealth_R")
# removed: "i6_comb_yr","i7_diabetes_f",

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

itt_gee_do <- function(var){ # trying geeglm function; replaex in function a variable and see if it works; dd return in funxtion to see output
  m <- geeglm(as.formula(paste0('i27a_rdt_result ~', var)),id=hrhhid, data=directoff, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}
# neither are working
glmresults_do <- map_dfr(vars_do_cat,itt_gee_do) 
glmresults_do <- map_dfr(vars_do_cat_orig,itt_gee_do) 

# using GLM for prelim results instead
itt_glm_do <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~h10_hbv_rdt+', var)), data=directoff, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}

directoff$cpshbvprox_rev <- 2 - directoff$cpshbvprox
directoff$cpshbvprox_rev <- as.factor(directoff$cpshbvprox_rev)

directoff <- left_join(directoff, hhdata2[,c("hrhhid","wealth_R")],  by = "hrhhid")
table(directoff$wealth_R, directoff$i27a_rdt_result_f)

directoff$wealth_R <- as.factor(directoff$wealth_R)

glmresults_do <- map_dfr(vars_do,itt_glm_do) 
glmresults_do %>% print(noSpaces=T) 
colnames(glmresults_do) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
#glmresults_do <- glmresults_do %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_do <- glmresults_do %>% mutate_if(is.numeric, round, digits=3)
glmresults_do$subset <- "Direct offspring"

write_xlsx(glmresults_do,"glmresults_do.xlsx")

table(directoff$i10_street_salon_f, directoff$i27a_rdt_result_f)

glmresults_do$term[glmresults_do$term == "wealth_R3"] <- "Highest wealth quartile vs lowest"
glmresults_do$term[glmresults_do$term == "i8_transfusion_fOui"] <- "Past transfusion vs never"
glmresults_do$term[glmresults_do$term == "i15_shared_nailclippers_fOui"] <- "Share nailclippers vs not"
glmresults_do$term[glmresults_do$term == "i14_shared_razor_fOui"] <- "Share razors vs not"
glmresults_do$term[glmresults_do$term == "i10_street_salon_fOui"] <- "Visits street salons vs not"
glmresults_do$term[glmresults_do$term == "hr4_sex_fFéminin"] <- "Female vs male"
glmresults_do$term[glmresults_do$term == "cpshbvprox_rev2"] <- "HBV vaccination: likely not vs probably"
glmresults_do$term[glmresults_do$term == "cpshbvprox_rev1"] <- "HBV vaccination: possibly vs probably"

glmresults_do$term[glmresults_do$term == "Female vs male"] <- "Female vs male"
glmresults_do$term[glmresults_do$term == "Highest wealth quartile vs lowest"] <- " Highest wealth quartile vs lowest"
glmresults_do$term[glmresults_do$term == "Past transfusion vs never"] <- "  Past transfusion vs never"
glmresults_do$term[glmresults_do$term == "Share razors vs not"] <- "   Share razors vs not" # 3 is max


glmresults_do %>% filter(UCI < 100 & !(is.na(LCI) & !(is.na(UCI)))) %>% # refuse to answer sex hx has really large CIs
  filter(term != "i12_food_first_chew_fOui" & term != "i11_manucure_fOui" & term != "wealth_R2" & term != "wealth_R1" & term != "age_combined" & term != "HBV vaccination: possibly vs probably") %>% 
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black", size=0.8, fatten=0.2) + #show.legend=F, 
  geom_point(shape=15, size=7,  aes(color=subset), show.legend=F, alpha=0.9) +
  scale_color_manual(values = "#A6DEAE")+
  coord_flip() + #theme_bw() + 
  ylim(0,30)+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank()) +
  ggtitle("C. Direct offspring, OR of HBV by enrollment HBV status")

ggsave('./plots/croi_2.png', width=15, height=3.5)


## other household members--------
itt_model_oth <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~h10_hbv_rdt+', var)), data=othermember, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}

# using GLM for prelim results instead
itt_glm_do <- function(var){ # glm function
  m <- glm(as.formula(paste0('i27a_rdt_result ~h10_hbv_rdt+', var)), data=directoff, family=binomial("logit")) #id=hrhhid
  cbind(tidy(m, exponentiate = T), exp(confint(m))) %>% filter(stringr::str_detect(term, var))}



othermember <- left_join(othermember, hhdata2[,c("hrhhid","wealth_R")],  by = "hrhhid")
table(othermember$wealth_R, othermember$i27a_rdt_result_f)

directoff$wealth_R <- as.factor(directoff$wealth_R)

glmresults_oth <- map_dfr(vars_oth,itt_model_oth) 
glmresults_oth %>% print(noSpaces=T) 
colnames(glmresults_oth) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
#glmresults_oth <- glmresults_oth %>% filter(!(is.na(LCI) | is.na(UCI)))
glmresults_oth <- glmresults_oth %>% mutate_if(is.numeric, round, digits=3)
glmresults_oth$subset <- "Other household members"
write_xlsx(glmresults_oth,"glmresults_oth.xlsx")


glmresults_oth$term[glmresults_oth$term == "i8_transfusion_fOui"] <- "Past transfusion vs never"
glmresults_oth$term[glmresults_oth$term == "i10_street_salon_fOui"] <- "Uses street salons vs not"
glmresults_oth$term[glmresults_oth$term == "i11_manucure_fOui"] <- "Manicures outside home vs not"
glmresults_oth$term[glmresults_oth$term == "i14_shared_razor_fOui"] <- "Shares razors in house vs not"
glmresults_oth$term[glmresults_oth$term == "i15_shared_nailclippers_fOui"] <- "Shares nail clippers in house vs not"
glmresults_oth$term[glmresults_oth$term == "i25_sex_hx_receive_money_fOui"] <- "Received money for sex vs never"
glmresults_oth$term[glmresults_oth$term == "i26_sex_hx_given_money_fOui"] <- "Given money for sex vs never"
glmresults_oth$term[glmresults_oth$term == "i26_sex_hx_given_money_fRefused"] <- "Refused to answer given money for sex vs never"
glmresults_oth$term[glmresults_oth$term == "debutsex_cat1"] <- "Sexual debut <18 yrs vs ≥ 18"
glmresults_oth$term[glmresults_oth$term == "debutsex_cat2"] <- "Refused to answer age of sexual debut vs ≥ 18 yrs"
glmresults_oth$term[glmresults_oth$term == "part3mo_cat1"] <- "≥1 sexual partner in last 3 months vs ≤ 1"
glmresults_oth$term[glmresults_oth$term == "part3mo_cat2"] <- "Refused: # sexual partners in last 3mo vs ≤ 1"
glmresults_oth$term[glmresults_oth$term == "partnew3mo_cat1"] <- "≥1 new sexual partner in last 3 months vs no new"
glmresults_oth$term[glmresults_oth$term == "partnew12mo_cat1"] <- "≥1 new sexual partner in last 12 months vs no new"
glmresults_oth$term[glmresults_oth$term == "part12mo_cat1"] <- "≥1 sexual partner in last year vs no new"
glmresults_oth$term[glmresults_oth$term == "part12mo_cat2"] <- "Refused: # sexual partners in 12 months vs ≤ 1"

glmresults_oth$term[glmresults_oth$term == "i16_traditional_scarring_fOui"] <- "Engaged in traditional scarring vs not"
glmresults_oth$term[glmresults_oth$term == "hr4_sex_fFéminin"] <- "Female vs male"


glmresults_oth %>% filter(UCI < 40 & !(is.na(LCI) & !(is.na(UCI)))) %>% # refuse to answer sex hx has really large CIs
  filter(term == "Sexual debut <18 yrs vs ≥ 18" | term == "Engaged in traditional scarring vs not"
         | term == "Past transfusion vs never" | term == "Manicures outside home vs not" | 
           term == "Shares razors in house vs not") %>% 
  ggplot(aes(x=term, y=estimate)) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI), shape=15,  color="black",size=0.8,  fatten=0.2) + #show.legend=F, 
  geom_point(shape=15, size=7, aes(color=subset), show.legend=F, alpha=0.9) + #
  scale_color_manual(values = "#FF700A")+
  coord_flip() + #theme_bw() + 
  ylim(0, 30)+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Crude odds ratio of HBsAg+") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size = 20),
        panel.grid.minor=element_blank(),
        panel.background = element_blank()) +
  ggtitle("D. Other household members, Crude OR of HBV by enrollment HBV status")

ggsave('./plots/croi_3.png', width=15, height=3)

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
glm_all$term[glm_all$term == "i15_shared_nailclippers"] <- "Shares nail clippers in house"
glm_all$term[glm_all$term == "i13_shared_toothbrush_fYes"] <- "Shares toothbrushes in house"
glm_all$term[glm_all$term == "i12_food_first_chew_fYes"] <- "Premasticates food for someone else"
glm_all$term[glm_all$term == "i17_tattoo_fYes"] <- "Has tattoos"
glm_all$term[glm_all$term == "i17_tattoo_fRefused"] <- "Refused to answer tattoos"
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
  geom_pointrange(aes(x=term, y=estimate, ymin=LCI, ymax=UCI, color=group), shape=15,  color="grey2", position=position_dodge2(width=1.0), fatten=0.1) + #show.legend=F, size=0.8,
  geom_point(shape=15, size=3, aes(color=group), position=position_dodge(width = 1.0) ,alpha=0.7) + 
  #scale_color_manual(values=c("#999999", "#E69F00"))+
  coord_flip() + theme_bw() + 
  scale_color_discrete(name = "", labels = c("Index mother", "Direct offspring", "Other household members"))+
  #scale_x_continuous(breaks=glmresults_mi$ID, labels=term, trans = "reverse") + 
  labs(x="Exposure", y="Odds of HBsAg+ compared with referent") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 20),
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
