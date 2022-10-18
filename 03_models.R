## analyze hover data
library(lme4)
library(lmtest)
library(geepack)
## Comparison 1: odds of infection of household members in exposed vs unexposed 
hhmemb <- inddata1 %>% filter(indexmom=="Household member")
#exclude those with missing results
hhmemb_nomiss <- hhmemb %>% filter(i27a_rdt_result==0 | i27a_rdt_result==1)

# basic model not accounting for clustering
comp1 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=hhmemb_nomiss)
summary(comp1)
exp(comp1$coefficients)
exp(confint(comp1))

# account for clustering
# multi-level
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

# GEE
comp1gee_ind <- geeglm(i27a_rdt_result ~ as.factor(h10_hbv_rdt), id=hrhhid, data=hhmemb_nomiss, family=binomial, corstr="ind")
summary(comp1gee_ind)
exp(comp1gee_ind$coefficients)
(tidy(comp1gee_ind, conf.int = T)) # not working

## Comparison 2: odds of infection in exposed households comparing offspring vs non offspring
hhmemb_nomiss %>% group_by(h10_hbv_rdt,directoff, i27a_rdt_result)%>%  count()
# Kim suggestion--interaction term
comp2_int <- glmer(i27a_rdt_result ~ h10_hbv_rdt +directoff+h10_hbv_rdt*directoff +(1 | hrhhid), family = binomial(link = "log"), data=hhmemb_nomiss)
summary(comp2_int)
fixef(comp2_int)
exp(fixef(comp2_int))

# before Kim's suggestion
hhmemb_exp <- hhmemb %>% filter(h10_hbv_rdt==1)

comp2 <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "logit"), data=hhmemb_exp)
summary(comp2)
exp(comp2$coefficients)
exp(confint(comp2))

comp2mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_exp)
summary(comp2mm)
fixef(comp2mm)
exp(fixef(comp2mm))
exp(confint(comp2mm, method = c("boot"), boot.type=c("basic")))

## Comparison 3: odds of infection in unexposed households comparing offspring vs non offspring
hhmemb_unexp <- hhmemb %>% filter(h10_hbv_rdt==0)

comp3 <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "logit"), data=hhmemb_unexp)
summary(comp3)
exp(comp3$coefficients)
exp(confint(comp3))

comp3mm <- glmer(i27a_rdt_result ~ directoff + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb_unexp)
summary(comp3mm)
fixef(comp3mm)
exp(fixef(comp3mm))
exp(confint(comp3mm, method = c("boot"), boot.type=c("basic")))

## Comparison 4: odds of infection in exposed direct off vs unexposed direct off
directoff <- hhmemb %>% filter(directoff==1)

comp4 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=directoff)
summary(comp4)
exp(comp4$coefficients)
exp(confint(comp4))

comp4mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=directoff)
summary(comp4mm)
fixef(comp4mm)
exp(fixef(comp4mm))
exp(confint(comp4mm, method = c("boot"), boot.type=c("basic")))

## Comparison 5: odds of infection in exposed other mem vs unexposed other mem
othermemb <- hhmemb %>% filter(directoff==0)

comp5 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=othermemb)
summary(comp5)
exp(comp5$coefficients)
exp(confint(comp5))



## Risk factor identification---------------
hhmemb_unexp <- hhmemb %>% filter(h10_hbv_rdt==0)

table(directoff$i12_food_first_chew_f)

premast <- glm(i27a_rdt_result ~ i12_food_first_chew_f, family = binomial(link = "logit"), data=directoff)
summary(premast)
exp(premast$coefficients)
exp(confint(premast))

## INLA explore-----------------------
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library()
