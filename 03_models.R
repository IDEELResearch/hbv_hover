## analyze hover data
library(lme4)
library(lmtest)
library(geepack)

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
