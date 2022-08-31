## analyze hover data
library(lme4)
library(lmtest)
## Comparison 1: odds of infection of household members in exposed vs unexposed 
hhmemb <- inddata1 %>% filter(indexmom=="Membre de ménage")

comp1 <- glm(i27a_rdt_result ~ h10_hbv_rdt, family = binomial(link = "logit"), data=hhmemb)
summary(comp1)
exp(comp1$coefficients)
exp(confint(comp1))

# account for clustering

comp1mm <- glmer(i27a_rdt_result ~ h10_hbv_rdt + (1 | hrhhid), family = binomial(link = "logit"), data=hhmemb)
summary(comp1mm)
exp(fixef(comp1mm))
# NEED TO FIGURE OUT CI FROM ML MODEL
exp(confint((comp1mm)))
lmerTest::con
lrtest(comp1, comp1mm)
library(broom)
tidy(comp1mm, effects="fixed", conf.int=T, confint.method = "boot")



## Comparison 2: odds of infection in exposed households comparing offspring vs non offspring
hhmemb_exp <- hhmemb %>% filter(h10_hbv_rdt==1)

comp2 <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "logit"), data=hhmemb_exp)
summary(comp2)
exp(comp2$coefficients)
exp(confint(comp2))

## Comparison 3: odds of infection in unexposed households comparing offspring vs non offspring
hhmemb_unexp <- hhmemb %>% filter(h10_hbv_rdt==0)

comp3 <- glm(i27a_rdt_result ~ directoff, family = binomial(link = "logit"), data=hhmemb_unexp)
summary(comp3)
exp(comp3$coefficients)
exp(confint(comp3))


