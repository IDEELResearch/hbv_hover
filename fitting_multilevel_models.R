## Fitting multi-level models
## January 23, 2022
## KB
## Based on Chapter 3 & 4 of Multilevel Modeling in R

library(lme4)
library(tidyverse)

# The formula to follow for basic multilevel models:
#   lmer(formula, data,
#        REML = TRUE, control = lmerControl(), start = NULL,
#        verbose = 0L, subset, weights, na.action, offset, 
#        contrasts = NULL, devFunOnly = FALSE, ...)
#        

# (1) Intercept only MLMs
ach <- read_csv("Achieve.csv")
mod3.0 <- lmer(geread ~ 1 + (1 | school), data = ach)
  # Predict general reading achievement on null model (no independent var)
  # Intercept only - random, varies by school
  # Estimates the residual and intercept variance
summary(mod3.0)

mod3.1 <- lmer(geread ~ gevocab + (1 | school), data = ach)
  # Predict reading with independent variable covab
  # Random intercept only, by school
summary(mod3.1)

mod3.2 <- lmer(geread ~ gevocab + senroll + (1 | school), data = ach)
  # Adds to Mod 3.1 to determine whether size of school has an effect on 
    # an individual student's reading score.
  # senroll (school size) is a Level 2 predictor but only included in the 
    # fixed effects section. The resulting estimate is a fixed (average) 
    # effect and does not vary by school.
summary(mod3.2)

mod3.3 <- lmer(geread ~ gevocab + age + gevocab:age + (1 | school), data = ach)
  # Interaction between age and gevocab (level 1 interaction)
summary(mod3.3)

mod3.4 <- lmer(geread ~ gevocab + senroll + gevocab:senroll + (1 | school), data = ach)
  # Cross level interaction between gevocab (level 1) and school size (level 2)
summary(mod3.4)

x <- mean(ach$gevocab)
ach <- ach %>% 
  mutate(gevocab_gm = gevocab - x)
mod3.5 <- lmer(geread ~ gevocab_gm + (gevocab_gm | school), data = ach)
  # Needed to grand mean center gevocab
  # Random intercept is implied with random slope; need to add -1 to syntax
    # if we do not want a random intercept
summary(mod3.5)

## Two random slopes in one model
## First need to grand mean center 'age'
x <- mean(ach$age)
ach <- ach %>% 
  mutate(age_gm = age - x)

mod3.6 <- lmer(geread ~ gevocab_gm + age_gm + (gevocab_gm + age_gm | school), data = ach)
  # The random effects of gevocab and age are allowed to be correlated by school
  # Using both scaled gevocab and age values (grand mean for both)
summary(mod3.6)

mod3.7 <- lmer(geread ~ gevocab_gm + age_gm + (gevocab_gm|school) + (age_gm|school),
               data = ach)
  # The random effects of gevocab and age are not allowed to be correlated
  # Using both scaled gevocab and age values (grand mean for both)
summary(mod3.7)

mod3.7a <- lmer(geread ~ gevocab_gm + age_gm + (1|school) +
                  (-1 + gevocab_gm|school) + (-1 + age_gm|school), data = ach)
  # Uncorrelated random slopes without separate estimated random intercepts for each
summary(mod3.7a)

mod3.3a <- lmer(geread ~ gevocab_gm + age_gm + gevocab_gm:age_gm + (1|school), data= ach)
summary(mod3.3a)

mod3.8a <- lmer(geread ~ gevocab + (1|school), data=ach, REML=FALSE)
  # To use Maximum Likelihood Estimation
summary(mod3.8a)


## There may be issues with convergence in MLMs. Solutions:
## Change iteration limits
## Change model optimzer
## 
## Example: lmerControl(optimizer = "optimx", optCtrl = list(maxiter=100))

mod3.8 <- lmer(geread ~ gevocab + (1|school),data=ach,REML=FALSE,
               control = lmerControl(optCtrl = list(maxiter = 2000)))
summary(mod3.8)


#### Comparing model fit using anova() from lme4
anova(mod3.0,mod3.1)

#### Confidence intervals of estimates
#### Model 3.5: geread ~ gevocab_gm + (gevocab_gm | school)
confint(mod3.5, method = c("boot"), boot.type=c("perc"))
confint(mod3.5, method = c("boot"), boot.type=c("basic"))
confint(mod3.5, method = c("boot"), boot.type=c("norm"))
confint(mod3.5, method = c("Wald"))
confint(mod3.5, method = c("profile"))

##########################################################################
# Three-level and higher models

library(lme4)
library(tidyverse)
ach <- read_csv("Achieve.csv")


mod4.0 <- lmer(geread ~ 1 + (1|school/class), data = ach)
  # Null, 3-level model. Syntax for the higher models is A/B
  # Where A is higher than B (school vs classroom for example)
  # Null = Random intercept only
summary(mod4.0)


mod4.1 <- lmer(geread ~ gevocab + clenroll + cenroll + (1|school/class), data = ach)
  # Indpendent variables, random intercept
summary(mod4.1)

# test for fit
anova(mod4.0,mod4.1)

mod4.2 <- lmer(geread ~ gevocab + clenroll + cenroll + gevocab:cenroll +
                 (1|school/class), data=ach)
  # Cross level interaction of independent variables
summary(mod4.2)

mod4.3 <- lmer(geread ~ 1 + (1|corp/school/class), data=ach)
  # Four level model
summary(mod4.3)

mod4.4 <- lmer(geread ~ gevocab + gender + (gender|school/class), data=ach)
  # Random effect that varies across 3 levels - gender
summary(mod4.4)

mod4.5 <- lmer(geread ~ gevocab + gender + (1|school) + (gender|class),
               data = ach)
  # Random effect at only one level - classrooms
  # Random intercept at both levels
summary(mod4.5)
