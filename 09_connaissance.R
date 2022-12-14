#Analysis sur la connaissance de l'hepatite B (Knowledge analysis) -------------------
# Date: 2 December 2022
# Date update: 5 December 2022

# Install packages-------------
# Il suffit d'exécuter une fois "install.packages" pour télécharger sur l'ordinateur.
install.packages("tableone") # répétez pour chaque paquet

# Nécessité d'exécuter "library()" à chaque fois que vous ouvrez R
library(tidyverse)
library(tableone)
library(readr)
library(lubridate)

# Load data-------------
ind_con <- readRDS('./ind_con.rds')

# se concentrer d'abord sur un sous-ensemble de questions de connaissance
ind_con_2 <- ind_con %>% 
  dplyr::select("hrhhid", "maternity","participant_code", "h10_hbv_rdt","h10_hbv_rdt_f","hr3_relationship","hr3_relationship_fr","hhmemcat_f","age_combined","avert_indic","astmh_indic","acq_ind",contains("hbv_percep"),"i27a_rdt_result_f")

# focus on adults - restrict to age ≥18 years
ind_con_2 <- ind_con_2 %>% filter(age_combined >=18)


# analyse de base
# variables clé: 
# i18_hbv_percep_serious: À votre avis, quelle est la gravité de l'hépatite B ? 1 = tres grave, 4 = pas du tout serieux, 98 NSP, 99 refuse
# i19_hbv_percep_sympt*** (1-4): Quels sont les signes et symptômes de l'hépatite B (veuillez cocher tout les réponses qui s'appliquent) ?
# i20_hbv_percep_trans*** (1-6): Comment une personne peut-elle contracter l'hépatite B (veuillez cocher tout les réponses qui s'appliquent) ?
# i20_hbv_percep_mtct_prev*** (1-2): Comment prévenir la transmission de l'hépatite B de la mère à l'enfant (veuillez cocher tout les réponses qui s'appliquent) ?

# pour tous, 98=NSP, 99=refuse

# renommer les variables
# utiliser le livre de codes de la base de données pour déterminer quel numéro correspond à chaque réponse
table(ind_con_2$i18_hbv_percep_serious, useNA = "always")

ind_con_2 <- ind_con_2 %>% 
  dplyr::mutate(i18_hbv_percep_serious_f=factor(
    ind_con_2$i18_hbv_percep_serious, 
    levels = c(1,2,3,4,98,99),
    labels = c("Très grave", "Assez grave","Pas très grave","Pas du tout sérieux","Ne sait pas","Refusé(e)")))

table(ind_con_2$i18_hbv_percep_serious_f, useNA = "always")

ind_con_2 <- ind_con_2 %>% filter(!is.na(i18_hbv_percep_serious)) # one missing - drop

ind_con_2 <- ind_con_2 %>% 
  rename(i19_hbv_percep_sympt_aucun = i19_hbv_percep_sympt___1, # aucun symptome
         i19_hbv_percep_sympt_jaun = i19_hbv_percep_sympt___2, # jaunisse
         i19_hbv_percep_sympt_dolab = i19_hbv_percep_sympt___3, # douleur abdomiale
         i19_hbv_percep_sympt_erup = i19_hbv_percep_sympt___4, # eruption cutanee
         i19_hbv_percep_sympt_nsp = i19_hbv_percep_sympt___98, # NSP
         i19_hbv_percep_sympt_ref = i19_hbv_percep_sympt___99,  # refuse
         
         i20_hbv_percep_trans_main = i20_hbv_percep_trans___1,
         i20_hbv_percep_trans_sang = i20_hbv_percep_trans___2,
         i20_hbv_percep_trans_plat = i20_hbv_percep_trans___3,
         i20_hbv_percep_trans_tme = i20_hbv_percep_trans___4,
         i20_hbv_percep_trans_sex = i20_hbv_percep_trans___5,
         i20_hbv_percep_trans_lait = i20_hbv_percep_trans___6,
         i20_hbv_percep_trans_nsp = i20_hbv_percep_trans___98,
         i20_hbv_percep_trans_ref = i20_hbv_percep_trans___99,
         
         i20_hbv_percep_mtct_prev_vacc = i20_hbv_percep_mtct_prev___1,
         i20_hbv_percep_mtct_prev_arv = i20_hbv_percep_mtct_prev___2,
         i20_hbv_percep_mtct_prev_nsp = i20_hbv_percep_mtct_prev___98,
         i20_hbv_percep_mtct_prev_ref = i20_hbv_percep_mtct_prev___99)

summary(ind_con_2$i18_hbv_percep_serious)
# gravité
# counts
ind_con_2 %>% 
  ggplot(aes(x = fct_infreq(hr3_relationship_fr), fill = i18_hbv_percep_serious_f)) +
  geom_bar() + coord_flip()+
  facet_wrap(vars(), ncol = 3) +
  theme_bw()

# gravite
# pct
ind_con_2 %>%
  count(hr3_relationship_fr, i18_hbv_percep_serious_f) %>%       
  group_by(hr3_relationship_fr) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(hr3_relationship_fr, pct, fill=i18_hbv_percep_serious_f) +
  geom_bar(stat="identity") + coord_flip() +
  ylab("% of Participants") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Gravité du VHB en fonction de la relation avec la mère index") +
  theme_bw()



# symptoms
ind_con_2 %>%
  pivot_longer(i19_hbv_percep_sympt_aucun:i19_hbv_percep_sympt_nsp, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response, fill = hr3_relationship_fr)) +
  geom_bar() +
  #scale_fill_brewer(palette = "paired")+
  #scale_fill_viridis_c(option = 'magma') +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (Yes or No)", y = "Number of respondents")+
  theme_bw()

# transmssion
ind_con_2 %>%
  pivot_longer(i20_hbv_percep_trans_main:i20_hbv_percep_trans_nsp, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response, fill = hr3_relationship_fr)) +
  geom_bar() +
  #scale_fill_brewer(palette = "paired")+
  #scale_fill_viridis_c(option = 'magma') +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (Yes or No)", y = "Number of respondents")+
  theme_bw()

# prevention
ind_con_2 %>%
  pivot_longer(i20_hbv_percep_mtct_prev_vacc:i20_hbv_percep_mtct_prev_nsp, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = as.factor(response), fill = hr3_relationship_fr)) +
  geom_bar() +
  #scale_fill_brewer(palette = "paired")+
  #scale_fill_viridis_c(option = 'magma') +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (Yes or No)", y = "Number of respondents")+
  theme_bw()


# nouveaux variables -----------
# score de connaissance

ind_con_2 = ind_con_2 %>%
  mutate(score = rowSums(across(c(i19_hbv_percep_sympt_aucun, i19_hbv_percep_sympt_jaun,i19_hbv_percep_sympt_dolab, i19_hbv_percep_sympt_erup,
                     i20_hbv_percep_trans_sang, i20_hbv_percep_trans_tme, i20_hbv_percep_trans_sex, 
                     i20_hbv_percep_mtct_prev_vacc,i20_hbv_percep_mtct_prev_arv ))),
         score_sympt = rowSums(across(c(i19_hbv_percep_sympt_aucun, i19_hbv_percep_sympt_jaun,i19_hbv_percep_sympt_dolab))),
         score_trans = rowSums(across(c(i20_hbv_percep_trans_sang, i20_hbv_percep_trans_tme, i20_hbv_percep_trans_sex))),
         score_prev = rowSums(across(c(i20_hbv_percep_mtct_prev_vacc,i20_hbv_percep_mtct_prev_arv))),
         score_sympt_indic = case_when(score_sympt>0 ~ 1, TRUE ~ 0),
         score_trans_indic = case_when(score_trans>0 ~ 1, TRUE ~ 0),
         score_prev_indic = case_when(score_prev>0 ~ 1, TRUE ~ 0),
         
  )
# check new variables
table(ind_con_2$score, ind_con_2$maternity)
table(ind_con_2$score_sympt, ind_con_2$score_sympt_indic, useNA = "always")
table(ind_con_2$score_trans,ind_con_2$score_trans_indic, useNA = "always")
table(ind_con_2$score_prev,ind_con_2$score_prev_indic, useNA = "always")

# score de connaissance 
ind_con_2 %>%
  pivot_longer(score_sympt:score_prev, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = as.factor(response), fill = hr3_relationship_fr)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (Yes or No)", y = "Number of respondents")+
  theme_bw()

# et indicators de connaissance
ind_con_2 %>%
  pivot_longer(score_sympt_indic:score_prev_indic, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = as.factor(response), fill = hr3_relationship_fr)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (Yes or No)", y = "Number of respondents")+
  theme_bw()

ind_con_2 %>%
  filter(hr3_relationship_fr=="Mère index") %>% 
  pivot_longer(score_sympt_indic:score_prev_indic, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = as.factor(response), fill = as.factor(avert_indic))) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (Yes or No)", y = "Number of respondents")+
  theme_bw()

install.packages("tidyverse") # répétez pour chaque paquet

