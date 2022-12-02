#Analysis sur la connaissance de l'hepatite B (Knowledge analysis) -------------------

# Install packages-------------
# install.packages("tidyverse")
# install.packages("tableone")
# 
library(tidyverse)
library(tableone)
library(readr)
library(lubridate)

# Load data-------------
ind_con <- readRDS('./ind_con.rds')
# remove names and GPS

ind_con <- inddata1 %>% select(-c(hrname_last,hrname_post,hrname_first,hxcoord, hycoord))
ind_con$hr3_relationship_fr

# ind_con <- ind_con %>% 
#  dplyr::select("hrhhid", "maternity","participant_code", "h10_hbv_rdt","h10_hbv_rdt_f","hr3_relationship_f",contains("hbv_percep"),)

