# code to analyze vaccinations in hover
library(tidyverse)

# print list reported as eligible and compare with those living in a hh with a pos person
table(inddata1$i39_vaccine_eligible, useNA = "always")
table(inddata1$totalpositive, inddata1$i39_vaccine_eligible)

table(inddata1$i27a_rdt_result)
vacc_elig_pos <- inddata1 %>% filter(totalpositive>0 & i27a_rdt_result==0) # must be exposed and negative themselves
table(vacc_elig_pos$i39_vaccine_eligible, vacc_elig_pos$maternity)

vacc_elig_pos %>%  
  ggplot()+
  geom_histogram(aes(x=age_combined, fill=as.factor(i39_vaccine_eligible)), bins = 40)+
  labs(x="Age (in years)", fill="Noted as eligible")+
  scale_fill_manual(values = nounprojgraphcol)+
  ggtitle("Age of eligibles")+
  theme_bw()
  #geom_text(data=NAdf, aes(x=xcoor, y=ycoor, label=paste(num_NA,"for",name))) 
  inddata1$hr4_sex_f

options(max.print=5000)
vacc_elig_pos %>% filter(i39_vaccine_eligible==0 ) %>% summarise(maternity, age_combined, hr4_sex_f, hr3_relationship_f,pid, hrname_first, hrname_post,hrname_last) %>% arrange(maternity,-age_combined)
vacc_elig_pos %>% filter(i39_vaccine_eligible==1 ) %>% summarise(maternity, age_combined, hr4_sex_f,hr3_relationship_f,pid, hrname_first, hrname_post,hrname_last) %>% arrange(maternity,-age_combined)


vacc_elig_pos %>% group_by(maternity,agegrp15_2) %>% count(i39_vaccine_eligible)

