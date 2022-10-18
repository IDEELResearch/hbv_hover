# HOVER paper investigate

# english version
hhdata2 <- hhdata2 %>% 
  dplyr::mutate(recruited_f=factor(
    hhdata2$recruited, 
    levels = c("Nouveau screening", "Étude précédente" ),
    labels = c("2022 screening", "2018-19 screening")))
#labels = c("Negative", "Positive")))

table(hhdata2$h10_hbv_rdt_f, hhdata2$recruited_f)
table(hhdata2$modernhousing, hhdata2$recruited_f)

# individuals
inddata1 <- left_join(inddata1, hhdata2[, c("hrhhid", "recruited_f")], by = "hrhhid")
table(inddata1$h10_hbv_rdt_f, inddata1$recruited_f)

inddata1 %>% filter(recruited_f == "2022 screening") %>%  median(as.numeric(age_combined))


