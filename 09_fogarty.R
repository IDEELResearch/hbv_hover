# serology results
library(tidyverse)
library(readxl)

fogarty_fin <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/FOGARTY DATASET.xlsx", sheet = "fogpids")
# HOVER metadata for Fogarty enrollees
fogarty_hoverdata <- inddata1 %>% filter(hrhhid %in% fogarty_fin$hrhhid)
view(fogarty_hoverdata)

# read in serology results-----------
fogsero <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/FOGARTY DATASET.xlsx", sheet = "sero_import")

# date format checks
fogsero$dob <- as.Date(fogsero$dob)
fogsero$fog_sampdate <- as.Date(fogsero$fog_sampdate)
fogsero$avert_enr_date <- as.Date(fogsero$avert_enr_date)
fogsero$avert_delivery_sampdate <- as.Date(fogsero$avert_delivery_sampdate)
fogsero$avert_6mo_date <- as.Date(fogsero$avert_6mo_date)
fogsero$acq_date <- as.Date(fogsero$acq_date)

# save VL numeric value as own variable
fogsero$fog_log_vl_num <- as.numeric(gsub("[^0-9.]+", "", fogsero$vl_log_iuml))
fogsero$fog_log_vl_num <- ifelse(fogsero$vl_log_iuml=="Not detected",0,fogsero$fog_log_vl_num)

print(fogsero$fog_log_vl_num)
class(fogsero$fog_log_vl_num)


# Antibody results-----------------------
# plan: group kids by birth month/year, count # sAb reactive, grayzone, total
# subset kids
fogsero_kids <- fogsero %>% filter(dob >= "2007-02-28")
table(fogsero_kids$hbsab_interpret)
table(fogsero_kids$hbcab_interpret)
table(fogsero_kids$hbsab_interpret, fogsero_kids$hbcab_interpret)

fogsero_kids <- fogsero_kids %>% 
  mutate(immstat = case_when(
    hbsab_interpret == "Reactive" & hbcab_interpret == "Nonreactive" ~ "Immunized",
    hbsab_interpret == "Reactive" & hbcab_interpret == "Reactive" ~ "Recovered",
    hbsab_interpret == "Reactive" & hbcab_interpret == "Grayzone" ~ "Recovered, waning cAb",
    hbsab_interpret == "Grayzone" & hbcab_interpret == "Nonreactive" ~ "Waning vaccine immunity",
    hbsab_interpret == "Nonreactive" & hbcab_interpret == "Reactive" ~ "Recovered, waned sAb",
    hbsab_interpret == "Nonreactive" & hbcab_interpret == "Nonreactive" ~ "Unprotected"),
    immstat_bin = case_when(
      hbsab_interpret == "Reactive" & hbcab_interpret == "Nonreactive" ~ "Immunized (HBsAb+/HBcAb-)",
      hbsab_interpret == "Reactive" & hbcab_interpret == "Reactive" ~ "Recovered (HBsAb+/HBcAb+)",
      hbsab_interpret == "Reactive" & hbcab_interpret == "Grayzone" ~ "Recovered (HBsAb+/HBcAb+)",
      hbsab_interpret == "Grayzone" & hbcab_interpret == "Nonreactive" ~ "Immunized (HBsAb+/HBcAb-)",
      hbsab_interpret == "Nonreactive" & hbcab_interpret == "Reactive" ~ "Recovered, waned sAb (HBsAb-/HBcAb+)",
      hbsab_interpret == "Nonreactive" & hbcab_interpret == "Nonreactive" ~ "Unprotected (HBsAb-/HBcAb-)"))

fogsero_kids <- fogsero_kids %>% 
  mutate(diroff = case_when(
    rel_to_indexmoth == "Son (twin)" | rel_to_indexmoth == "Son" | rel_to_indexmoth =="Daughter" ~ "Direct offspring",
    TRUE ~ "Different mother"))
table(fogsero_kids$diroff)

vacc_counts_do <- 
  fogsero_indexkids %>% 
  mutate(month = format(dob, "%m"), year = format(dob, "%Y")) %>%
  group_by(year, immstat_bin) %>% #  immstat or immstat_bin
  summarise(numkids = n()) 

ggplot(vacc_counts, aes(fill=fct_rev(immstat_bin), y=numkids, x=year)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_vline(xintercept = "2009")+ # centered on year, need to be shifted to beginning of bar
  #geom_vline(xintercept = "2007")+ # centered on year, need to be shifted to beginning of bar
  #scale_fill_viridis(discrete = T)+
  #scale_fill_brewer(palette = "PuOr") +
  #scale_fill_brewer(palette = "BrBG") +
  #scale_color_manual(values = wes_palette(n=4, name="GrandBudapest1")) #GrandBudapest1
  #scale_fill_manual(values = c("#543005","#F6E8C3", "#BF812D","#01665E"))+
  #scale_fill_manual(values = c("#8C510A","#F6E8C3", "#DFC27D","#35978F"))+
  #scale_fill_manual(values = c("#8C510A","#D8DAEB", "#8073AC","#35978F"))+
  scale_fill_manual(values = c("gray","#D8DAEB", "#8073AC","#35978F"))+
  labs(x="Year of birth", y="Number of participants")+
  theme(panel.background = element_blank(),
        legend.title = element_blank())

ggsave("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/ab_results.png", width = 12, height = 6)

# separated by direct offspring or not
vacc_counts <- 
  fogsero_kids %>% 
  mutate(month = format(dob, "%m"), year = format(dob, "%Y")) %>%
  group_by(year, diroff, immstat_bin) %>% #  immstat or immstat_bin (grayzone levels); diroff to facet by diroff or not
  summarise(numkids = n()) 

ggplot(vacc_counts, aes(fill=fct_rev(immstat_bin), y=numkids, x=year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c("gray","#D8DAEB", "#8073AC","#35978F"))+
  labs(x="Year of birth", y="Number of participants")+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~factor(fct_rev(diroff)), nrow=2)
ggsave("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/ab_results_do.png", width = 9, height = 6)



library("RColorBrewer")
brewer.pal(12, "BrBG")
hexbrbg <- c("#543005" "#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#F5F5F5" "#C7EAE5" "#80CDC1" "#35978F" "#01665E" "#003C30")

brewer.pal(12, "PuOr")
hexPuOr <- c("#7F3B08" "#B35806" "#E08214" "#FDB863" "#FEE0B6" "#F7F7F7" "#D8DAEB" "#B2ABD2" "#8073AC" "#542788" "#2D004B")

# VL figure for mothers---------------------------------------------------------------------------------
vls <- fogsero %>% filter(!(is.na(avert_delivery_vl_iuml))) %>% select(c("PID_clean","avert_enr_date", "avert_delivery_sampdate", "avert_6mo_date", "fog_sampdate","avert_enr_vl_log","avert_delivery_vl_log","avert_6mo_vl_log" ,"fog_log_vl_num"))
view(vls)
# names(fogsero)<-gsub("_","",names(fogsero))
view(fogsero)
colnames(fogsero)

long_df <- vls %>%
  gather(DateVariable, Date, ends_with("Date")) %>%
  gather(ValueVariable, Value, contains("_log"))
view(long_df)

df2 <- rbind(long_df %>% filter(if_all(c("DateVariable","ValueVariable"), ~ str_detect(., "enr"))),
      long_df %>% filter(if_all(c("DateVariable","ValueVariable"), ~ str_detect(., "delivery"))),
      long_df %>% filter(if_all(c("DateVariable","ValueVariable"), ~ str_detect(., "6mo"))),
      long_df %>% filter(if_all(c("DateVariable","ValueVariable"), ~ str_detect(., "fog")))
      )

view(df2)

df2 %>% filter(!is.na(Value)) %>% 
  ggplot()+
  geom_point(aes(group=PID_clean ,x=Date, y=Value, color=PID_clean))+
  geom_line(aes(group = PID_clean,x=Date, y=Value,color=PID_clean))+
  scale_color_brewer(palette="Paired")+
  geom_hline(yintercept = 5.3)+
  #annotate("text", x=max(df2$Date), y=5.5, label="PMTCT TDF treatment")+
  geom_hline(yintercept = 0.1)+
  ylab("Log HBV viral load (IU/mL")+
  xlab("Sample date")+
  ggtitle("Viral loads of index mothers across available timepoints")+
  theme(panel.background = element_blank())
ggsave("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/mother_vl_time.png", width = 12, height = 6)

# HBsAb reactivity for kids with longitudinal sample----------
# use rdt_0_date_approx as astmh_12mo_hbsab timepoint (approximate - this is 1 year after DOB)
surfab_astmh <- fogsero %>% filter(!is.na(astmh_12mo_hbsab)) %>% rename("sampdate" = "rdt_0_date_approx", "hbsab"="astmh_12mo_hbsab") %>% select(c("PID_clean","dob","sampdate","rel_to_indexmoth","birthdose","hbsab"))
surfab_fog_astmhnomiss <- fogsero  %>% filter(!is.na(astmh_12mo_hbsab)) %>% rename("sampdate" = "fog_sampdate", "hbsab"="hbsab_mIUmL") %>% select(c("PID_clean","dob","sampdate","rel_to_indexmoth","birthdose","hbsab"))

surfab <- rbind(surfab_fog_astmhnomiss,surfab_astmh )
surfab$hbsab <- as.numeric(surfab$hbsab)
class(surfab$sampdate)

surfab %>% 
  ggplot()+
  geom_point(aes(group=PID_clean ,x=sampdate, y=hbsab, color=PID_clean))+
  geom_line(aes(group = PID_clean,x=sampdate, y=hbsab,color=PID_clean))+
  scale_color_brewer(palette="Paired")+
  geom_hline(yintercept = 10)+ # limit of detection
  ylab("anti-HBs (mIU/mL)")+
  xlab("Sample date")+
  ggtitle("Anti-HBs of ASTMH offspring across available timepoints")+
  theme(panel.background = element_blank())+
  facet_wrap(~birthdose)
ggsave("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/ab_results_scattertime.png", width = 12, height = 6)


# everyone with sab



## sero venn diagrams (early draft)----------
library(eulerr)
install.packages("devtools")
devtools::install_github("jolars/eulerr")

VennDiag <- euler(c("A" = 35, "B" = 28, "C" = 35, "A&B" = 23, "B&C" = 26, 
                    "A&C" = 25, "A&B&C" = 23), shape = "ellipse")
plot(VennDiag, counts = TRUE, font=1, cex=1, alpha=0.5) #fill=c("grey", "lightgrey", "darkgrey")


library(VennDiagram)
grid.newpage()
draw.triple.venn(area1 = 35, area2 = 28, area3 = 35, n12 = 23, n23 = 26, n13 = 25, 
                 n123 = 23, category = c("test rapide", "charge viral detectable", ""), lty = "blank", 
                 fill = c("blue", "yellow", "red"),
                 fontfamily = "Arial" )

# other counts
table(fogsero$hbeag_interp, fogsero$rdt_simple)

# HIV status
table(fogarty_hoverdata$i3_hiv_pos_test_f, useNA = "always")
table(fogarty_hoverdata$i3a_hiv_treatment_f, useNA = "always")
table(fogarty_hoverdata$i3b_hiv_medications, useNA = "always")

fogarty_hoverdata %>% filter(i3_hiv_pos_test==1) %>% reframe(pid,hr3_relationship_f,paststudymutexcl)
fogarty_hoverdata %>% filter(hr3_relationship_f=="Index mother") %>% reframe(pid,i3_hiv_pos_test,paststudymutexcl,i3a_hiv_treatment_f,i3b_hiv_medications)

fogarty_hoverdata %>% filter(paststudymutexcl=="astmh only" |paststudymutexcl=="avert/astmh") %>% count(hrhhid,paststudymutexcl)
fogarty_hoverdata %>% filter(paststudymutexcl=="avert/astmh") %>% count(hrhhid)

table(fogarty_hoverdata$paststudymutexcl)

