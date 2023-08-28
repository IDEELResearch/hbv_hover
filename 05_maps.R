# make specialised maps
library(sf)
library(raster)
library(tidyverse)
library(ggspatial)
library(gstat)
library(stars)
library(patchwork)
library(ggsn)

# Maps--------------------------------------------------------------------------------

# merge gps onto individual survey
inddata1 <- merge(inddata1, hhdata1[,c("hrhhid","hycoord_edit","hxcoord_edit")], by = "hrhhid")
# make spatial object
indgps_2 = st_as_sf(inddata1[!is.na(inddata1$hxcoord_edit) &!is.na(inddata1$hycoord_edit),], coords = c("hycoord_edit", "hxcoord_edit"), crs = 4326)  
# order by hbsag result
indgps_2 <- indgps_2[order(indgps_2$i27a_rdt_result_f),]
# jitter points
indgps_2_jitt<- st_jitter(indgps_2,  factor = 0.005)

hover_gps_full_jitt <-st_jitter(hover_gps_full,  factor = 0.01)

# surrounding polygons
drc_healthzone_correctkinshasa = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/drc_healthzone_adm2_correctkinshasa/RDC_Zone_de_sante_09092019.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# keep only Kin prov
drc_healthzone_kinshasa <- subset(drc_healthzone_correctkinshasa, PROVINCE == "Kinshasa")

# brazzaville polygon
congo_br = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/congo_adm0/cog_admbnda_adm0_gaul_20190617.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# rename brazza labels
congo_br$ADM0_FR <- as.character(congo_br$ADM0_FR) 
congo_br$ADM0_FR[congo_br$ADM0_FR == "Congo (le)"] <- "Congo"
congo_br$ADM0_FR[congo_br$ADM0_FR == "Congo"] <- "Brazzaville"

# health areas
drc_healtharea = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/rdc_aires-de-sante/RDC_Aires de sant‚.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# restrict to Kin
drc_healtharea_Kin <- subset(drc_healtharea, Province == "Kinshasa")

#provinces
drcprov = st_read("./adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# restrict to Kin and prov below kin (but prov shp covers river)
drcprov_Kin <- subset(drcprov, ADM1_NAME == "KINSHASA")
drcprov_KinKC <- subset(drcprov, ADM1_NAME == "KINSHASA" | ADM1_NAME == "KONGO CENTRAL" )

##rivers - covered by adm0 and adm1 
# rivers = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/congo_rivers_simp/congo_rivers_simp.shp", stringsAsFactors = F) %>% st_transform(4326)

# read in data
admin0 <- readRDS('./admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 

st_crs(admin0) # view CRS

DRC <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC

## Africa with DRC highlighted and Kinshasa in red box
africa <- st_read("./afr_simp/afr_g2014_2013_0.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
#A <- 
ggplot(africa) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=DRC, fill = "gray75", size=0.2)+
  geom_rect(aes(xmin = 15.2, xmax = 15.6, ymin = -4.48, ymax = -4.07),
            fill = "transparent", color = "red", linewidth = 2.5)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())

ggsave('./plots/afr_drc.png', width=9, height=9)


## Base map----------
base <- ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf_text(data=congo_br, aes(label = ADM0_FR), size=3)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.15,0.9))

## Map: HH prev--------------------
base +
  geom_sf(data=hover_gps_full, aes(color=hhprev))+
  scale_color_binned(type="viridis", breaks = c(0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9))+
  #scale_color_continuous(type = "RdYlBu")+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  guides(fill=guide_legend(title="Household prevalence (%)"))

## Map: Exp vs Unexp------------------
hover_gps_full <- hover_gps_full %>% 
  dplyr::mutate(h10_hbv_rdt_f=factor(
    hover_gps_full$h10_hbv_rdt, 
    levels = c(0, 1),
    labels = c("Unexposed", "Exposed")))
hover_gps_full$cpn_maternity
#B <-  # households colored by exposed/unexposed
ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2,aes(label = ADM0_FR))+
  geom_sf(data=hover_gps_full, aes(fill=h10_hbv_rdt_f, color=h10_hbv_rdt_f), size=3)+ #or 3 for standalone
  geom_sf(data=mat_gps_sf, color = "gray39", fill="#fee090", shape = 23, size=5)+
  #scale_fill_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  scale_color_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  geom_sf_text(data=congo_br, aes(label = ADM0_FR), size=1, hjust = 2, vjust = 44)+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  #coord_sf(xlim = c(14.1, 15.6), ylim = c(-7.1, -3.1), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.12,0.92)#,
        #legend.text=element_text(size=20)
  )+
  guides(color = guide_legend(override.aes = list(size = 2)))
#annotation_scale(location = "br", plot_unit = "mi")

## color by maternity-----------------
acqhh <- hover_gps_full %>% filter(cpn_maternity != "Binza" & cpn_maternity != "Kingasani")
acqmat <- mat_gps_sf %>% filter(cpn_maternity != "Binza" & cpn_maternity != "Kingasani")
view()
library(ggnewscale)

ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+ #,aes(label = ADM0_FR)
  geom_sf(data=st_jitter(hover_gps_full,  factor = 0.03), aes(fill=cpn_maternity, color=cpn_maternity), size=3)+ #or 3 for standalone
  #scale_fill_brewer(palette = "Paired")+ # the reds and oranges in paired overlap with red/orange of exposed/unex
  #scale_color_brewer(palette = "Paired")+
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  scale_color_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  new_scale("fill") +
  geom_sf(data=mat_gps_sf, aes(fill=cpn_maternity), shape = 23, size=5)+ #color = "gray39", fill="#fee090",
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  #coord_sf(xlim = c(14.1, 15.6), ylim = c(-7.1, -3.1), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.12,0.8)#,
        #legend.text=element_text(size=20)
  )#+guides(color = guide_legend(override.aes = list(size = 4)))
#annotation_scale(location = "br", plot_unit = "mi")

ggsave('./plots/hh_by_mat.png', width=9, height=9)

# JUST MATERNITIES
ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+ #,aes(label = ADM0_FR)
  geom_sf(data=mat_gps_sf, aes(fill=cpn_maternity, size = binzking), shape = 23)+ # , size=5,color = "gray39", fill="#fee090",
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  #coord_sf(xlim = c(14.1, 15.6), ylim = c(-7.1, -3.1), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.12,0.8)#,
        #legend.text=element_text(size=20)
  )#+guides(color = guide_legend(override.aes = list(size = 4)))
#annotation_scale(location = "br", plot_unit = "mi")

# JUST HOUSEHOLDS BY RECRUITMENT MAT
ggplot(drc_healtharea_Kin) +
  geom_sf(alpha=0.75, size = 0.1)+
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+ #,aes(label = ADM0_FR)
  geom_sf(data=st_jitter(hover_gps_full,  factor = 0.03), aes(fill=cpn_maternity, color=cpn_maternity), size=3)+ #or 3 for standalone
  #scale_fill_brewer(palette = "Paired")+ # the reds and oranges in paired overlap with red/orange of exposed/unex
  #scale_color_brewer(palette = "Paired")+
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  scale_color_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  #coord_sf(xlim = c(14.1, 15.6), ylim = c(-7.1, -3.1), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.12,0.8)#,
        #legend.text=element_text(size=20)
  )#+guides(color = guide_legend(override.aes = list(size = 4)))
#annotation_scale(location = "br", plot_unit = "mi")


## Map: Hbsag results--------------------
#C <- 
  base + 
  geom_sf(data=indgps_2_jitt[!is.na(indgps_2_jitt$i27a_rdt_result_f),], aes(fill=i27a_rdt_result_f, color=i27a_rdt_result_f), size = 0.5)+
  scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.1,0.92))+
  guides(color = guide_legend(override.aes = list(size = 2)))



##Map: Traditional scarring---------------------------------------------------------------
indgps_2_jitt <- indgps_2_jitt %>% 
  dplyr::mutate(i16_traditional_scarring_f_map=factor(
    indgps_2_jitt$i16_traditional_scarring, 
    levels = c(0, 1, 99),
    labels = c("No traditional scars", "Has traditional scars", "Refused")))
#D <-
  base + 
  geom_sf(data=indgps_2_jitt, aes(fill=i16_traditional_scarring_f_map, color=i16_traditional_scarring_f_map), size=0.5)+
  #scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  #scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#665191',"#F5B24E","#A87323"))+   # purple and yellow and brown (refuse)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.2,0.9))+
  guides(color = guide_legend(override.aes = list(size = 2)))
D

# past transfusions
indgps_2_jitt <- indgps_2_jitt %>% 
  dplyr::mutate(i8_transfusion_f_map=factor(
    indgps_2_jitt$i8_transfusion, 
    levels = c(0, 1, 99),
    labels = c("No transfusions", "Received transfusions", "Refused")))
#E <-
base + 
  geom_sf(data=indgps_2_jitt[!is.na(indgps_2_jitt$i8_transfusion_f_map),], aes(fill=i8_transfusion_f_map, color=i8_transfusion_f_map), size=0.5)+
  #scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  #scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#665191',"#F5B24E","#A87323"))+   # purple and yellow and brown (refuse)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.213,0.9))+
  guides(color = guide_legend(override.aes = list(size = 2)))
E
#money exchanged for sex
indgps_2_jitt <- indgps_2_jitt %>% 
  dplyr::mutate(i26_sex_hx_given_money_f_map=factor(
    indgps_2_jitt$i26_sex_hx_given_money, 
    levels = c(0, 1, 99),
    labels = c("Never given money for sex", "Given money for sex", "Refused")))
#F <-
  base + 
  geom_sf(data=indgps_2_jitt[!is.na(indgps_2_jitt$i26_sex_hx_given_money_f_map),], aes(fill=i26_sex_hx_given_money_f_map, color=i26_sex_hx_given_money_f_map), size=0.5)+
  #scale_fill_manual(values = c('#878787','#f4a582','ghostwhite'))+
  #scale_color_manual(values = c('#878787','#f4a582','ghostwhite'))+
  scale_color_manual(values = c('#665191',"#F5B24E","#A87323"))+   # purple and yellow and brown (refuse)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(legend.position = c(0.245,0.9))+
  guides(color = guide_legend(override.aes = list(size = 2)))
F
# piece plots together using library(patchwork)
#A + B + C + D + E + F + plot_layout(nrow=2, ncol = 3) + plot_annotation(tag_levels = 'A')

#A + grid + plot_layout(nrow=1, ncol = 2) + plot_annotation(tag_levels = 'A')

# output
ggsave('./plots/croiabs.png', width=15, height=9)

## Kriging-------------
# kriging using gstat: https://rpubs.com/nabilabd/118172 
# https://mgimond.github.io/Spatial/interpolation-in-r.html#generate-the-variance-and-confidence-interval-maps


output <- indgps_2_jitt %>% 
  #group_by(hrhhid) %>%
  # make variable for hh prev
  mutate(hhprev = (totalpositive/n)*100)

output <- hover_gps_full
# remove points where geometry is outside of DRC outline (geometry=c(0,0))
output_points <- st_join(output, drcprov_Kin, join = st_intersects)
#%>% filter(!is.na(Country)) # from Hill's code

# make variogram
m.vgm <- gstat::variogram(hhprev~1, output_points)

# fit a model to the sample variogram
# https://gisgeography.com/semi-variogram-nugget-range-sill/
m.fit <- gstat::fit.variogram(m.vgm, model=vgm(psill=1,"Exp",range=500, nugget=0))

library(automap)
variogram = autofitVariogram(hhprev~1,output_points)
plot(variogram)

# plot
plot(m.vgm,m.fit)

# simple kriging
spDRC <- as_Spatial(DRC)
grd <- makegrid(spDRC, n = 50000)# making grid of points
colnames(grd) <- c('x','y')
grd_pts <- SpatialPoints(coords = grd, 
                         proj4string=CRS(proj4string(spDRC)))

# find all points in `grd_pts` that fall within DRC outline
grd_pts_in <- grd_pts[spDRC, ]

# transform grd_pts_in back into a data frame
gdf <- as.data.frame(coordinates(grd_pts_in)) 

# conduct kriging: Pf prev
m.kriged <- gstat::krige(prev~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged$var1.pred)

# assign points into bins
krige <- m.kriged %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,80,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))

# conduct kriging: animal ownership
m.kriged.own <- gstat::krige(ownership~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.own$var1.pred)

# assign points into bins
krige_own <- m.kriged.own %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,90,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))



# Integrate Population density-----------------------------
drcpop = raster("COD_population_v2_0_gridded/COD_population_v2_0_gridded.tif")

# DRC health zone bounds
drc_healthzone_correctkinshasa = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/drc_healthzone_adm2_correctkinshasa/RDC_Zone_de_sante_09092019.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# keep only Kin prov
drc_healthzone_kinshasa <- subset(drc_healthzone_correctkinshasa, PROVINCE == "Kinshasa")

drcprov = st_read("./adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# restrict to Kin and prov below kin (but prov shp covers river)
drcprov_Kin <- subset(drcprov, ADM1_NAME == "KINSHASA")
drcprov_KinKC <- subset(drcprov, ADM1_NAME == "KINSHASA" | ADM1_NAME == "KONGO CENTRAL" )

plot(st_geometry(drcprov_Kin), add = TRUE)

# crop the lidar raster using the vector extent
drcpop_crop <- crop(drcpop, drcprov_Kin)
drcpop_crop2 <- mask(r2, drcprov_Kin)

plot(drcpop_crop, main = "Cropped lidar chm")

## crop and mask
r2 <- crop(drcpop_crop, extent(drc_healtharea_Kin))
r3 <- mask(r2, drc_healtharea_Kin)

# simple plot of Kin pop raster
ggplot() + 
  layer_spatial(r3, mapping = aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(option = "cividis",na.value = NA, name="Pop per 100m", direction = -1)+
  geom_sf(drc_healtharea_Kin, mapping = aes(alpha=0.75, size = 0.1)) + # province shapefile covers river, but health area does not (could also check health zone)
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Pop + hover mat and hhs
ggplot() + 
  layer_spatial(r3, mapping = aes(fill = after_stat(band1))) +
  #layer_spatial(drcpop_crop, mapping = aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(option = "magma",na.value = NA, name="Pop per 100m", direction = -1)+ #zero value "#fcea66"
  new_scale("fill") +
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf(drc_healtharea_Kin, mapping = aes(alpha=0.5, size = 0.1)) + # province shapefile covers river, but health area does not (could also check health zone)
  geom_sf(data=st_jitter(hover_gps_full,  factor = 0.03), aes(fill=cpn_maternity, color=cpn_maternity), size=3)+ #or 3 for standalone
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  scale_color_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  #new_scale("fill") +
  geom_sf(data=mat_gps_sf, aes(fill=cpn_maternity), shape = 23, size=5)+ #color = "gray39", fill="#fee090",
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('./plots/hhmatpop.png', width=9, height=9)
ggsave('./plots/hhmatpop.png', width=9, height=9, dpi = 300)
ggsave('./plots/hhmatpop.eps', width=9, height=9, dpi = 300) # eps doesn't like transparency

# locations of households in clusters
ggplot() + 
  layer_spatial(r3, mapping = aes(fill = after_stat(band1))) +
  #layer_spatial(drcpop_crop, mapping = aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(option = "magma",na.value = NA, name="Pop per 100m", direction = -1)+ #zero value "#fcea66"
  new_scale("fill") +
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf(drc_healtharea_Kin, mapping = aes(alpha=0.5, size = 0.1)) + # province shapefile covers river, but health area does not (could also check health zone)
  geom_sf(data=st_jitter(hover_gps_full[hover_gps_full$totalpositive>1,],  factor = 0.03), aes(fill=cpn_maternity, color=cpn_maternity), size=3)+ #or 3 for standalone
  scale_fill_manual(values = c("#a6cee3","#33a02c","#c51b7d"))+
  scale_color_manual(values = c("#a6cee3","#33a02c","#c51b7d"))+
  #new_scale("fill") +
  #geom_sf(data=mat_gps_sf, aes(fill=cpn_maternity), shape = 23, size=5)+ #color = "gray39", fill="#fee090",
  #scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('./plots/hhmatpop_clust.png', width=9, height=9, dpi = 300)

## pop map with exp/unexp------
ggplot() + 
  layer_spatial(r3, mapping = aes(fill = after_stat(band1))) +
  #layer_spatial(drcpop_crop, mapping = aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(option = "magma",na.value = NA, name="Pop per 100m", direction = -1)+ #zero value "#fcea66"
  new_scale("fill") +
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf(drc_healtharea_Kin, mapping = aes(alpha=0.5, size = 0.1)) + # province shapefile covers river, but health area does not (could also check health zone)
  geom_sf(data=st_jitter(hover_gps_full,  factor = 0.02), aes(fill=h10_hbv_rdt_f, color=h10_hbv_rdt_f), size=3)+ #or 3 for standalone
  scale_color_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  #new_scale("fill") +
  geom_sf(data=mat_gps_sf, color = "gray39", fill="#fee090", shape = 23, aes(size= binzking*2))+ # size=5, colors: # , #F5B24E
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('./plots/hhexp_pop.png', width=9, height=9, dpi = 300)

# clusters by exposed/unexposed
ggplot() + 
  layer_spatial(r3, mapping = aes(fill = after_stat(band1))) +
  #layer_spatial(drcpop_crop, mapping = aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(option = "magma",na.value = NA, name="Pop per 100m", direction = -1)+ #zero value "#fcea66"
  new_scale("fill") +
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf(drc_healtharea_Kin, mapping = aes(alpha=0.5, size = 0.1)) + # province shapefile covers river, but health area does not (could also check health zone)
  geom_sf(data=st_jitter(hover_gps_full[hover_gps_full$totalpositive>1,],  factor = 0.03), aes(fill=h10_hbv_rdt_f, color=h10_hbv_rdt_f), size=3)+ #or 3 for standalone
  scale_color_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  #new_scale("fill") +
  #geom_sf(data=mat_gps_sf, aes(fill=cpn_maternity), shape = 23, size=5)+ #color = "gray39", fill="#fee090",
  #scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#f1b6da","#c51b7d","#80cdc1" ,"#01665e","#cab2d6","#6a3d9a","#ffff99"))+
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('./plots/hhexppop_clust.png', width=9, height=9, dpi = 300)



# Make Pop chloropleth map-------
# from https://milospopovic.net/6-ways-to-map-population-with-r.r/
#1. make polygons
get_polygon <- function() {
  # st_area returns square meters so we get square km by dividing the result by 1000
  df$area_sqkm <- as.numeric(sf::st_area(df) / 1000000)
  
  deu_polygon <- df |>
    dplyr::mutate(pop_per_sqr_km = values / area_sqkm)
  
  return(deu_polygon)
}

deu_polygon <- get_polygon()

# min/max values
vmin <- min(deu_polygon$pop_per_sqr_km, na.rm = T)
vmax <- max(deu_polygon$pop_per_sqr_km, na.rm = T)

# bins
brk <- round(classIntervals(deu_polygon$pop_per_sqr_km,
                            n = 6,
                            style = "equal"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)

# breaks
breaks <- c(vmin, brk)

make_polygon_map <- function() {
  p1 <-
    ggplot(deu_polygon) +
    geom_sf(aes(fill = pop_per_sqr_km),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p1)
}

map1 <- make_polygon_map()

# another example: https://gis.stackexchange.com/questions/332109/extract-population-density-data-from-a-raster-by-shapefile

# Fogarty map------------------

fogpids <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/FOGARTY DATASET.xlsx", sheet = "fogpids")

hover_gps_full_jitt <-st_jitter(hover_gps_full,  factor = 0.01)

foggps_jitt <- hover_gps_full_jitt %>% filter(hrhhid %in% fogpids$hrhhid)
# check not matching
# fogmapmiss <- fogpids %>% filter(!(hrhhid %in% foggps_jitt$hrhhid))
ggplot() + 
  layer_spatial(r3, mapping = aes(fill = after_stat(band1))) +
  #layer_spatial(drcpop_crop, mapping = aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(option = "magma",na.value = NA, name="Pop per 100m", direction = -1)+ #zero value "#fcea66"
  new_scale("fill") +
  geom_sf(data=congo_br, fill = "gray75", size=0.2)+
  geom_sf(drc_healtharea_Kin, mapping = aes(alpha=0.5, size = 0.1)) + # province shapefile covers river, but health area does not (could also check health zone)
  geom_sf(data=foggps_jitt, aes(fill=h10_hbv_rdt_f, color=h10_hbv_rdt_f), size=3)+ #or 3 for standalone
  scale_color_manual(values = c("#4D4D4D","#B2182B",'ghostwhite'))+
  #new_scale("fill") +
  geom_sf(data=mat_gps_sf, color = "gray39", fill="#fee090", shape = 23, aes(size= binzking*2))+ # size=5, colors: # , #F5B24E
  coord_sf(xlim = c(15.2, 15.6), ylim = c(-4.48, -4.07), expand = FALSE)+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('./plots/hhexp_pop.png', width=9, height=9, dpi = 300)
