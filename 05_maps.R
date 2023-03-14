# make specialised maps

library(sf)
library(raster)
library(tidyverse)
library(ggspatial)

drcpop = raster("COD_population_v2_0_gridded/COD_population_v2_0_gridded.tif")

# DRC health zone bounds
drc_healthzone_correctkinshasa = st_read("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/sanru/Mapping/NEW files/drc_healthzone_adm2_correctkinshasa/RDC_Zone_de_sante_09092019.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# keep only Kin prov
drc_healthzone_kinshasa <- subset(drc_healthzone_correctkinshasa, PROVINCE == "Kinshasa")

drcprov = st_read("./adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% st_transform(4326)
# restrict to Kin and prov below kin (but prov shp covers river)
drcprov_Kin <- subset(drcprov, ADM1_NAME == "KINSHASA")
drcprov_KinKC <- subset(drcprov, ADM1_NAME == "KINSHASA" | ADM1_NAME == "KONGO CENTRAL" )



plot(drcpop, axes = FALSE)
plot(st_geometry(drcprov_Kin), add = TRUE)

# crop the lidar raster using the vector extent
drcpop_crop <- crop(drcpop, drcprov_Kin)
plot(drcpop_crop, main = "Cropped lidar chm")

ggplot() + 
  layer_spatial(drcpop_crop, aes(fill = after_stat(band1))) +
  scale_fill_viridis_c(na.value = NA, name="Pop per 100m")+
  
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())




